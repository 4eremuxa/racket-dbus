#lang racket/base
;
; D-Bus Connection Infrastructure
;


(require "ffi.rkt"
         "common.rkt"
         "message.rkt")

(require racket/contract
         racket/function
         racket/list
         racket/set
         racket/match)

(provide (all-defined-out))


(define-struct/contract dbus-connection
  ((dbc        DBusConnection-pointer?)
   (thread     (or/c thread? #f))
   (ports      (hash/c DBusWatch-pointer? (cons/c input-port? output-port?)))
   (functions  list?)
   (signals    (hash/c (list/c string? string? string? string?)
                       (set/c any/c))))
  #:mutable)


(define/contract (make-add-watch-function c)
                 (-> dbus-connection?
                     (-> DBusWatch-pointer? any/c boolean?))
  (lambda (watch data)
    (let ((socket (dbus_watch_get_socket watch)))
      (let-values (((in out) (scheme_socket_to_ports
                               socket (format "socket(~a)" socket) 0)))
        (hash-set! (dbus-connection-ports c) watch (cons in out))
        (when (dbus_watch_get_enabled watch)
          (when (member 'readable (dbus_watch_get_flags watch))
            (thread-send (dbus-connection-thread c) `(watch ,in ,watch)))
          (when (member 'writable (dbus_watch_get_flags watch))
            (thread-send (dbus-connection-thread c) `(watch ,out ,watch))))))
    #t))


(define/contract (make-remove-watch-function c)
                 (-> dbus-connection?
                     (-> DBusWatch-pointer? any/c void?))
  (lambda (watch data)
    (let ((inout (hash-ref (dbus-connection-ports c) watch)))
      (hash-remove! (dbus-connection-ports c) watch)
      (thread-send (dbus-connection-thread c) `(unwatch ,(car inout)))
      (thread-send (dbus-connection-thread c) `(unwatch ,(cdr inout))))))


(define/contract (make-watch-toggled-function c)
                 (-> dbus-connection?
                     (-> DBusWatch-pointer? any/c void?))
  (lambda (watch data)
    (let* ((inout  (hash-ref (dbus-connection-ports c) watch)))
      (thread-send (dbus-connection-thread c) `(unwatch ,(car inout)))
      (thread-send (dbus-connection-thread c) `(unwatch ,(cdr inout)))
      (when (dbus_watch_get_enabled watch)
        (when (member 'readable (dbus_watch_get_flags watch))
          (thread-send (dbus-connection-thread c)
                       `(watch ,(car inout) ,watch)))
        (when (member 'writable (dbus_watch_get_flags watch))
          (thread-send (dbus-connection-thread c)
                       `(watch ,(cdr inout) ,watch)))))))


(define/contract (make-add-timeout-function c)
                 (-> dbus-connection?
                     (-> DBusTimeout-pointer? any/c boolean?))
  (lambda (timeout data)
    (when (dbus_timeout_get_enabled timeout)
      (thread-send (dbus-connection-thread c) `(add-timeout ,timeout)))
    #t))


(define/contract (make-remove-timeout-function c)
                 (-> dbus-connection?
                     (-> DBusTimeout-pointer? any/c void?))
  (lambda (timeout data)
    (thread-send (dbus-connection-thread c) `(remove-timeout ,timeout))))


(define/contract (make-timeout-toggle-function c)
                 (-> dbus-connection?
                     (-> DBusTimeout-pointer? any/c void?))
  (lambda (timeout data)
    (if (dbus_timeout_get_enabled timeout)
      (thread-send (dbus-connection-thread c) `(add-timeout ,timeout))
      (thread-send (dbus-connection-thread c) `(remove-timeout ,timeout)))))


(define/contract (make-filter-function c)
                 (-> dbus-connection?
                     (-> DBusConnection-pointer?
                         DBusMessage-pointer?
                         any/c
                         (one-of/c 'handled 'not-yet-handled)))
  (lambda (dbc msg data)
    (if (eq? (dbus_message_get_type msg) 'signal)
      (let ((sender (dbus_message_get_sender msg))
            (path   (dbus_message_get_path msg))
            (iface  (dbus_message_get_interface msg))
            (signal (dbus_message_get_member msg)))
        (let ((key (list sender path iface signal)))
          (when (hash-has-key? (dbus-connection-signals c) key)
            (let ((handlers (hash-ref (dbus-connection-signals c) key))
                  (args (dbus-unpack-raw msg)))
              (set-for-each
                (lambda (handler)
                  (thread
                    (thunk (apply handler sender path iface signal args))))
                handlers))))
        'handled)
      'not-yet-handled)))


(define/contract (make-background-thread c)
                 (-> dbus-connection? thread?)
  (thread
    (thunk
      (define events (make-hasheq
                       (list (cons (thread-receive-evt)
                                   (thread-receive-evt)))))

      (define timeouts (make-hasheq))

      (define dbc (dbus-connection-dbc c))

      (let loop ()
        (let ((event (apply sync (thread-receive-evt) (hash-keys events))))
          (cond
            ((port? event)
             (dbus_watch_handle (hash-ref events event)
                                (if (input-port? event)
                                  'readable
                                  'writable)))

            ((DBusTimeout-pointer? (hash-ref events event))
             (let ((timeout (hash-ref events event)))
               (hash-remove! events event)
               (hash-remove! timeouts timeout)

               (dbus_timeout_handle timeout)

               (when (dbus_timeout_get_enabled timeout)
                 (let ((event (alarm-evt
                                (+ (current-inexact-milliseconds)
                                   (dbus_timeout_get_interval timeout)))))
                   (hash-set! timeouts timeout event)
                   (hash-set! events event timeout)))))

            (else
             (let ((message (thread-receive)))
               (match message
                 ((list 'watch (var port) (var watch))
                  (hash-set! events port watch))

                 ((list 'unwatch (var port))
                  (hash-remove! events port))

                 ((list 'add-timeout (var timeout))
                  (let ((event (alarm-evt
                                 (+ (current-inexact-milliseconds)
                                    (dbus_timeout_get_interval timeout)))))
                    (hash-set! events event timeout)
                    (hash-set! timeouts timeout event)))

                 ((list 'remove-timeout (var timeout))
                  (when (hash-has-key? timeouts timeout)
                    (let ((event (hash-ref timeouts timeout)))
                      (hash-remove! events event)
                      (hash-remove! timeouts timeout))))

                 ((list 'exit)
                  (kill-thread (current-thread))))))))

        (let loop ()
          (when (eq? (dbus_connection_get_dispatch_status dbc) 'data-remains)
            (dbus_connection_dispatch dbc)
            (loop)))

        (loop)))))


(define/contract (dbus-open type)
                 (-> (one-of/c 'session 'system 'starter) dbus-connection?)
  (dbus_threads_init_default)

  (let-values (((dbc error) (dbus_bus_get type)))
    (when (dbus_error_is_set error)
      (raise (dbus-error->exception error)))

    (let ((c (make-dbus-connection dbc
                                   #f
                                   (make-hasheq)
                                   (list)
                                   (make-hash))))
      (set-dbus-connection-thread! c (make-background-thread c))

      (set-dbus-connection-functions! c
        (list (make-add-watch-function c)
              (make-remove-watch-function c)
              (make-watch-toggled-function c)
              (make-add-timeout-function c)
              (make-remove-timeout-function c)
              (make-timeout-toggle-function c)
              (make-filter-function c)))

      (dbus_connection_set_watch_functions
        (dbus-connection-dbc c)
        (first (dbus-connection-functions c))
        (second (dbus-connection-functions c))
        (third (dbus-connection-functions c)))

      (dbus_connection_set_timeout_functions
        (dbus-connection-dbc c)
        (fourth (dbus-connection-functions c))
        (fifth (dbus-connection-functions c))
        (sixth (dbus-connection-functions c)))

      (dbus_connection_add_filter
        (dbus-connection-dbc c)
        (seventh (dbus-connection-functions c)))
      c)))


(define/contract (dbus-send-raw bus message)
                 (-> dbus-connection?
                     DBusMessage-pointer?
                     integer?)
  (dbus_connection_send (dbus-connection-dbc bus) message))


(define/contract (dbus-call-raw bus message)
                 (-> dbus-connection?
                     DBusMessage-pointer?
                     DBusMessage-pointer?)
  (let ((pc (dbus_connection_send_with_reply
              (dbus-connection-dbc bus) message -1))
        (sm (make-semaphore)))
    (dbus_pending_call_set_notify pc (lambda (pc data)
                                       (semaphore-post sm)))
    (semaphore-wait sm)
    (let ((msg (dbus_pending_call_steal_reply pc)))
      (when (eq? (dbus_message_get_type msg) 'error)
        (raise (dbus-error->exception (dbus_set_error_from_message msg))))
      msg)))


(define/contract (dbus-subscribe bus sender path iface signal handler)
                 (-> dbus-connection? string? string? string? string? any/c
                     void?)
  (let ((signals (dbus-connection-signals bus))
        (key     (list sender path iface signal)))
    (if (hash-has-key? signals key)
      (hash-set! signals key (set-add (hash-ref signals key) handler))
      (begin
        (hash-set! signals key (set handler))
        (dbus_bus_add_match
          (dbus-connection-dbc bus)
            (string-append "type='signal',"
                           "sender='" sender "',"
                           "interface='" iface "',"
                           "member='" signal "',"
                           "path='" path "'"))))))


(define/contract (dbus-unsubscribe bus sender path iface signal handler)
                 (-> dbus-connection? string? string? string? string? any/c
                     void?)
  (let ((signals (dbus-connection-signals bus))
        (key     (list sender path iface signal)))
    (when (hash-has-key? signals key)
      (let ((items (set-remove (hash-ref signals key) handler)))
        (if (= 0 (set-count items))
          (begin
            (dbus_bus_remove_match
              (dbus-connection-dbc bus)
              (string-append "type='signal',"
                             "sender='" sender "',"
                             "interface='" iface "',"
                             "member='" signal "',"
                             "path='" path "'"))
            (hash-remove! signals key))
          (hash-set! signals key items))))))


; vim:set ts=2 sw=2 et:
