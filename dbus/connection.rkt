#lang racket/base
;
; D-Bus Connection Infrastructure
;


(require "ffi.rkt"
         "common.rkt"
         "message.rkt")

(require (rename-in ffi/unsafe (-> -->))
         racket/contract
         racket/function
         racket/list
         racket/set
         racket/match)

(provide (all-defined-out))


(define (get-dbc ptr)
  (dbus-connection-dbc (weak-box-value (ptr-ref ptr _scheme))))

(define (get-thread ptr)
  (dbus-connection-thread (weak-box-value (ptr-ref ptr _scheme))))

(define (get-signals ptr)
  (dbus-connection-signals (weak-box-value (ptr-ref ptr _scheme))))


(define-struct/contract dbus-connection
  ((dbc        DBusConnection-pointer?)
   (thread     (or/c thread? #f))
   (signals    (hash/c (list/c string? string? string? string?)
                       (set/c any/c)))
   (cell       cpointer?))
  #:mutable)


(define/contract (add-watch-function watch c)
                 (-> DBusWatch-pointer? cpointer? boolean?)
  (thread-send (get-thread c) `(add-watch ,watch))
  #t)


(define/contract (remove-watch-function watch c)
                 (-> DBusWatch-pointer? cpointer? void?)
  (thread-send (get-thread c) `(remove-watch ,watch)))


(define/contract (watch-toggled-function watch c)
                 (-> DBusWatch-pointer? cpointer? void?)
  (thread-send (get-thread c) `(toggle-watch ,watch)))


(define/contract (add-timeout-function timeout c)
                 (-> DBusTimeout-pointer? cpointer? boolean?)
  (thread-send (get-thread c) `(add-timeout ,timeout))
  #t)


(define/contract (remove-timeout-function timeout c)
                 (-> DBusTimeout-pointer? cpointer? void?)
  (thread-send (get-thread c) `(remove-timeout ,timeout)))


(define/contract (timeout-toggle-function timeout c)
                 (-> DBusTimeout-pointer? cpointer? void?)
  (thread-send (get-thread c) `(toggle-timeout ,timeout)))


(define/contract (filter-function dbc msg c)
                 (-> DBusConnection-pointer? DBusMessage-pointer?
                     cpointer? (one-of/c 'handled 'not-yet-handled))
  (if (eq? (dbus_message_get_type msg) 'signal)
    (let ((sender (dbus_message_get_sender msg))
          (path   (dbus_message_get_path msg))
          (iface  (dbus_message_get_interface msg))
          (signal (dbus_message_get_member msg)))
      (let ((key (list sender path iface signal)))
        (when (hash-has-key? (get-signals c) key)
          (let ((handlers (hash-ref (get-signals c) key))
                (args (dbus-unpack-raw msg)))
            (set-for-each
              (lambda (handler)
                (thread
                  (thunk (apply handler sender path iface signal args))))
              handlers))))
      'handled)
    'not-yet-handled))


(define/contract (make-background-thread c)
                 (-> dbus-connection? thread?)
  (thread
    (thunk
      (define events (make-hasheq
                       (list (cons (thread-receive-evt)
                                   (thread-receive-evt)))))
      (define timeouts (make-hasheq))
      (define ports (make-hasheq))
      (define dbc (dbus-connection-dbc c))

      (let loop ()
        (let ((event (apply sync (thread-receive-evt) (hash-keys events))))
          (when (hash-has-key? events event)
            (let ((subject (hash-ref events event)))
              (cond
                ((DBusWatch-pointer? subject)
                 (dbus_watch_handle subject (if (input-port? event)
                                              'readable
                                              'writable)))

                ((DBusTimeout-pointer? subject)
                 (dbus_timeout_handle subject))
                 ;)

                (else
                 (let ((message (thread-receive)))
                   (match message
                     ((list 'add-watch (var watch))
                      (let ((socket (dbus_watch_get_socket watch)))
                        (let-values (((in out) (scheme_socket_to_ports
                                                 socket (format "socket(~a)"
                                                                socket) 0)))
                          (hash-set! ports watch (cons in out))
                          (when (dbus_watch_get_enabled watch)
                            (let ((flags (dbus_watch_get_flags watch)))
                              (when (member 'readable flags)
                                (hash-set! events in watch))
                              (when (member 'writable flags)
                                (hash-set! events out watch)))))))

                     ((list 'remove-unwatch (var watch))
                      (let ((inout (hash-ref ports watch)))
                        (hash-remove! ports watch)
                        (hash-remove! events (car inout))
                        (hash-remove! events (cdr inout))))

                     ((list 'toggle-watch (var watch))
                      (let* ((inout (hash-ref ports watch)))
                        (hash-remove! events (car inout))
                        (hash-remove! events (cdr inout))
                        (when (dbus_watch_get_enabled watch)
                          (let ((flags (dbus_watch_get_flags watch)))
                            (when (member 'readable flags)
                              (hash-set! events (car inout) watch))
                            (when (member 'writable flags)
                              (hash-set! events (cdr inout) watch))))))

                     ((list 'add-timeout (var timeout))
                      (when (dbus_timeout_get_enabled timeout)
                        (let ((event (alarm-evt
                                       (+ (current-inexact-milliseconds)
                                          (dbus_timeout_get_interval timeout)))))
                          (hash-set! events event timeout)
                          (hash-set! timeouts timeout event))))

                     ((list 'remove-timeout (var timeout))
                      (when (hash-has-key? timeouts timeout)
                        (let ((event (hash-ref timeouts timeout)))
                          (hash-remove! events event)
                          (hash-remove! timeouts timeout))))

                     ((list 'toggle-timeout (var timeout))
                      (when (hash-has-key? timeouts timeout)
                        (let ((event (hash-ref timeouts timeout)))
                          (hash-remove! events event))
                        (when (dbus_timeout_get_enabled timeout)
                          (let ((event (alarm-evt
                                         (+ (current-inexact-milliseconds)
                                            (dbus_timeout_get_interval timeout)))))
                            (hash-set! events event timeout)))))

                     ((list 'exit)
                      (kill-thread (current-thread))))))))))

        (let loop ()
          (when (eq? (dbus_connection_get_dispatch_status dbc) 'data-remains)
            (dbus_connection_dispatch dbc)
            (loop)))

        (loop)))))


(define/contract (dbus-open type)
                 (-> (one-of/c 'session 'system 'starter) dbus-connection?)
  (dbus_threads_init_default)

  (let-values (((dbc error) (dbus_bus_get_private type)))
    (when (dbus_error_is_set error)
      (raise (dbus-error->exception error)))

    (let ((c (make-dbus-connection dbc #f (make-hash) #f)))
      (set-dbus-connection-cell! c (malloc-immobile-cell (make-weak-box c)))
      (register-finalizer c (lambda (c)
                              (free-immobile-cell (dbus-connection-cell c))))

      (set-dbus-connection-thread! c (make-background-thread c))

      (dbus_connection_set_watch_functions
        (dbus-connection-dbc c)
        add-watch-function
        remove-watch-function
        watch-toggled-function
        (dbus-connection-cell c))

      (dbus_connection_set_timeout_functions
        (dbus-connection-dbc c)
        add-timeout-function
        remove-timeout-function
        timeout-toggle-function
        (dbus-connection-cell c))

      (dbus_connection_add_filter
        (dbus-connection-dbc c)
        filter-function
        (dbus-connection-cell c))
      c)))


(define/contract (dbus-send-raw bus message)
                 (-> dbus-connection?
                     DBusMessage-pointer?
                     integer?)
  (dbus_connection_send (dbus-connection-dbc bus) message))


(define/contract (pending-call-notify-function pending-call semaphore)
                 (-> DBusPendingCall-pointer? cpointer? void?)
  (semaphore-post (ptr-ref semaphore _scheme)))


(define/contract (dbus-call-raw bus message)
                 (-> dbus-connection?
                     DBusMessage-pointer?
                     DBusMessage-pointer?)
  (let* ((pending-call (dbus_connection_send_with_reply
                         (dbus-connection-dbc bus) message -1))
         (semaphore    (make-semaphore))
         (cell         (malloc-immobile-cell semaphore)))

    (dbus_pending_call_set_notify pending-call
                                  pending-call-notify-function
                                  cell)
    (semaphore-wait semaphore)
    (free-immobile-cell cell)

    (let ((message (dbus_pending_call_steal_reply pending-call)))
      (when (eq? (dbus_message_get_type message) 'error)
        (raise (dbus-error->exception (dbus_set_error_from_message message))))
      message)))


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
