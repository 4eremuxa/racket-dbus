#lang racket/base
;
; D-Bus Connection Infrastructure
;


(require "ffi.rkt"
         "common.rkt")

(require racket/contract
         racket/function
         racket/list
         racket/match)

(provide (all-defined-out))


(define-struct/contract dbus-connection
  ((dbc        DBusConnection-pointer?)
   (thread     (or/c thread? #f))
   (ports      (hash/c integer? (cons/c input-port? output-port?)))
   (functions  list?))
  #:transparent
  #:mutable)


(define/contract (make-add-watch-function c)
                 (-> dbus-connection?
                     (-> DBusWatch-pointer? any/c boolean?))
  (lambda (watch data)
    (let ((socket (dbus_watch_get_socket watch)))
      (let-values (((in out) (scheme_socket_to_ports
                               socket (format "socket(~a)" socket) 0)))
        (hash-set! (dbus-connection-ports c) socket (cons in out))
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
    (let* ((socket (dbus_watch_get_socket watch))
           (inout  (hash-ref (dbus-connection-ports c) socket)))
      (hash-remove! (dbus-connection-ports c) socket)
      (thread-send (dbus-connection-thread c) `(unwatch ,(car inout)))
      (thread-send (dbus-connection-thread c) `(unwatch ,(cdr inout))))))


(define/contract (make-watch-toggled-function c)
                 (-> dbus-connection?
                     (-> DBusWatch-pointer? any/c void?))
  (lambda (watch data)
    (let* ((socket (dbus_watch_get_socket watch))
           (inout  (hash-ref (dbus-connection-ports c) socket)))
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
                                   (list))))
      (set-dbus-connection-thread! c (make-background-thread c))

      (set-dbus-connection-functions! c
        (list (make-add-watch-function c)
              (make-remove-watch-function c)
              (make-watch-toggled-function c)
              (make-add-timeout-function c)
              (make-remove-timeout-function c)
              (make-timeout-toggle-function c)))

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


; vim:set ts=2 sw=2 et:
