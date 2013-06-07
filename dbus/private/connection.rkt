#lang racket/base
;
; D-Bus Connection Infrastructure
;


(require "ffi.rkt"
         "common.rkt"
         "message.rkt")

(require (rename-in ffi/unsafe (-> -->))
         racket/async-channel
         racket/contract
         racket/function
         racket/list
         racket/set
         racket/match
         main-loop)

(provide (all-defined-out))


(define watches (make-hash))
(define timeouts (make-hash))
(define signals (make-hash))


(define-struct/contract dbus-connection
  ((dbc DBusConnection-pointer?)))


(define/contract (add-watch-function watch c)
                 (-> DBusWatch-pointer? cpointer? boolean?)
  (let ((socket (dbus_watch_get_socket watch)))
    (let-values (((in out) (scheme_socket_to_ports socket
                             (format "socket(~a)" socket) 0)))
      (hash-set! watches watch (cons in out))
      (watch-toggled-function watch c)))
  #t)


(define/contract (remove-watch-function watch c)
                 (-> DBusWatch-pointer? cpointer? void?)
  (let ((inout (hash-ref watches watch #f)))
    (when inout
      (hash-remove! watches watch)
      (cancel-event (car inout))
      (cancel-event (cdr inout)))))


(define/contract (watch-toggled-function watch c)
                 (-> DBusWatch-pointer? cpointer? void?)
  (let* ((inout (hash-ref watches watch #f)))
    (when (and inout
               (dbus_watch_get_enabled watch))
      (let ((flags (dbus_watch_get_flags watch)))

        (if (member 'readable flags)
          (add-event-handler (car inout)
                             (thunk (dbus_watch_handle watch 'readable)))
          (cancel-event (car inout)))

        (if (member 'writable flags)
          (add-event-handler (cdr inout)
                             (thunk (dbus_watch_handle watch 'writable)))
          (cancel-event (cdr inout)))))))


(define/contract (add-timeout-function timeout c)
                 (-> DBusTimeout-pointer? cpointer? boolean?)
  (timeout-toggle-function timeout c)
  #t)


(define/contract (remove-timeout-function timeout c)
                 (-> DBusTimeout-pointer? cpointer? void?)
  (let ((event (hash-ref timeouts timeout #f)))
    (when event
      (hash-remove! timeouts timeout)
      (cancel-event event))))


(define/contract (timeout-toggle-function timeout c)
                 (-> DBusTimeout-pointer? cpointer? void?)
  (remove-timeout-function timeout c)
  (when (dbus_timeout_get_enabled timeout)
    (let ((event (alarm-evt (+ (current-inexact-milliseconds)
                               (dbus_timeout_get_interval timeout)))))
      (hash-set! timeouts timeout event)
      (add-event-handler event
        (thunk
          (dbus_timeout_handle timeout)
          (timeout-toggle-function timeout c))))))


(define/contract (dispatch-status-function dbc status c)
                 (-> DBusConnection-pointer? symbol? cpointer? void?)
  (when (eq? (dbus_connection_get_dispatch_status dbc) 'data-remains)
    (dbus_connection_dispatch dbc)
    (dispatch-status-function dbc status c)))


(define/contract (filter-function dbc msg c)
                 (-> DBusConnection-pointer? DBusMessage-pointer?
                     cpointer? (one-of/c 'handled 'not-yet-handled))
  (if (eq? (dbus_message_get_type msg) 'signal)
    (let ((path   (dbus_message_get_path msg))
          (iface  (dbus_message_get_interface msg))
          (signal (dbus_message_get_member msg)))
      (let ((key (list dbc path iface signal)))
        (let ((handlers (hash-ref signals key (set))))
          (call-with-values
            (thunk (dbus-unpack-raw msg))
            (lambda args
              (set-for-each
                handlers
                (lambda (handler)
                  (thread
                    (thunk (apply handler path iface signal args)))))))))
      'handled)
    'not-yet-handled))


(define/contract (dbus-open type)
                 (-> (one-of/c 'session 'system 'starter) dbus-connection?)
  (dbus_threads_init_default)

  (let-values (((dbc error) (dbus_bus_get_private type)))
    (when (dbus_error_is_set error)
      (raise (dbus-error->exception error)))

    (let ((connection (make-dbus-connection dbc)))
      (dbus_connection_add_filter dbc
        filter-function)

      (dbus_connection_set_dispatch_status_function dbc
        dispatch-status-function)

      (dbus_connection_set_watch_functions dbc
        add-watch-function
        remove-watch-function
        watch-toggled-function)

      (dbus_connection_set_timeout_functions dbc
        add-timeout-function
        remove-timeout-function
        timeout-toggle-function)

      (dispatch-status-function dbc 'initial-dispatch #f)
      connection)))


(define/contract (dbus-send-raw bus message)
                 (-> dbus-connection?
                     DBusMessage-pointer?
                     integer?)
  (dbus_connection_send (dbus-connection-dbc bus) message))


(define/contract (dbus-call-raw bus message)
                 (-> dbus-connection?
                     DBusMessage-pointer?
                     DBusMessage-pointer?)
  (let* ((pending-call (dbus_connection_send_with_reply
                         (dbus-connection-dbc bus) message -1))
         (channel      (make-async-channel)))

    (unless pending-call
      (throw exn:fail:dbus "failed to send message" "unknown"))

    (let* ((notify-callback (lambda (pending-call dummy)
                              (async-channel-put channel #t)))
           (boxee (box notify-callback)))
      (dbus_pending_call_set_notify pending-call notify-callback #f)
      (unless (dbus_pending_call_get_completed pending-call)
        (async-channel-get channel))
      (set-box! boxee #f))

    (let* ((message (dbus_pending_call_steal_reply pending-call))
           (error   (dbus_set_error_from_message message)))
      (when error
        (raise (dbus-error->exception error)))
      message)))


(define/contract (dbus-subscribe bus path iface signal handler)
                 (-> dbus-connection? string? string? string? procedure? void?)
  (let* ((dbc (dbus-connection-dbc bus))
         (key (list dbc path iface signal)))
      (hash-set! signals key (set-add (hash-ref signals key (set)) handler))
      (dbus_bus_add_match dbc
        (string-append "type='signal',"
                       "interface='" iface "',"
                       "member='" signal "',"
                       "path='" path "'"))))


(define/contract (dbus-unsubscribe bus path iface signal handler)
                 (-> dbus-connection? string? string? string? procedure? void?)
  (let* ((dbc (dbus-connection-dbc bus))
         (key (list path iface signal)))
    (let ((items (set-remove (hash-ref signals key (set)) handler)))
      (if (= 0 (set-count items))
        (begin
          (dbus_bus_remove_match bus
            (string-append "type='signal',"
                           "interface='" iface "',"
                           "member='" signal "',"
                           "path='" path "'"))
          (hash-remove! signals key))
        (hash-set! signals key items)))))


; vim:set ts=2 sw=2 et:
