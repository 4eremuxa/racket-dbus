#lang racket/base
;
; Racket D-Bus Bindings
;

(require racket/contract)

(require "private/common.rkt"
         "private/connection.rkt"
         "private/message.rkt"
         "private/ffi.rkt")


(provide exn:fail:dbus?
         exn:fail:dbus-name
         dbus-connection?
         dbus-signature?
         dbus-interface-name?
         dbus-bus-name?
         dbus-well-known-name?
         dbus-unique-name?
         dbus-object-path?
         dbus-member-name?
         dbus-open
         dbus-call
         dbus-send-message
         dbus-send-signal
         dbus-subscribe
         dbus-unsubscribe
         dbus-variant
         dbus-variant-type
         dbus-variant-value
         dbus-variant?)


(define/contract (dbus-send-message bus target path iface method sign . args)
                 (->* (dbus-connection?
                       dbus-bus-name?
                       dbus-object-path?
                       dbus-interface-name?
                       dbus-member-name?
                       dbus-signature?)
                      () #:rest list? void?)
  (let ((message (dbus_message_new_method_call target path iface method)))
    (dbus_message_set_no_reply message #t)
    (apply dbus-append-raw message sign args)
    (dbus-send-raw bus message)))


(define/contract (dbus-send-signal bus path iface name sign . args)
                 (->* (dbus-connection?
                       dbus-object-path?
                       dbus-interface-name?
                       dbus-member-name?
                       dbus-signature?)
                      () #:rest list? void?)
  (let ((message (dbus_message_new_signal path iface name)))
    (apply dbus-append-raw message sign args)
    (dbus-send-raw bus message)))


(define/contract (dbus-call bus target path iface method sign . args)
                 (->* (dbus-connection?
                       dbus-bus-name?
                       dbus-object-path?
                       dbus-interface-name?
                       dbus-member-name?
                       dbus-signature?)
                      () #:rest list? any)
  (let ((message (dbus_message_new_method_call target path iface method)))
    (apply dbus-append-raw message sign args)
    (dbus-unpack-raw (dbus-call-raw bus message))))


; vim:set ts=2 sw=2 et:
