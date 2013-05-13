#lang racket/base
;
; Common Code
;


(require "ffi.rkt")

(require racket/contract)

(provide (all-defined-out))


(define-struct/contract (exn:fail:dbus exn:fail)
  ((name string?))
  #:transparent)


(define/contract (dbus-signature? v)
                 (-> any/c boolean?)
  (and (string? v)
       (let-values (((result error) (dbus_signature_validate v)))
         result)))


(define-struct/contract dbus-variant
  ((type dbus-signature?)
   (value any/c))
  #:transparent)


(define/contract (dbus-error->exception error)
                 (-> DBusError? exn:fail:dbus?)
  (make-exn:fail:dbus (DBusError-message error)
                      (current-continuation-marks)
                      (DBusError-name error)))


; vim:set ts=2 sw=2 et:
