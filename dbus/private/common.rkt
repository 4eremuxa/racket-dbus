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


(define/contract (dbus-interface-name? v)
                 (-> any/c boolean?)
  (and (string? v)
       (< (string-length v) 256)
       (> (string-length v) 0)
       (let ((elements (regexp-split #rx"\\." v)))
         (and (> (length elements) 1)
              (andmap (lambda (e)
                        (regexp-match? #rx"^[A-Za-z_][A-Za-z0-9_]*$" e))
                      elements)))))


(define/contract (dbus-well-known-name? v)
                 (-> any/c boolean?)
  (and (string? v)
       (< (string-length v) 256)
       (> (string-length v) 0)
       (let ((elements (regexp-split #rx"\\." v)))
         (and (> (length elements) 1)
              (andmap (lambda (e)
                        (regexp-match? #rx"^[A-Za-z_-][A-Za-z0-9_-]*$" e))
                      elements)))))


(define/contract (dbus-unique-name? v)
                 (-> any/c boolean?)
  (and (string? v)
       (< (string-length v) 256)
       (> (string-length v) 0)
       (string=? (substring v 0 1) ":")
       (let ((elements (regexp-split #rx"\\." (substring v 1))))
         (and (> (length elements) 1)
              (andmap (lambda (e)
                        (regexp-match? #rx"^[A-Za-z0-9_-]+$" e))
                      elements)))))


(define/contract (dbus-object-path? v)
                 (-> any/c boolean?)
  (and (string? v)
       (> (string-length v) 0)
       (string=? (substring v 0 1) "/")
       (or (= (string-length v) 1)
           (let ((elements (regexp-split #rx"/" (substring v 1))))
             (andmap (lambda (e)
                       (regexp-match? #rx"^[A-Za-z0-9_]+$" e))
                     elements)))))


(define/contract (dbus-member-name? v)
                 (-> any/c boolean?)
  (and (string? v)
       (regexp-match? #rx"^[A-Za-z_][A-Za-z0-9_]*$" v)))


(define/contract (dbus-bus-name? v)
                 (-> any/c boolean?)
  (or (dbus-unique-name? v)
      (dbus-well-known-name? v)))


(define-struct/contract dbus-variant
  ((type dbus-signature?)
   (value any/c))
  #:transparent)


(define/contract (dbus-error->exception error)
                 (-> DBusError? exn:fail:dbus?)
  (make-exn:fail:dbus (DBusError-message error)
                      (current-continuation-marks)
                      (DBusError-name error)))


(define/contract (dbus-check-error error)
                 (-> DBusError? void?)
  (when (dbus_error_is_set error)
    (raise (dbus-error->exception error))))


; vim:set ts=2 sw=2 et:
