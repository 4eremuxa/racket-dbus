#lang racket/base
;
; D-Bus Object Proxies
;

(require racket/contract)

(require "main.rkt"
         "private/util.rkt")

(provide (all-defined-out))


(define-struct/contract dbus-member
  ((bus        dbus-connection?)
   (endpoint   dbus-bus-name?)
   (path       dbus-object-path?)
   (interface  dbus-interface-name?)
   (name       dbus-member-name?)
   (signature  dbus-signature?))
  #:transparent)

(define-struct/contract (dbus-method dbus-member) () #:transparent)
(define-struct/contract (dbus-property dbus-member) () #:transparent)
(define-struct/contract (dbus-signal dbus-member) () #:transparent)

(define-struct/contract dbus-object
  ((bus         dbus-connection?)
   (endpoint    dbus-bus-name?)
   (path        dbus-object-path?)
   (methods     (hash/c dbus-member-name? dbus-method?))
   (properties  (hash/c dbus-member-name? dbus-property?))
   (signals     (hash/c dbus-member-name? dbus-signal?)))
  #:transparent)


(define/contract (dbus-method-call method . args)
                 (->* (dbus-method?) () #:rest list? any)
  (apply dbus-call (dbus-member-bus method)
                   (dbus-member-endpoint method)
                   (dbus-member-path method)
                   (dbus-member-interface method)
                   (dbus-member-name method)
                   (dbus-member-signature method)
                   args))


(define/contract (dbus-method-send method . args)
                 (->* (dbus-method?) () #:rest list? any)
  (apply dbus-send-message (dbus-member-bus method)
                           (dbus-member-endpoint method)
                           (dbus-member-path method)
                           (dbus-member-interface method)
                           (dbus-member-name method)
                           (dbus-member-signature method)
                           args))


(define/contract (dbus-object-call object method . args)
                 (->* (dbus-object? dbus-member-name?)
                      () #:rest list? any)
  (let ((method (hash-ref (dbus-object-methods object) method)))
    (apply dbus-method-call method args)))


(define/contract (dbus-object-send object method . args)
                 (->* (dbus-object? dbus-member-name?)
                      () #:rest list? void?)
  (let ((method (hash-ref (dbus-object-methods object) method)))
    (apply dbus-method-send method args)))


(define/contract (dbus-object-get object property)
                 (-> dbus-object? dbus-member-name? any)
  (let ((property (hash-ref (dbus-object-properties object) property))
        (method   (hash-ref (dbus-object-methods object) "Get")))
    (dbus-method-call method (dbus-member-interface property)
                             (dbus-member-name property))))


(define/contract (dbus-object-set! object property . value)
                 (->* (dbus-object? dbus-member-name?) () #:rest list? any)
  (let ((property (hash-ref (dbus-object-properties object) property))
        (method   (hash-ref (dbus-object-methods object) "Set")))
    (dbus-method-call method (dbus-member-interface property)
                             (dbus-member-name property)
                             (dbus-variant (dbus-member-signature property)
                                           value))))


; vim:set ts=2 sw=2 et:
