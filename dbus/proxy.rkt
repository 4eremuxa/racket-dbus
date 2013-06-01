#lang racket/base
;
; D-Bus Object Proxies
;

(require racket/contract
         racket/dict)

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


(define/contract (dbus-property-get property)
                 (-> dbus-property? any)
  (dbus-call (dbus-member-bus property)
             (dbus-member-endpoint property)
             (dbus-member-path property)
             "org.freedesktop.DBus.Properties" "Get"
             "ss" (dbus-member-interface property)
                  (dbus-member-name property)))


(define/contract (dbus-property-set! property . value)
                 (->* (dbus-property?) () #:rest list? any)
  (apply dbus-call (dbus-member-bus property)
                   (dbus-member-endpoint property)
                   (dbus-member-path property)
                   "org.freedesktop.DBus.Properties" "Set"
                   "ssv" (dbus-member-interface property)
                         (dbus-member-name property)
                         (dbus-variant (dbus-member-signature property)
                                       value)))


(define/contract (dbus-signal-subscribe signal handler)
                 (-> dbus-signal? procedure? void?)
  (dbus-subscribe (dbus-member-bus signal)
                  (dbus-member-path signal)
                  (dbus-member-interface signal)
                  (dbus-member-name signal)
                  handler))


(define/contract (dbus-signal-unsubscribe signal handler)
                 (-> dbus-signal? procedure? void?)
  (dbus-unsubscribe (dbus-member-bus signal)
                    (dbus-member-path signal)
                    (dbus-member-interface signal)
                    (dbus-member-name signal)
                    handler))


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
  (dbus-property-get (hash-ref (dbus-object-properties object) property)))


(define/contract (dbus-object-get-all object)
                 (-> dbus-object? list?)
  (for/list ((prop (in-dict-pairs (dbus-object-properties object))))
    (cons (car prop) (dbus-property-get (cdr prop)))))


(define/contract (dbus-object-set! object property . value)
                 (->* (dbus-object? dbus-member-name?) () #:rest list? any)
  (apply dbus-property-set! (hash-ref (dbus-object-properties object) property)
                            value))


(define/contract (dbus-object-subscribe object signal handler)
                 (-> dbus-object? dbus-member-name? procedure? void?)
  (dbus-signal-subscribe (hash-ref (dbus-object-signals object) signal)
                         handler))


(define/contract (dbus-object-unsubscribe object signal handler)
                 (-> dbus-object? dbus-member-name? procedure? void?)
  (dbus-signal-unsubscribe (hash-ref (dbus-object-signals object) signal)
                           handler))


; vim:set ts=2 sw=2 et:
