#lang racket/base
;
; D-Bus Object Introspection
;

(require racket/contract
         racket/string
         racket/list
         racket/dict
         xml)

(require "main.rkt"
         "proxy.rkt"
         "private/util.rkt")

(provide dbus-introspect)


(define/contract (arg-xexpr->dbus-signature xexpr)
                 (-> xexpr/c dbus-signature?)
  (let-values (((args children) (decompose-tag xexpr)))
    (dict-ref args 'type)))


(define/contract (method-xexpr->dbus-method bus endpoint path interface xexpr)
                 (-> dbus-connection? dbus-bus-name? dbus-object-path?
                     dbus-interface-name? xexpr/c dbus-method?)
  (let-values (((args children) (decompose-tag xexpr)))
    (dbus-method bus endpoint path interface
                 (dict-ref args 'name)
                 (string-append*
                   (map arg-xexpr->dbus-signature
                        (filter (lambda (arg)
                                  (let-values (((args children)
                                                (decompose-tag arg)))
                                    (equal? "in" (dict-ref args 'direction))))
                                children))))))


(define/contract (property-xexpr->dbus-property bus endpoint path interface
                                                xexpr)
                 (-> dbus-connection? dbus-bus-name? dbus-object-path?
                     dbus-interface-name? xexpr/c dbus-property?)
  (let-values (((args children) (decompose-tag xexpr)))
    (dbus-property bus endpoint path interface
                   (dict-ref args 'name)
                   (dict-ref args 'type))))


(define/contract (signal-xexpr->dbus-signal bus endpoint path interface xexpr)
                 (-> dbus-connection? dbus-bus-name? dbus-object-path?
                     dbus-interface-name? xexpr/c dbus-signal?)
  (let-values (((args children) (decompose-tag xexpr)))
    (dbus-signal bus endpoint path interface
                 (dict-ref args 'name)
                 (string-append* (map arg-xexpr->dbus-signature children)))))


(define/contract (node-xexpr->dbus-object bus endpoint path xexpr)
                 (-> dbus-connection? dbus-bus-name? dbus-object-path? xexpr/c
                     dbus-object?)
  (define (filter-tags name children)
    (filter (lambda (tag) (equal? (car tag) name)) children))

  (define (parse-interface xexpr)
    (let-values (((args children) (decompose-tag xexpr)))
      (let ((name (dict-ref args 'name)))
        (list (map (lambda (m)
                     (method-xexpr->dbus-method bus endpoint path name m))
                   (filter-tags 'method children))
              (map (lambda (m)
                     (property-xexpr->dbus-property bus endpoint path name m))
                   (filter-tags 'property children))
              (map (lambda (m)
                     (signal-xexpr->dbus-signal bus endpoint path name m))
                   (filter-tags 'signal children))))))

  (define (hash-members members)
    (make-hash
      (map (lambda (memb)
             (cons (dbus-member-name memb) memb))
           members)))

  (let ((data (map parse-interface (filter-tags 'interface (cddr xexpr)))))
    (dbus-object bus endpoint path
                 (hash-members (append* (map first data)))
                 (hash-members (append* (map second data)))
                 (hash-members (append* (map third data))))))


(define/contract (dbus-introspect bus endpoint path)
                 (-> dbus-connection? dbus-bus-name? dbus-object-path?
                     dbus-object?)
  (node-xexpr->dbus-object bus endpoint path
    (xexpr-strip-whitespace
      (string->xexpr
        (dbus-call bus endpoint path
                   "org.freedesktop.DBus.Introspectable" "Introspect" "")))))


; vim:set ts=2 sw=2 et:
