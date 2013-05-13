#lang racket/base
;
; Message Handling
;


(require "ffi.rkt"
         "common.rkt")

(require racket/contract
         racket/generator
         racket/list
         racket/match)

(provide (all-defined-out))


(define/contract (iter->list iter)
                 (-> DBusMessageIter? list?)
  (if (eq? (dbus_message_iter_get_arg_type iter) 'invalid)
    null
    (let ((value (iter-current-value iter)))
      (dbus_message_iter_next iter)
      (cons value (iter->list iter)))))


(define/contract (iter-current-value iter)
                 (-> DBusMessageIter? any/c)
  (match (dbus_message_iter_get_arg_type iter)
    ('array      (iter->list (dbus_message_iter_recurse iter)))
    ('variant    (car (iter->list (dbus_message_iter_recurse iter))))
    ('struct     (iter->list (dbus_message_iter_recurse iter)))
    ('dict-entry (apply cons (iter->list
                               (dbus_message_iter_recurse iter))))
    (_           (dbus_message_iter_get_basic iter))))


(define/contract (dbus-unpack-raw msg)
                 (-> DBusMessage-pointer? any)
  (let ((iter (dbus_message_iter_init msg)))
    (apply values (if iter (iter->list iter) null))))


(define/contract (in-signature-iter signature-iter)
                 (-> DBusSignatureIter? sequence?)
  (in-producer
    (generator ()
      (let loop ()
        (let ((type (dbus_signature_iter_get_current_type signature-iter)))
          (unless (eq? type 'invalid)
            (yield type)
            (dbus_signature_iter_next signature-iter)
            (loop)))))
    (void)))


(define/contract (iter-append-value message-iter signature-iter type value)
                 (-> DBusMessageIter? DBusSignatureIter? symbol? any/c void?)
  (cond
    ((dbus_type_is_basic type)
     (dbus_message_iter_append_basic message-iter type
                                     (make-basic-value type value)))

    (else
     (let* ((signature-sub
              (match type
                ('variant (dbus_signature_iter_init (dbus-variant-type value)))
                (_        (dbus_signature_iter_recurse signature-iter))))

            (item-signature
              (match type
                ('variant (dbus-variant-type value))
                ('array   (dbus_signature_iter_get_signature signature-sub))
                (_        #f)))

            (message-sub (dbus_message_iter_open_container message-iter
                                                           type
                                                           item-signature)))

       (with-handlers ((exn:fail? (lambda (e)
                                    (dbus_message_iter_abandon_container
                                      message-iter
                                      message-sub)
                                    (raise e))))
         (cond
           ((eq? type 'array)
            (for-each (lambda (item)
                        (list->iter message-sub signature-sub (list item)))
                      value))

           (else
            (list->iter
              message-sub
              signature-sub
              (match type
                ('variant    (list (dbus-variant-value value)))
                ('struct     value)
                ('dict-entry (list (car value) (cdr value)))))))

         (dbus_message_iter_close_container message-iter message-sub))))))


(define/contract (list->iter message-iter signature-iter args)
                 (-> DBusMessageIter? DBusSignatureIter? list? void?)
  (for ((type  (in-signature-iter signature-iter))
        (value (in-list args)))
    (iter-append-value message-iter signature-iter type value)))


(define/contract (dbus-append-raw msg signature . args)
                 (->* (DBusMessage-pointer? dbus-signature?)
                      () #:rest list? void?)
  (let ((message-iter (dbus_message_iter_init_append msg))
        (signature-iter (dbus_signature_iter_init signature)))
    (list->iter message-iter signature-iter args)))


; vim:set ts=2 sw=2 et:
