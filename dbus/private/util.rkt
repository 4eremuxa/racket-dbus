#lang racket/base
;
; Misc Utilities
;

(require racket/contract
         racket/string
         racket/match
         xml)

(provide (all-defined-out))


(define/contract (xexpr-strip-whitespace expr)
                 (-> xexpr/c xexpr/c)
  (if (pair? expr)
    (if (string? (car expr))
      (if (string=? "" (string-trim (car expr)))
        (xexpr-strip-whitespace (cdr expr))
        (cons (car expr)
              (xexpr-strip-whitespace (cdr expr))))
      (cons (xexpr-strip-whitespace (car expr))
            (xexpr-strip-whitespace (cdr expr))))
    expr))


(define/contract (decompose-tag tag)
                 (-> xexpr/c (values (listof (cons/c symbol? string?))
                                     (listof xexpr/c)))
  (match tag
    ((list _ (var args) (var children) ...)
     (let ((args (map (lambda (arg-pair)
                        (cons (car arg-pair) (cadr arg-pair)))
                      args)))
       (values args children)))))


; vim:set ts=2 sw=2 et:
