#lang racket


(require eopl)
(require "../procedure-sig.rkt")

(provide (all-defined-out))

(define-signature datatype^
  (proc-val))
(define-unit datatype@
  (import proc-def^)
  (export datatype^)
  (define-datatype Data Data?
    (proc-val
     (proc proc?))))

(provide datatype^ datatype@)
