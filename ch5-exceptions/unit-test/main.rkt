#lang racket

(require "../procedure-sig.rkt"
         "validation-unit.rkt"
         "data-structures.rkt")

(define-compound-unit/infer app@
  (import)
  (export datatype^)
  (link simple-proc-def@ datatype@))
(define-values/invoke-unit/infer app@)

(define p-val (proc-val (lambda() 2)))
(provide (all-defined-out))
