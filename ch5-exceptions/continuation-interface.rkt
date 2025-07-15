#lang racket

(require "continuation-passing-evaluation.rkt")
(require "continuations-definition.rkt")

(provide (all-from-out "continuation-passing-evaluation.rkt"))
(provide (all-from-out "continuations-definition.rkt"))

(require (for-syntax "continuations-definition.rkt"))
(require-provide-conditional "continuation-passing-apply.rkt"
                             "continuation-apply-for-fc.rkt")

(provide (all-defined-out))

(define apply-procedure/k
  (lambda (proc vals senv cont)
    (proc vals senv cont)))
