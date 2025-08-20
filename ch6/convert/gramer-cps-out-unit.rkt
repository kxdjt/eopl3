#lang racket

(require (prefix-in cps- "./lang-cps-out.rkt")
         "./gramer-sig.rkt")

(provide gramer-cpsout@)

(define-unit gramer-cpsout@
  (import)
  (export gramer-inf^)

  (define inner-operator?
    cps-cps-inner-operator?))
