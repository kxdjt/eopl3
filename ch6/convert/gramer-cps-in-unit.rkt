#lang racket

(require (prefix-in cps- "./lang-cps-in.rkt")
         "./gramer-sig.rkt")

(provide gramer-cpsin@)

(define-unit gramer-cpsin@
  (import)
  (export gramer-inf^)

  (define inner-operator?
    cps-inner-operator?))
