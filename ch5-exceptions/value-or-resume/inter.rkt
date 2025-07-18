#lang racket

(require eopl)

(require "continuation-evaluation-unit.rkt"
         "continuations-dt-unit.rkt"
         "procedure-def-unit.rkt"
         "continuation-interface-sig.rkt"
         "continuations-sig.rkt"
         "data-structures-unit.rkt"
         "operator-functions-unit.rkt"
         "senv-unit.rkt"
         "store-unit.rkt")

(define-compound-unit/infer inter@
  (import)
  (export cont-valueof^ continuation^ senv^ data-structures^)
  (link value-of/k-imp@
        apply-procedure@
        data-structures@
        continuation-dt@
        proc-def@
        operator-fun@
        senv@
        store@))
(define-values/invoke-unit/infer inter@)

(provide run)


#| Interpreter for the LET language |#
;; run : string -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
(define init-senv
  (lambda ()
    (extend-senv 'i (num-val 1)
                 (extend-senv 'v (num-val 5)
                              (extend-senv 'x (num-val 10)
                                           (empty-senv))))))

;; Program -> FinalAnswer = ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of/k exp1
                             (init-senv)
                             (end-cont)
                             (empty-econt))))))
