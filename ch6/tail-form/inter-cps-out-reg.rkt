#lang racket

(require eopl)

(require "./eval-cps-out-reg-unit.rkt"
         "./data-structures-unit.rkt"
         "./procdure-cps-out-rmcont-unit.rkt"
         "./continuation-interface-sig.rkt"
         "./operator-functions-unit.rkt"
         "./gramer-cps-out-unit.rkt"
         "../../common/enironment.rkt"
         "./lang-cps-out.rkt")

(define-compound-unit/infer inter-cpsout@
  (import)
  (export data-structures^ cont-valueof^)
  (link value-of/k-cpsout-reg@
        proc-cps-out-rmcont@
        data-structures@
        gramer-cpsout@
        operator-fun@))
(define-values/invoke-unit/infer inter-cpsout@)

(provide run expval->schemaval)

#| Interpreter for the LET language |#
;; run : string -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
                (extend-env 'v (num-val 5)
                            (extend-env 'x (num-val 10)
                                        (empty-env))))))

;; Program -> FinalAnswer = ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of/k exp1
                             (init-env))))))
