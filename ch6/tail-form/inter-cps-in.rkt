#lang racket

(require eopl)

(require "./continuation-interface-sig.rkt"
         "./lang-cps-in.rkt"
         "./data-structures-unit.rkt"
         "./procedure-cps-in-unit.rkt"
         "./eval-cps-in-unit.rkt"
         "./operator-functions-unit.rkt"
         "./gramer-cps-in-unit.rkt"
         "../../common/enironment.rkt")

(define-compound-unit/infer inter-cpsin@
  (import)
  (export data-structures^ cont-valueof^)
  (link value-of/k-cpsin@
        proc-cps-in@
        data-structures@
        gramer-cpsin@
        operator-fun@))
(define-values/invoke-unit/infer inter-cpsin@)

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
                             (init-env)
                             (end-cont))))))

(define end-cont
  (lambda ()
    (lambda (val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))
