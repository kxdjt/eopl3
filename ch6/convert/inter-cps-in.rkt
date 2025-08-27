#lang racket

(require eopl)

(require "./continuation-interface-sig.rkt"
         "./lang-cps-in.rkt"
         "./data-structures-unit.rkt"
         "./procedure-cps-in-unit.rkt"
         "./eval-cps-in-unit.rkt"
         "./operator-functions-unit.rkt"
         "./gramer-cps-in-unit.rkt"
         "./senv-unit.rkt"
         "./store-unit.rkt"
         "../../common/enironment.rkt")

(define-compound-unit/infer inter-cpsin@
  (import)
  (export data-structures^ cont-valueof^ senv^)
  (link value-of/k-cpsin@
        proc-cps-in@
        data-structures@
        gramer-cpsin@
        operator-fun@
        senv@
        store@))
(define-values/invoke-unit/infer inter-cpsin@)

(provide run expval->schemaval)

#| Interpreter for the LET language |#
;; run : string -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse-cps-in string))))
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
                             (end-cont))))))

(define end-cont
  (lambda ()
    (lambda (val store)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))
