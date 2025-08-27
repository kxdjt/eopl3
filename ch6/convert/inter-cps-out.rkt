#lang racket

(require eopl)

(require "./eval-cps-out-unit.rkt"
         "./data-structures-unit.rkt"
         "./procedure-cps-out-unit.rkt"
         "./continuation-interface-sig.rkt"
         "./operator-functions-unit.rkt"
         "./gramer-cps-out-unit.rkt"
         "./senv-with-s-store-unit.rkt"
         "./store-s-unit.rkt"
         "../../common/enironment.rkt"
         "./lang-cps-out.rkt")
(require "../../common/utils.rkt")
(require "./fmt-cps-out.rkt")

(define-compound-unit/infer inter-cpsout@
  (import )
  (export data-structures^ cont-valueof^ senv^)
  (link value-of/k-cpsout@
        senv-s@
        s-store@
        proc-cps-out@
        data-structures@
        gramer-cpsout@
        operator-fun@))
(define-values/invoke-unit/infer inter-cpsout@)

(provide run expval->schemaval value-of-program)

#| Interpreter for the LET language |#
;; run : string -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse-cps-out string))))
(define init-senv
  (lambda ()
    (extend-senv 'i (num-val 1)
                 (extend-senv 'v (num-val 5)
                              (extend-senv 'x (num-val 10)
                                           (empty-senv))))))

;; Program -> FinalAnswer = ExpVal
(define value-of-program
  (lambda (pgm)
    (cases cps-program pgm
      (cps-a-program (exp1)
                     (debug-expfmt "out-exp:"
                                   (string-append "\n" (exp->fmt exp1) "\n"))
                     (value-of/k exp1
                                 (init-senv)
                                 (end-cont))))))

(define end-cont
  (lambda ()
    (lambda (val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))

