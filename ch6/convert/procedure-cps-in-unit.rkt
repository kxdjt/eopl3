#lang racket

(require "./lang-cps-in.rkt"
         "./procedure-sig.rkt"
         "./continuation-interface-sig.rkt"
         "./senv-unit.rkt"
         "../../common/enironment.rkt")
(require eopl)

(provide proc-cps-in@)

(define-unit proc-cps-in@
  (import cont-valueof^ senv^)
  (export proc-def^)


  ;; Define procedure data type by datetype
  (define-datatype proc proc?
    (procedure
     (args (list-of symbol?))
     (body inpexp?)
     (env environment?)))
  ;;apply-procedure : Proc × ExpVal → ExpVal
  (define apply-procedure/k
    (lambda (proc1 args store cont)
      (cases proc proc1
        (procedure (vars body saved-env)
                   (value-of/k body
                               (extend-senv* vars args
                                             (cons saved-env store))
                               cont)))))
  )
