#lang racket

(require "lang-cps-out.rkt")
(require "procedure-sig.rkt")
(require "./continuation-interface-sig.rkt")
(require "../../common/enironment.rkt")
(require "./senv-with-s-store-unit.rkt")
(require eopl)

(provide proc-cps-out@)

(define-unit proc-cps-out@
  (import cont-valueof^ senv^)
  (export proc-def^)


  ;; Define procedure data type by datetype
  (define-datatype proc proc?
    (procedure
     (args (list-of symbol?))
     (body tfexp?)
     (env environment?)))
  ;;apply-procedure : Proc × ExpVal → ExpVal
  (define apply-procedure/k
    (lambda (proc1 args cont)
      (cases proc proc1
        (procedure (vars body saved-env)
                   (value-of/k body
                               (extend-senv* vars args saved-env)
                               cont)))))
  )
