#lang racket

(require "./continuation-interface-sig.rkt")
(require "./lang-cps-out.rkt")
(require "./data-structures-unit.rkt")
(require "./procedure-sig.rkt")
(require "./operator-functions-unit.rkt")
(require "./senv-with-s-store-unit.rkt")
(require "./store-s-unit.rkt")
(require "../../common/enironment.rkt")
(require "../../common/utils.rkt")
(require "./fmt-cps-out.rkt")
(require eopl)

(provide value-of/k-cpsout@)

(define-unit value-of/k-cpsout@
  (import data-structures^ proc-def^ operator-fun^ senv^ s-store^)
  (export cont-valueof^)


  ;;value-of/k : TfExp × Env × Cont → FinalAnswer
  (define value-of/k
    (lambda (exp senv cont)
      (debug-trace "value-of/k"
                   (string-append "exp:\n" (exp->fmt exp) "\n"))
      (cases tfexp exp
        (simple-exp->exp (simple)
                         (apply-cont cont
                                     (value-of-simple-exp simple senv)))
        (cps-let-exp (vars rhss body)
                     (let ((rhs-vals (map
                                      (lambda(simple)
                                        (value-of-simple-exp simple senv))
                                      rhss)))
                       (value-of/k body
                                   (extend-senv* vars rhs-vals senv)
                                   cont)))
        (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
                        (value-of/k letrec-body
                                    (extend-senv-rec* p-names b-varss p-bodies senv)
                                    cont))
        (cps-if-exp (simple1 body1 body2)
                    (if (expval->bool (value-of-simple-exp simple1 senv))
                        (value-of/k body1 senv cont)
                        (value-of/k body2 senv cont)))
        (cps-call-exp (rator rands)
                      (let ((rator
                             (value-of-simple-exp rator senv))
                            (rand-vals
                             (map
                              (lambda (simple)
                                (value-of-simple-exp simple senv))
                              rands)))
                        (apply-procedure/k (expval->proc rator) rand-vals cont))))))

  ;;apply-cont : Cont × ExpVal -> ExpVal
  (define apply-cont
    (lambda (cont val)
      (cont val)))

  ;;value-of-simple-exp : SimpleExp × Env → ExpVal
  (define value-of-simple-exp
    (lambda (simple senv)
      (debug-trace "value-of-simple-exp"
                   (string-append "exp:\n" (exp->fmt simple) "\n"))
      (cases simpleexp simple
        (cps-const-exp (number) (num-val number))
        (cps-var-exp (ident) (apply-senv senv ident))
        (cps-str-exp (str) (str-val str))
        (cps-set-exp (ident simp)
                     (let ((res (value-of-simple-exp simp senv)))
                       (setref!
                        (apply-env senv ident)
                        res)
                       res))
        (cps-proc-exp (vars body) (proc-val
                                   (procedure vars body senv)))
        (cps-innerop-exp (inner-op simples)
                         (let ((rand-vals
                                (map
                                 (lambda (simple)
                                   (value-of-simple-exp simple senv))
                                 simples)))
                           (cases cps-inner-operator inner-op
                             (cps-none-op (op)
                                          ((none-operator op)))
                             (cps-binary-op (op)
                                            ((binary-operator op) rand-vals))
                             (cps-unary-op (op)
                                           ((unary-operator op) (car rand-vals)))
                             (cps-any-op (op)
                                         ((any-operator op) rand-vals))
                             )))
        )))
  )
