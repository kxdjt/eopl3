#lang racket

(require "./continuation-interface-sig.rkt")
(require "./lang-cps-out.rkt")
(require "./data-structures-unit.rkt")
(require "./procedure-sig.rkt")
(require "./operator-functions-unit.rkt")
(require "../../common/enironment.rkt")
(require "../../common/utils.rkt")
(require eopl)

(provide value-of/k-cpsout@)

(define-unit value-of/k-cpsout@
  (import data-structures^ proc-def^ operator-fun^)
  (export cont-valueof^)


  ;;value-of/k : TfExp × Env × Cont → FinalAnswer
  (define value-of/k
    (lambda (exp env cont)
      (cases tfexp exp
        (simple-exp->exp (simple)
                         (apply-cont cont
                                     (value-of-simple-exp simple env)))
        (cps-let-exp (vars rhss body)
                     (let ((rhs-vals (map
                                      (lambda(simple)
                                        (value-of-simple-exp simple env))
                                      rhss)))
                       (value-of/k body
                                   (extend-env* vars rhs-vals env)
                                   cont)))
        (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
                        (value-of/k letrec-body
                                    (extend-env-rec* p-names b-varss p-bodies env)
                                    cont))
        (cps-if-exp (simple1 body1 body2)
                    (if (expval->bool (value-of-simple-exp simple1 env))
                        (value-of/k body1 env cont)
                        (value-of/k body2 env cont)))
        (cps-call-exp (rator rands)
                      (let ((rator
                             (value-of-simple-exp rator env))
                            (rand-vals
                             (map
                              (lambda (simple)
                                (value-of-simple-exp simple env))
                              rands)))
                        (apply-procedure/k (expval->proc rator) rand-vals cont))))))

  ;;apply-cont : Cont × ExpVal -> ExpVal
  (define apply-cont
    (lambda (cont val)
      (cont val)))

  ;;value-of-simple-exp : SimpleExp × Env → ExpVal
  (define value-of-simple-exp
    (lambda (simple env)
      (cases simpleexp simple
        (const-exp (number) (num-val number))
        (var-exp (ident) (apply-env env ident))
        (cps-proc-exp (vars body) (proc-val
                                   (procedure vars body env)))
        (innerop-exp (inner-op simples)
                     (let ((rand-vals
                            (map
                             (lambda (simple)
                               (value-of-simple-exp simple env))
                             simples)))
                       (cases inner-operator inner-op
                         (none-op (op)
                                  ((none-operator op)))
                         (binary-op (op)
                                    ((binary-operator op) rand-vals))
                         (unary-op (op)
                                   ((unary-operator op) (car rand-vals)))
                         (any-op (op)
                                 ((any-operator op) rand-vals))
                         )))
        )))

  (define extend-env-rec*
    (lambda (proc-names list-of-vars exps env)
      (cons
       (lambda (search-var)
         (let ((res (ormap (lambda(proc-name vars exp1)
                             (if (equal? search-var proc-name)
                                 (proc-val (procedure vars exp1
                                                      (extend-env-rec* proc-names list-of-vars exps env)))
                                 #f))
                           proc-names
                           list-of-vars
                           exps)))
           (if (not res)
               (apply-env env search-var)
               res)))
       (lambda (search-var)
         (if (list-member? search-var proc-names)
             #t
             (has-binding? env search-var))))))
  )
