#lang racket

(require "./continuation-interface-sig.rkt")
(require "./lang-cps-out.rkt")
(require "./data-structures-unit.rkt")
(require "./procedure-sig.rkt")
(require "./operator-functions-unit.rkt")
(require "../../common/enironment.rkt")
(require "../../common/utils.rkt")
(require eopl)

(provide value-of/k-cpsout-reg@)

(define exp 'undefined)
(define env 'undefined)
(define val 'undefined)
(define pc 'undefined)

(define trampolined
  (lambda()
    (if pc
        (begin
          (pc)
          (trampolined))
        val)))

(define-unit value-of/k-cpsout-reg@
  (import data-structures^ proc-def^ operator-fun^)
  (export cont-valueof^)

  (define value-of/k
    (lambda(arg-exp arg-env)
      (debug-trace "value-of/k" "exp:~s\n" exp)
      (set! exp arg-exp)
      (set! env arg-env)
      (set! pc value-of/k-imp)
      (trampolined)
      (debug-trace "value-of/k" "exp:~s ret:~s\n" exp val)
      val))

  (define value-of/k-imp
    (lambda ()
      (cases tfexp exp
        (simple-exp->exp (simple)
                         (set! val (value-of-simple-exp simple env))
                         (set! pc #f))
        (cps-let-exp (vars rhss body)
                     (let ((rhs-vals (map
                                      (lambda(simple)
                                        (value-of-simple-exp simple env))
                                      rhss)))
                       (set! exp body)
                       (set! env (extend-env* vars rhs-vals env))))
        (cps-letrec-exp (p-names b-varss p-bodies letrec-body)
                        (set! exp letrec-body)
                        (set! env (extend-env-rec* p-names b-varss p-bodies env)))
        (cps-if-exp (simple1 body1 body2)
                    (if (expval->bool (value-of-simple-exp simple1 env))
                        (set! exp body1)
                        (set! exp body2)))
        (cps-call-exp (rator rands)
                      (let ((rator
                             (value-of-simple-exp rator env))
                            (rand-vals
                             (map
                              (lambda (simple)
                                (value-of-simple-exp simple env))
                              rands)))
                        (set! val (apply-procedure/k (expval->proc rator) rand-vals))
                        (set! pc #f)  )))))


  ;;value-of-simple-exp : SimpleExp × Env → ExpVal
  (define value-of-simple-exp
    (lambda (simple env)
      (debug-trace "value-of-simple-exp" "simple:~s\n" simple)
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
