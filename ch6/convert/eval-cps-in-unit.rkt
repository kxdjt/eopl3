#lang racket

(require "./continuation-interface-sig.rkt"
         "./lang-cps-in.rkt"
         "./data-structures-unit.rkt"
         "./procedure-sig.rkt"
         "./operator-functions-unit.rkt"
         "./fmt-cps-in.rkt"
         "../../common/enironment.rkt"
         "../../common/utils.rkt")
(require eopl)

(provide value-of/k-cpsin@)

(define-unit value-of/k-cpsin@
  (import data-structures^ proc-def^ operator-fun^)
  (export cont-valueof^)

  (define value-of/k
    (lambda(exp env cont)
      (debug-trace "value-of/k" "exp:~s\n" (exp->fmt exp))
      (cases inpexp exp
        (const-exp (number)
                   (apply-cont cont (num-val number)))
        (if-exp (exp1 exp2 exp3)
                (value-of/k exp1 env
                            (if-cont exp2 exp3 env cont)))
        (var-exp (ident)
                 (apply-cont cont (apply-env env ident)))
        (let-exp (idents exps1 exp2)
                 (if (null? idents)
                     (value-of/k exp2 env cont)
                     (value-of/k (car exps1) env
                                 (let-cont idents (cdr exps1) exp2 env cont))))
        (letrec-exp (p-names b-varss p-bodies letrec-body)
                    (value-of/k letrec-body
                                (extend-env-rec* p-names b-varss p-bodies env)
                                cont))
        (proc-exp (b-vars p-body)
                  (apply-cont cont
                              (proc-val
                               (procedure b-vars p-body env))))
        (call-exp (exp1 exps2)
                  (value-of/k exp1 env
                              (call-cont exps2 env cont)))
        (innerop-exp (inner-op exps)
                     (cases inner-operator inner-op
                       (none-op (op)
                                (apply-cont cont ((none-operator op))))
                       (binary-op (op)
                                  (value-of/k (car exps)
                                              env
                                              (binary-op-cont1 op (cdr exps) env cont)))
                       (unary-op (op)
                                 (value-of/k (car exps)
                                             env
                                             (unary-op-cont op cont)))
                       (any-op (op)
                               (if (null? exps)
                                   (apply-cont cont
                                               ((any-operator op) '()))
                                   (value-of/k (car exps)
                                               env
                                               (any-op-cont op (cdr exps) env cont '()))))
                       ))
        )))

  (define apply-cont
    (lambda (cont val)
      (cont val)))

  (define if-cont
    (lambda (exp2 exp3 env cont)
      (lambda (val)
        (if (expval->bool val)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont)))))
  (define let-cont
    (lambda (idents exps exp2 env cont)
      (lambda(val)
        (let ((new-env (extend-env (car idents) val env)))
          (if (null? exps)
              (value-of/k exp2
                          new-env
                          cont)
              (value-of/k (car exps)
                          new-env
                          (let-cont (cdr idents) (cdr exps)
                                    exp2 new-env cont)))))))
  (define call-cont
    (lambda (exps2 env cont)
      (lambda (val)
        (debug-trace "call-cont" "val:~s exps2:~s\n" val exps2)
        (let ((proc (expval->proc val)))
          (if (null? exps2)
              (apply-procedure/k proc '() cont)
              (value-of/k (car exps2)
                          env
                          (proc-cont proc (cdr exps2) env cont '()))))
        )))

  (define binary-op-cont1
    (lambda (op exps env cont)
      (lambda (val)
        (value-of/k (car exps)
                    env
                    (binary-op-cont2 op val cont)))))
  (define binary-op-cont2
    (lambda (op val1 cont)
      (lambda (val)
        (apply-cont cont
                    ((binary-operator op) (list val1 val))))))
  (define unary-op-cont
    (lambda (op cont)
      (lambda (val)
        (apply-cont cont
                    ((unary-operator op) val)))))
  (define any-op-cont
    (lambda(op exps env cont rands)
      (lambda(val)
        (if (null? exps)
            (apply-cont cont
                        ((any-operator op) (append rands (list val))))
            (value-of/k (car exps)
                        env
                        (any-op-cont op (cdr exps) env cont
                                     (append rands (list val))))))))
  (define proc-cont
    (lambda(proc exps env cont rands)
      (lambda(val)
        (debug-trace "proc-cont" "val:~s exps:~s rands:~s\n" val exps rands)
        (if (null? exps)
            (apply-procedure/k proc (append rands (list val)) cont)
            (value-of/k (car exps)
                        env
                        (proc-cont proc (cdr exps) env cont
                                   (append rands (list val))))))))

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
