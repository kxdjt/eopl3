#lang racket

(require "./continuation-interface-sig.rkt"
         "./lang-cps-in.rkt"
         "./data-structures-unit.rkt"
         "./procedure-sig.rkt"
         "./operator-functions-unit.rkt"
         "./fmt-cps-in.rkt"
         "./senv-unit.rkt"
         "./store-unit.rkt"
         "../../common/enironment.rkt"
         "../../common/utils.rkt")
(require eopl)

(provide value-of/k-cpsin@)

(define-unit value-of/k-cpsin@
  (import data-structures^ proc-def^ operator-fun^ senv^ store^)
  (export cont-valueof^)

  (define value-of/k
    (lambda (exp senv cont)
      (debug-trace "value-of/k" (string-append "exp:\n" (exp->fmt exp) "\n"))
      (let ((res (value-of/k-imp exp senv cont)))
        (debug-trace "value-of/k" "res:~s\n" res)
        res)))


  (define value-of/k-imp
    (lambda(exp senv cont)
      (let* ((store (cdr senv))
             (env (car senv))
             (make-answer (lambda(eval)
                            (an-answer eval store))))
        (cases inpexp exp
          (const-exp (number)
                     (apply-cont cont
                                 (make-answer (num-val number))))
          (str-exp (str)
                   (apply-cont cont
                               (make-answer (str-val str))))
          (if-exp (exp1 exp2 exp3)
                  (value-of/k exp1 senv
                              (if-cont exp2 exp3 env cont)))
          (var-exp (ident)
                   (apply-cont cont
                               (make-answer
                                (apply-senv senv ident))))
          (let-exp (idents exps1 exp2)
                   (if (null? idents)
                       (value-of/k exp2 senv cont)
                       (value-of/k (car exps1) senv
                                   (let-cont idents (cdr exps1) exp2 env env cont))))
          (letrec-exp (p-names b-varss p-bodies letrec-body)
                      (value-of/k letrec-body
                                  (extend-senv-rec* p-names b-varss p-bodies senv)
                                  cont))
          (proc-exp (b-vars p-body)
                    (apply-cont cont
                                (make-answer
                                 (proc-val
                                  (procedure b-vars p-body env)))))
          (call-exp (exp1 exps2)
                    (value-of/k exp1 senv
                                (call-cont exps2 env cont)))
          (innerop-exp (inner-op exps)
                       (cases inner-operator inner-op
                         (none-op (op)
                                  (apply-cont cont
                                              (make-answer
                                               ((none-operator op)))))
                         (binary-op (op)
                                    (value-of/k (car exps)
                                                senv
                                                (binary-op-cont1 op (cdr exps) env cont)))
                         (unary-op (op)
                                   (value-of/k (car exps)
                                               senv
                                               (unary-op-cont op cont)))
                         (any-op (op)
                                 (if (null? exps)
                                     (apply-cont cont
                                                 (make-answer
                                                  ((any-operator op) '())))
                                     (value-of/k (car exps)
                                                 senv
                                                 (any-op-cont op (cdr exps) env cont '()))))
                         ))
          (set-exp (ident exp1)
                   (value-of/k
                    exp1
                    senv
                    (set-cont (apply-env env ident) cont)))
          ))))

  (define apply-cont
    (lambda (cont aw)
      (let ((val (answer->eval aw))
            (store (answer->store aw)))
        (cont val store))))

  (define if-cont
    (lambda (exp2 exp3 env cont)
      (lambda (val store)
        (let ((senv (cons env store)))
          (if (expval->bool val)
              (value-of/k exp2 senv cont)
              (value-of/k exp3 senv cont))))))
  (define let-cont
    (lambda (idents exps exp2 ori-env env cont)
      (lambda(val store)
        (let ((new-senv (extend-senv (car idents)
                                     val
                                     (cons env store))))
          (if (null? exps)
              (value-of/k exp2
                          new-senv
                          cont)
              (value-of/k (car exps)
                          (cons ori-env (cdr new-senv))
                          (let-cont (cdr idents) (cdr exps)
                                    exp2 ori-env (car new-senv) cont)))))))
  (define call-cont
    (lambda (exps2 env cont)
      (lambda (val store)
        (debug-trace "call-cont" "val:~s exps2:~s\n" val exps2)
        (let ((proc (expval->proc val)))
          (if (null? exps2)
              (apply-procedure/k proc '() store cont)
              (value-of/k (car exps2)
                          (cons env store)
                          (proc-cont proc (cdr exps2) env cont '()))))
        )))

  (define binary-op-cont1
    (lambda (op exps env cont)
      (lambda (val store)
        (value-of/k (car exps)
                    (cons env store)
                    (binary-op-cont2 op val cont)))))
  (define binary-op-cont2
    (lambda (op val1 cont)
      (lambda (val store)
        (debug-trace "binary-op-cont2"
                     "op:~s val1:~s val:~s\n"
                     op val1 val)
        (apply-cont cont
                    (an-answer
                     ((binary-operator op) (list val1 val))
                     store)))))
  (define unary-op-cont
    (lambda (op cont)
      (lambda (val store)
        (apply-cont cont
                    (an-answer
                     ((unary-operator op) val)
                     store)))))
  (define any-op-cont
    (lambda(op exps env cont rands)
      (lambda(val store)
        (if (null? exps)
            (apply-cont cont
                        (an-answer
                         ((any-operator op) (append rands (list val)))
                         store))
            (value-of/k (car exps)
                        (cons env store)
                        (any-op-cont op (cdr exps) env cont
                                     (append rands (list val))))))))
  (define proc-cont
    (lambda(proc exps env cont rands)
      (lambda(val store)
        (debug-trace "proc-cont" "val:~s exps:~s rands:~s\n" val exps rands)
        (if (null? exps)
            (apply-procedure/k proc (append rands (list val)) store cont)
            (value-of/k (car exps)
                        (cons env store)
                        (proc-cont proc (cdr exps) env cont
                                   (append rands (list val))))))))
  (define set-cont
    (lambda(ref cont)
      (lambda(val store)
        (apply-cont cont
                    (an-answer val
                               (store->setref
                                store
                                ref
                                val))))))

  )
