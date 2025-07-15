#lang racket

(require eopl)
(require "data-structures.rkt")
(require "continuations-by-dt.rkt")
(require "operator-functions.rkt")
(require "senv.rkt")
(require "../common/utils.rkt")
(require racket/lazy-require)
(lazy-require ["continuation-interface.rkt" (value-of/k)])
(lazy-require ["continuation-interface.rkt" (apply-procedure/k)])

(provide (all-defined-out))

(define apply-cont
  (lambda (cont aw)
    (cases continuation cont
      (if-cont (exp2 exp3 env cont)
               (let* ((eval (answer->eval aw))
                      (store (answer->store aw))
                      (senv (cons env store)))
                 (if (expval->bool eval)
                     (value-of/k exp2 senv cont)
                     (value-of/k exp3 senv cont))))
      (let*-cont (vars exps body cont env)
                 (let* ((eval (answer->eval aw))
                        (store (answer->store aw))
                        (new-senv (extend-senv (car vars) eval
                                               (cons env store))))
                   (if (null? exps)
                       (value-of/k body new-senv cont)
                       (value-of/k (car exps)
                                   new-senv
                                   (let*-cont (cdr vars) (cdr exps) body cont
                                              (car new-senv))))))
      (let-cont (vars exps body cont ori-env env)
                (let* ((eval (answer->eval aw))
                       (store (answer->store aw))
                       (new-senv (extend-senv (car vars) eval
                                              (cons env store))))
                  (if (null? exps)
                      (value-of/k body new-senv cont)
                      (value-of/k (car exps)
                                  (cons ori-env (cdr new-senv))
                                  (let-cont (cdr vars) (cdr exps) body cont ori-env
                                            (car new-senv))))))
      (cond-cont (exps1 exps2 env cont)
                 (let* ((eval (answer->eval aw))
                        (store (answer->store aw))
                        (senv (cons env store)))
                   (if (expval->bool eval)
                       (value-of/k (car exps2) senv cont)
                       (if (null? exps1)
                           (eopl:error 'cond "None of the tests succeeds!")
                           (value-of/k (car exps1)
                                       senv
                                       (cond-cont (cdr exps1) (cdr exps2) env cont))))))
      (call-cont (rands env cont)
                 (let* ((eval (answer->eval aw))
                        (store (answer->store aw))
                        (senv (cons env store))
                        (make-answer (lambda(eval)
                                       (an-answer eval store))))
                   (cases expval eval
                     (innerop-val (innerop)
                                  (cases inner-operator innerop
                                    (none-op (op)
                                             (apply-cont cont
                                                         (make-answer ((none-operator op)))))
                                    (binary-op (op)
                                               (value-of/k (car rands)
                                                           senv
                                                           (binary-op-cont1 op (cdr rands) env cont)))
                                    (unary-op (op)
                                              (value-of/k (car rands)
                                                          senv
                                                          (unary-op-cont op cont)))
                                    (any-op (op)
                                            (if (null? rands)
                                                (apply-cont cont
                                                            (make-answer ((any-operator op) '())))
                                                (value-of/k (car rands)
                                                            senv
                                                            (any-op-cont op (cdr rands) env cont '()))))
                                    ))
                     (proc-val (proc)
                               (if (null? rands)
                                   (apply-procedure/k proc '() senv cont)
                                   (value-of/k (car rands)
                                               senv
                                               (proc-cont proc (cdr rands) env cont '()))))
                     (else
                      (eopl:error 'call-exp "can not apply on expval ~s" eval))
                     )))
      (binary-op-cont1 (op rands env cont)
                       (let* ((eval (answer->eval aw))
                              (store (answer->store aw))
                              (senv (cons env store)))
                         (value-of/k (car rands)
                                     senv
                                     (binary-op-cont2 op eval cont))))
      (binary-op-cont2 (op eval1 cont)
                       (apply-cont cont
                                   (an-answer
                                    ((binary-operator op) eval1
                                                          (answer->eval aw))
                                    (answer->store aw))))
      (unary-op-cont (op cont)
                     (apply-cont cont
                                 (an-answer
                                  ((unary-operator op) (answer->eval aw))
                                  (answer->store aw))))
      (any-op-cont (op rands env cont vals)
                   (let* ((eval (answer->eval aw))
                          (store (answer->store aw))
                          (new-vals (append vals (list eval))))
                     (if (null? rands)
                         (apply-cont cont
                                     (an-answer
                                      ((any-operator op) vals)
                                      store))
                         (value-of/k (car rands)
                                     (cons env store)
                                     (any-op-cont op (cdr rands) env cont new-vals)))))
      (proc-cont (proc rands env cont vals)
                 (let* ((eval (answer->eval aw))
                        (store (answer->store aw))
                        (senv (cons env store))
                        (new-vals (append vals (list eval))))
                   (if (null? rands)
                       (apply-procedure/k proc new-vals senv cont)
                       (value-of/k (car rands)
                                   senv
                                   (proc-cont proc (cdr rands) env cont new-vals)))))
      (begin-cont (exps2 env cont)
                  (if (null? exps2)
                      (apply-cont cont aw)
                      (value-of/k (car exps2)
                                  (cons env (answer->store aw))
                                  (begin-cont (cdr exps2) env cont))))
      (set-cont (ref cont)
                (let* ((eval (answer->eval aw))
                       (store (answer->store aw)))
                  (apply-cont cont
                              (an-answer eval
                                         (store->setref
                                          store
                                          ref
                                          eval)))))
      (try-cont (ident exp2 env cont)
                (apply-cont cont aw))
      (raise-cont (cont)
                  (apply-handler cont aw))
      (end-cont ()
                (printf "End of Computation. \n")
                (answer->eval aw))
      )))

(define apply-handler
  (lambda (cont aw)
    (debug-trace "apply-handler" "cont:~s\n" cont)
    (cases continuation cont
      (try-cont (ident exp2 env cont)
                (apply-procedure/k (procedure (list ident)
                                              exp2
                                              env)
                                   (list (answer->eval aw))
                                   (cons env (answer->store aw))
                                   cont))
      (else
       (apply-handler (cont->nextcont cont) aw)))))
