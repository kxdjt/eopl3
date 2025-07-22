#lang racket

(require eopl)
(require "../data-structures-unit.rkt")
(require "../../common/enironment.rkt")
(require "../../common/utils.rkt")
(require "../procedure-sig.rkt")
(require "../store-unit.rkt")
(require "../senv-unit.rkt")
(require "../operator-functions-unit.rkt")
(require "continuations-sig.rkt")
(require "continuation-interface-sig.rkt")
(require "scheduler-sig.rkt")

(provide continuation-dt@)


(define-unit continuation-dt@
  (import data-structures^ proc-def^ store^ senv^ operator-fun^ cont-valueof^ apply-procedure^ scheduler^)
  (export continuation^)

  ;; data representation of continuations
  (define-datatype continuation continuation?
    (end-cont)
    (if-cont
     (exp2 expression?)
     (exp3 expression?)
     (env environment?)
     (cont continuation?))
    (let-cont
     (vars (list-of symbol?))
     (exps (list-of expression?))
     (body expression?)
     (cont continuation?)
     (ori-env environment?)
     (env environment?))
    (let*-cont
     (vars (list-of symbol?))
     (exps (list-of expression?))
     (body expression?)
     (cont continuation?)
     (env environment?))
    (cond-cont
     (exps1 (list-of expression?))
     (exps2 (list-of expression?))
     (env environment?)
     (cont continuation?))
    (call-cont
     (rands (list-of expression?))
     (env environment?)
     (cont continuation?))
    (binary-op-cont1
     (op string?)
     (rands (list-of expression?))
     (env environment?)
     (cont continuation?))
    (binary-op-cont2
     (op string?)
     (eval expval?)
     (cont continuation?))
    (unary-op-cont
     (op string?)
     (cont continuation?))
    (any-op-cont
     (op string?)
     (rands (list-of expression?))
     (env environment?)
     (cont continuation?)
     (vals (list-of expval?)))
    (proc-cont
     (proc proc?)
     (rands (list-of expression?))
     (env environment?)
     (cont continuation?)
     (vals (list-of expval?)))
    (begin-cont
      (exps2 (list-of expression?))
      (env environment?)
      (cont continuation?))
    (set-cont
     (ref number?)
     (cont continuation?))
    (spawn-cont
     (env environment?)
     (cont continuation?))
    (end-subthread-cont)
    (end-main-thread-cont)
    )

  (define make-let-cont-by-op
    (lambda (op vars exps body cont env)
      (if (equal? op "let*")
          (let*-cont vars exps body cont env)
          (let-cont vars exps body cont env env))))
  (define apply-cont
    (lambda (cont aw)
      (if (time-expired?)
          (begin
            (place-on-ready-queue!
             (lambda(store)
               (apply-cont cont (an-answer (answer->eval aw)
                                           store))))
            (run-next-thread (answer->store aw)))
          (begin
            (decrement-timer!)
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
                           (cond
                             ((expval->isinnerop? eval)
                              (cases inner-operator (expval->innerop eval)
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
                             ((expval->isproc? eval)
                              (let ((proc (expval->proc eval)))
                                (if (null? rands)
                                    (apply-procedure/k proc '() senv cont)
                                    (value-of/k (car rands)
                                                senv
                                                (proc-cont proc (cdr rands) env cont '())))))
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
              (spawn-cont (env cont)
                          (let ((proc (expval->proc (answer->eval aw))))
                            (place-on-ready-queue!
                             (lambda(store)
                               (apply-procedure/k proc
                                                  (list (num-val 28))
                                                  (cons env store)
                                                  (end-subthread-cont))))
                            (apply-cont cont (an-answer (num-val 73)
                                                        (answer->store aw)))))

              (end-cont ()
                        (printf "End of Computation. \n")
                        (answer->eval aw))
              (end-subthread-cont ()
                                  (run-next-thread (answer->store aw)))
              (end-main-thread-cont ()
                                    (set-final-answer! aw)
                                    (run-next-thread (answer->store aw)))
              )))))

  )
