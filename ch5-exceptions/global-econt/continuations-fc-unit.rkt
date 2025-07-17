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

(provide continuation-fc@)

(define try-cont-list '())

(define-unit continuation-fc@
  (import data-structures^ proc-def^ store^ senv^ operator-fun^ cont-valueof^ apply-procedure^)
  (export continuation^)

  ;; procedure representation of continuations
  (define if-cont
    (lambda (exp2 exp3 env cont)
      (lambda (aw)
        (let* ((eval (answer->eval aw))
               (store (answer->store aw))
               (senv (cons env store)))
          (if (expval->bool eval)
              (value-of/k exp2 senv cont)
              (value-of/k exp3 senv cont))))))
  (define let*-cont
    (lambda (vars exps body cont env)
      (lambda (aw)
        (let* ((eval (answer->eval aw))
               (store (answer->store aw))
               (new-senv (extend-senv (car vars) eval
                                      (cons env store))))
          (if (null? exps)
              (value-of/k body new-senv cont)
              (value-of/k (car exps)
                          new-senv
                          (let*-cont (cdr vars) (cdr exps) body cont
                                     (car new-senv))))))))
  (define let-cont
    (lambda (vars exps body cont ori-env env)
      (lambda (aw)
        (debug-trace "let-cont" "start!")
        (let* ((eval (answer->eval aw))
               (store (answer->store aw))
               (new-senv (extend-senv (car vars) eval
                                      (cons env store))))
          (if (null? exps)
              (value-of/k body new-senv cont)
              (value-of/k (car exps)
                          (cons ori-env (cdr new-senv))
                          (let-cont (cdr vars) (cdr exps) body cont ori-env
                                    (car new-senv))))))))
  (define cond-cont
    (lambda (exps1 exps2 env cont)
      (lambda (aw)
        (let* ((eval (answer->eval aw))
               (store (answer->store aw))
               (senv (cons env store)))
          (if (expval->bool eval)
              (value-of/k (car exps2) senv cont)
              (if (null? exps1)
                  (eopl:error 'cond "None of the tests succeeds!")
                  (value-of/k (car exps1)
                              senv
                              (cond-cont (cdr exps1) (cdr exps2) env cont))))))))
  (define call-cont
    (lambda (rands env cont)
      (lambda (aw)
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
            )))))
  (define binary-op-cont1
    (lambda (op rands env cont)
      (lambda (aw)
        (let* ((eval (answer->eval aw))
               (store (answer->store aw))
               (senv (cons env store)))
          (value-of/k (car rands)
                      senv
                      (binary-op-cont2 op eval cont))))))
  (define binary-op-cont2
    (lambda (op eval1 cont)
      (lambda (aw)
        (apply-cont cont
                    (an-answer
                     ((binary-operator op) eval1
                                           (answer->eval aw))
                     (answer->store aw))))))
  (define unary-op-cont
    (lambda (op cont)
      (lambda (aw)
        (apply-cont cont
                    (an-answer
                     ((unary-operator op) (answer->eval aw))
                     (answer->store aw))))))
  (define any-op-cont
    (lambda (op rands env cont vals)
      (lambda (aw)
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
                          (any-op-cont op (cdr rands) env cont new-vals)))))))
  (define proc-cont
    (lambda (proc rands env cont vals)
      (lambda (aw)
        (let* ((eval (answer->eval aw))
               (store (answer->store aw))
               (senv (cons env store))
               (new-vals (append vals (list eval))))
          (if (null? rands)
              (apply-procedure/k proc new-vals senv cont)
              (value-of/k (car rands)
                          senv
                          (proc-cont proc (cdr rands) env cont new-vals)))))))
  (define begin-cont
    (lambda (exps2 env cont)
      (lambda (aw)
        (if (null? exps2)
            (apply-cont cont aw)
            (value-of/k (car exps2)
                        (cons env (answer->store aw))
                        (begin-cont (cdr exps2) env cont))))))
  (define make-let-cont-by-op
    (lambda (op vars exps body cont env)
      (if (equal? op "let*")
          (let*-cont vars exps body cont env)
          (let-cont vars exps body cont env env))))
  (define set-cont
    (lambda (ref cont)
      (lambda (aw)
        (let* ((eval (answer->eval aw))
               (store (answer->store aw)))
          (apply-cont cont
                      (an-answer eval
                                 (store->setref
                                  store
                                  ref
                                  eval)))))))
  (define end-cont
    (lambda ()
      (lambda (aw)
        (printf "End of Computation. \n")
        (answer->eval aw))))
  (define try-cont
    (lambda (cont)
      (lambda (aw)
        (apply-cont cont aw))))
  (define raise-cont
    (lambda (cont)
      (lambda (aw)
        (apply-handler cont aw))))
  (define exception-cont
    (lambda (ident exp2 env cont)
      (lambda (aw)
        (apply-procedure/k (procedure (list ident)
                                      exp2
                                      env)
                           (list (answer->eval aw))
                           (cons env (answer->store aw))
                           cont))))
  (define try-handler
    (lambda (ident exp2 env cont)
      (let ((e-cont (exception-cont ident exp2 env cont)))
        (set! try-cont-list (cons e-cont try-cont-list))
        (try-cont cont))))

  (define apply-handler
    (lambda (cont aw)
      (debug-trace "apply-handler" "cont:~s\n try-cont-list:~s\n" cont try-cont-list)
      (if (null? try-cont-list)
          (eopl:error 'apply-handler "Not found try-cont!")
          (let ((t-cont (car try-cont-list)))
            (set! try-cont-list (cdr try-cont-list))
            (apply-cont t-cont aw)))))
  (define apply-cont
    (lambda (cont aw)
      (cont aw)))
  )
