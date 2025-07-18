#lang racket

(require eopl)
(require "data-structures-unit.rkt")
(require "../../common/enironment.rkt")
(require "../procedure-sig.rkt")
(require "store-unit.rkt")
(require "senv-unit.rkt")
(require "operator-functions-unit.rkt")
(require "continuations-sig.rkt")
(require "continuation-interface-sig.rkt")

(provide continuation-dt@)

(define-unit continuation-dt@
  (import data-structures^ proc-def^ store^ senv^ operator-fun^ cont-valueof^ apply-procedure^)
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
    (except-cont
     (ident symbol?)
     (exp2 expression?)
     (env environment?)
     (econt continuation?)
     (cont continuation?))
    (raise-cont
     (exps2 (list-of expression?))
     (env environment?)
     (cont continuation?))
    (raise-cont2
     (val1 expval?)
     (cont continuation?))
    (try-cont
     (econt continuation?)
     (cont continuation?))
    (empty-econt)
    )

  (define make-let-cont-by-op
    (lambda (op vars exps body cont env)
      (if (equal? op "let*")
          (let*-cont vars exps body cont env)
          (let-cont vars exps body cont env env))))
  (define apply-cont
    (lambda (cont econt aw)
      (cases continuation cont
        (if-cont (exp2 exp3 env cont)
                 (let* ((eval (answer->eval aw))
                        (store (answer->store aw))
                        (senv (cons env store)))
                   (if (expval->bool eval)
                       (value-of/k exp2 senv cont econt)
                       (value-of/k exp3 senv cont econt))))
        (let*-cont (vars exps body cont env)
                   (let* ((eval (answer->eval aw))
                          (store (answer->store aw))
                          (new-senv (extend-senv (car vars) eval
                                                 (cons env store))))
                     (if (null? exps)
                         (value-of/k body new-senv cont econt)
                         (value-of/k (car exps)
                                     new-senv
                                     (let*-cont (cdr vars) (cdr exps) body cont
                                                (car new-senv))
                                     econt))))
        (let-cont (vars exps body cont ori-env env)
                  (let* ((eval (answer->eval aw))
                         (store (answer->store aw))
                         (new-senv (extend-senv (car vars) eval
                                                (cons env store))))
                    (if (null? exps)
                        (value-of/k body new-senv cont econt)
                        (value-of/k (car exps)
                                    (cons ori-env (cdr new-senv))
                                    (let-cont (cdr vars) (cdr exps) body cont ori-env
                                              (car new-senv))
                                    econt))))
        (cond-cont (exps1 exps2 env cont)
                   (let* ((eval (answer->eval aw))
                          (store (answer->store aw))
                          (senv (cons env store)))
                     (if (expval->bool eval)
                         (value-of/k (car exps2) senv cont econt)
                         (if (null? exps1)
                             (eopl:error 'cond "None of the tests succeeds!")
                             (value-of/k (car exps1)
                                         senv
                                         (cond-cont (cdr exps1) (cdr exps2) env cont)
                                         econt)))))
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
                                   (apply-cont cont econt
                                               (make-answer ((none-operator op)))))
                          (binary-op (op)
                                     (value-of/k (car rands)
                                                 senv
                                                 (binary-op-cont1 op (cdr rands) env cont)
                                                 econt))
                          (unary-op (op)
                                    (value-of/k (car rands)
                                                senv
                                                (unary-op-cont op cont)
                                                econt))
                          (any-op (op)
                                  (if (null? rands)
                                      (apply-cont cont econt
                                                  (make-answer ((any-operator op) '())))
                                      (value-of/k (car rands)
                                                  senv
                                                  (any-op-cont op (cdr rands) env cont '())
                                                  econt)))
                          ))
                       ((expval->isproc? eval)
                        (let ((proc (expval->proc eval)))
                          (if (null? rands)
                              (apply-procedure/k proc '() senv cont econt)
                              (value-of/k (car rands)
                                          senv
                                          (proc-cont proc (cdr rands) env cont '())
                                          econt))))
                       (else
                        (eopl:error 'call-exp "can not apply on expval ~s" eval))
                       )))
        (binary-op-cont1 (op rands env cont)
                         (let* ((eval (answer->eval aw))
                                (store (answer->store aw))
                                (senv (cons env store)))
                           (value-of/k (car rands)
                                       senv
                                       (binary-op-cont2 op eval cont)
                                       econt)))
        (binary-op-cont2 (op eval1 cont)
                         (if (and (equal? op "/") (zero? (expval->num (answer->eval aw))))
                             (begin
                               (printf "Division by zero! (~s ~s ~s)\n" op eval1 (answer->eval aw))
                               (apply-cont econt econt (an-answer (num-val -1)
                                                                  (answer->store aw))))
                             (apply-cont cont econt
                                         (an-answer
                                          ((binary-operator op) eval1
                                                                (answer->eval aw))
                                          (answer->store aw)))))
        (unary-op-cont (op cont)
                       (apply-cont cont econt
                                   (an-answer
                                    ((unary-operator op) (answer->eval aw))
                                    (answer->store aw))))
        (any-op-cont (op rands env cont vals)
                     (let* ((eval (answer->eval aw))
                            (store (answer->store aw))
                            (new-vals (append vals (list eval))))
                       (if (null? rands)
                           (apply-cont cont econt
                                       (an-answer
                                        ((any-operator op) vals)
                                        store))
                           (value-of/k (car rands)
                                       (cons env store)
                                       (any-op-cont op (cdr rands) env cont new-vals)
                                       econt))))
        (proc-cont (proc rands env cont vals)
                   (let* ((eval (answer->eval aw))
                          (store (answer->store aw))
                          (senv (cons env store))
                          (new-vals (append vals (list eval))))
                     (if (null? rands)
                         (apply-procedure/k proc new-vals senv cont econt)
                         (value-of/k (car rands)
                                     senv
                                     (proc-cont proc (cdr rands) env cont new-vals)
                                     econt))))
        (begin-cont (exps2 env cont)
                    (if (null? exps2)
                        (apply-cont cont econt aw)
                        (value-of/k (car exps2)
                                    (cons env (answer->store aw))
                                    (begin-cont (cdr exps2) env cont)
                                    econt)))
        (set-cont (ref cont)
                  (let* ((eval (answer->eval aw))
                         (store (answer->store aw)))
                    (apply-cont cont econt
                                (an-answer eval
                                           (store->setref
                                            store
                                            ref
                                            eval)))))
        (try-cont (econt cont)
                  (apply-cont cont econt aw))
        (except-cont (ident exp2 env econt cont)
                     (let* ((eval (answer->eval aw))
                            (elist (expval->list eval))
                            (proc-val (explist->car elist))
                            (ncont (if (explist->null? (explist->cdr elist))
                                       cont
                                       (expval->cont (explist->car (explist->cdr elist))))))
                       (apply-procedure/k (procedure (list ident)
                                                     exp2
                                                     env)
                                          (list proc-val)
                                          (cons env (answer->store aw))
                                          ncont
                                          econt)))
        (raise-cont (exps2 env cont)
                    (if (null? exps2)
                        (apply-cont econt econt
                                    (an-answer (list-val (cons-val (answer->eval aw)
                                                                   (empty-list)))
                                               (answer->store aw)))
                        (value-of/k (car exps2)
                                    (cons env (answer->store aw))
                                    (raise-cont2 (answer->eval aw) cont)
                                    econt)))
        (raise-cont2 (val1 cont)
                     (apply-cont econt econt
                                 (an-answer (list-val (cons-val val1
                                                                (cons-val (answer->eval aw)
                                                                          (empty-list))))
                                            (answer->store aw))))
        (end-cont ()
                  (printf "End of Computation. \n")
                  (answer->eval aw))
        (empty-econt ()
                     (printf "Not find exception-cont! \n"))
        )))
  (define get-nextcont
    (lambda (cont)
      (cases continuation cont
        (end-cont () cont)
        (if-cont (exp2 exp3 env cont) cont)
        (let-cont (vars exps body cont _ env) cont)
        (let*-cont (vars exps body cont env) cont)
        (cond-cont (exps1 exps2 env cont) cont)
        (call-cont (rands env cont) cont)
        (binary-op-cont1 (op rands env cont) cont)
        (binary-op-cont2 (op eval cont) cont)
        (unary-op-cont (op cont) cont)
        (any-op-cont (op rands env cont vals) cont)
        (proc-cont (proc rands env cont vals) cont)
        (begin-cont (exps2 env cont) cont)
        (set-cont (ref cont) cont)
        (except-cont (ident exp2 env econt cont) cont)
        (try-cont (econt cont) cont)
        (raise-cont (_ env cont) cont)
        (raise-cont2 (val cont) cont)
        (empty-econt () cont))))
  )
