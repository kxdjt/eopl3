#lang racket


(require eopl)
(require "../data-structures-unit.rkt")
(require "../../common/enironment.rkt")
(require "../../common/utils.rkt")
(require "../procedure-sig.rkt")
(require "../store-unit.rkt")
(require "../senv-unit.rkt")
(require "operator-functions-unit.rkt")
(require "continuations-sig.rkt")
(require "../continuation-interface-sig.rkt")
(require "scheduler-sig.rkt")
(require "mutex-unit.rkt")
(require "thread-unit.rkt")


(provide continuation-list@)

(define debug-timer
  (make-debug-fun 1))

#| (define make-list-cont |#
#|   (lambda (apply-fun) |#
#|     (lambda (cont . vars) |#
#|       (cons |#
#|        (cons apply-fun vars) |#
#|        cont)))) |#

(define make-list-cont
  (lambda (apply-fun)
    (lambda vars
      (let ((cont (if (null? vars) '() (car vars)))
            (r-vars (if (null? vars) '() (cdr vars))))
        (cons
         (cons apply-fun r-vars)
         cont)))))

(define-unit continuation-list@
  (import data-structures^ proc-def^ store^ senv^ operator-fun^
          cont-valueof^ apply-procedure^ scheduler^ mutex^ thread^)
  (export continuation^)

  (define apply-cont
    (lambda (cont aw)
      (debug-trace "apply-cont" "cont:~s aw:~s\n" cont aw)
      (if (time-expired?)
          (begin
            (place-on-ready-queue!
             (make-thread
              (lambda(store)
                (apply-cont cont (an-answer (answer->eval aw)
                                            store)))))
            (run-next-thread (answer->store aw)))
          (begin
            (debug-timer "time-remain" "~s\n" (get-time-remaining))
            (decrement-timer!)
            (apply (caar cont) aw (cdr cont) (cdar cont))))))

  (define make-let-cont-by-op
    (lambda (op vars exps body cont env)
      (if (equal? op "let*")
          (let*-cont cont vars exps body env)
          (let-cont cont vars exps body env env))))
  (define make-unary-cont-table
    (lambda (default . lst)
      (lambda (op . vars)
        (let ((found (assoc op lst))
              (get-vars (lambda(r)
                          (list-head vars r))))
          #| (printf "unary-op-fun op:~s found:~s vars:~s\n" op found vars) |#
          (if (not found)
              (apply default (car vars) (list op))
              (apply (caddr found) (get-vars (cadr found))))))))
  (define if-cont
    (make-list-cont
     (lambda (aw cont exp2 exp3 env)
       (let* ((eval (answer->eval aw))
              (store (answer->store aw))
              (senv (cons env store)))
         (if (expval->bool eval)
             (value-of/k exp2 senv cont)
             (value-of/k exp3 senv cont))))))
  (define let*-cont
    (make-list-cont
     (lambda (aw cont vars exps body env)
       (let* ((eval (answer->eval aw))
              (store (answer->store aw))
              (new-senv (extend-senv (car vars) eval
                                     (cons env store))))
         (if (null? exps)
             (value-of/k body new-senv cont)
             (value-of/k (car exps)
                         new-senv
                         (let*-cont cont (cdr vars) (cdr exps) body
                                    (car new-senv))))))))
  (define let-cont
    (make-list-cont
     (lambda (aw cont vars exps body ori-env env)
       (let* ((eval (answer->eval aw))
              (store (answer->store aw))
              (new-senv (extend-senv (car vars) eval
                                     (cons env store))))
         (if (null? exps)
             (value-of/k body new-senv cont)
             (value-of/k (car exps)
                         (cons ori-env (cdr new-senv))
                         (let-cont cont (cdr vars) (cdr exps) body ori-env
                                   (car new-senv))))))))
  (define cond-cont
    (make-list-cont
     (lambda (aw cont exps1 exps2 env)
       (let* ((eval (answer->eval aw))
              (store (answer->store aw))
              (senv (cons env store)))
         (if (expval->bool eval)
             (value-of/k (car exps2) senv cont)
             (if (null? exps1)
                 (eopl:error 'cond "None of the tests succeeds!")
                 (value-of/k (car exps1)
                             senv
                             (cond-cont cont (cdr exps1) (cdr exps2) env))))))))
  (define call-cont
    (make-list-cont
     (lambda (aw cont rands env)
       (let* ((eval (answer->eval aw))
              (store (answer->store aw))
              (senv (cons env store))
              (make-answer (lambda(eval)
                             (an-answer eval store))))
         (cond
           ((expval->isinnerop? eval)
            (cases inner-operator (expval->innerop eval)
              (none-op (op)
                       ((none-operator op) store cont))
              (binary-op (op)
                         (value-of/k (car rands)
                                     senv
                                     (binary-op-cont1 cont op (cdr rands) env)))
              (unary-op (op)
                        (value-of/k (car rands)
                                    senv
                                    (make-unary-cont-by-op op cont (car senv))))
              (any-op (op)
                      (if (null? rands)
                          (apply-cont cont
                                      (make-answer ((any-operator op) '())))
                          (value-of/k (car rands)
                                      senv
                                      (any-op-cont cont op (cdr rands) env '()))))
              ))
           ((expval->isproc? eval)
            (let ((proc (expval->proc eval)))
              (if (null? rands)
                  (apply-procedure/k proc '() senv cont)
                  (value-of/k (car rands)
                              senv
                              (proc-cont cont proc (cdr rands) env '())))))
           (else
            (eopl:error 'call-exp "can not apply on expval ~s" eval))
           )))))
  (define binary-op-cont1
    (make-list-cont
     (lambda (aw cont op rands env)
       (let* ((eval (answer->eval aw))
              (store (answer->store aw))
              (senv (cons env store)))
         (value-of/k (car rands)
                     senv
                     (binary-op-cont2 cont op eval))))))
  (define binary-op-cont2
    (make-list-cont
     (lambda (aw cont op eval1)
       (apply-cont cont
                   (an-answer
                    ((binary-operator op) eval1
                                          (answer->eval aw))
                    (answer->store aw))))))
  (define unary-op-cont
    (make-list-cont
     (lambda (aw cont op)
       (apply-cont cont
                   (an-answer
                    ((unary-operator op) (answer->eval aw))
                    (answer->store aw))))))
  (define any-op-cont
    (make-list-cont
     (lambda (aw cont op rands env vals)
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
                         (any-op-cont cont op (cdr rands) env new-vals)))))))
  (define proc-cont
    (make-list-cont
     (lambda (aw cont proc rands env vals)
       (let* ((eval (answer->eval aw))
              (store (answer->store aw))
              (senv (cons env store))
              (new-vals (append vals (list eval))))
         (if (null? rands)
             (apply-procedure/k proc new-vals senv cont)
             (value-of/k (car rands)
                         senv
                         (proc-cont cont proc (cdr rands) env new-vals)))))))
  (define begin-cont
    (make-list-cont
     (lambda (aw cont exps2 env)
       (if (null? exps2)
           (apply-cont cont aw)
           (value-of/k (car exps2)
                       (cons env (answer->store aw))
                       (begin-cont cont (cdr exps2) env))))))
  (define set-cont
    (make-list-cont
     (lambda (aw cont ref)
       (let* ((eval (answer->eval aw))
              (store (answer->store aw)))
         (debug-notice "set-cont" "ref:~s eval:~s\n" ref eval)
         (apply-cont cont
                     (an-answer eval
                                (store->setref
                                 store
                                 ref
                                 eval)))))))
  (define spawn-cont
    (make-list-cont
     (lambda (aw cont env)
       (let* ((proc (expval->proc (answer->eval aw)))
              (thread (new-thread
                       (lambda(store)
                         (apply-procedure/k proc
                                            (list (num-val (get-cur-thread-id)))
                                            (cons env store)
                                            (end-subthread-cont))))))
         (place-on-ready-queue!
          thread)
         (apply-cont cont (an-answer (num-val (get-thread-id thread))
                                     (answer->store aw)))))))
  (define kill-cont
    (make-list-cont
     (lambda(aw cont)
       (let ((th-id (expval->num (answer->eval aw))))
         (debug-thread "kill" "thid:~s cur-thid:~s\n" th-id (get-cur-thread-id))
         (if (equal? th-id (get-cur-thread-id))
             (run-next-thread (answer->store aw))
             (apply-cont cont (an-answer (num-val 30)
                                         (remove-thread th-id (answer->store aw)))))))))
  (define end-cont
    (make-list-cont
     (lambda (aw cont)
       (printf "End of Computation. \n")
       (answer->eval aw))))
  (define end-subthread-cont
    (make-list-cont
     (lambda (aw cont)
       (debug-thread "end-subthread" "thid:~s\n" (get-cur-thread-id))
       (run-next-thread (answer->store aw)))))
  (define end-main-thread-cont
    (make-list-cont
     (lambda (aw cont)
       (debug-thread "end-mainthread" "thid:~s\n" (get-cur-thread-id))
       (set-final-answer! aw)
       (run-next-thread (answer->store aw)))))
  (define wait-cont
    (make-list-cont
     (lambda (aw cont)
       (wait-for-mutex
        (answer->eval aw)
        (answer->store aw)
        (lambda (store)
          (apply-cont cont (an-answer
                            (num-val 53)
                            store)))))))
  (define signal-cont
    (make-list-cont
     (lambda (aw cont)
       (signal-mutex
        (answer->eval aw)
        (answer->store aw)
        (lambda(store)
          (apply-cont cont (an-answer
                            (num-val 54)
                            store)))))))
  (define wait-for-mutex
    (lambda (mutex store cont-fun)
      (if (mutex->isclose? mutex store)
          (run-next-thread
           (answer->store
            (mutex->place-on-wait-queue! mutex store (make-thread cont-fun))))
          (cont-fun
           (answer->store
            (mutex->close mutex store))))))
  (define signal-mutex
    (lambda (mutex store cont-fun)
      (cont-fun
       (answer->store
        (if (mutex->waitq-empty? mutex store)
            (mutex->open mutex store)
            (mutex->waitq-dequeue mutex store
                                  (lambda (thread)
                                    (place-on-ready-queue! thread))))))))

  (define make-unary-cont-by-op
    (make-unary-cont-table
     unary-op-cont
     (list "wait" 1 wait-cont)
     (list "signal" 1 signal-cont)
     (list "spawn" 2 spawn-cont)
     (list "kill" 1 kill-cont)
     ))

  )

