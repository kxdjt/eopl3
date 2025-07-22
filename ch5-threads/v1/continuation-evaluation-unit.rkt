#lang racket

(require eopl)
(require "../../common/utils.rkt")

(require "../../common/enironment.rkt")
(require "../data-structures-unit.rkt")
(require "../operator-functions-unit.rkt")
(require "../senv-unit.rkt")
(require "../store-unit.rkt")
(require "continuations-sig.rkt")
(require "continuation-interface-sig.rkt")

#| (provide value-of/k) |#
(provide (all-defined-out))

(define-unit value-of/k-imp@
  (import data-structures^ operator-fun^ senv^ store^ continuation^)
  (export cont-valueof^)

  ;; Exp * Env * Cont -> Bounce
  (define value-of/k
    (lambda (exp senv cont)
      (debug-trace "value-of/k" "exp:~s\n" (exp->fmt exp))
      (let ((res (value-of/k-imp exp senv cont)))
        (debug-trace "value-of/k" "res:~s\n" res)
        res)))
  (define value-of/k-imp
    (lambda (exp senv cont)
      (let* ((store (cdr senv))
             (env (car senv))
             (make-answer (lambda(eval)
                            (an-answer eval store))))
        (cases expression exp
          (const-exp (num)
                     (apply-cont cont
                                 (make-answer
                                  (num-val num))))
          (var-exp (var)
                   #| (printf "var ~s ~s ~s\n" var (apply-env (car senv) var) (apply-senv senv var)) |#
                   (apply-cont cont
                               (make-answer
                                (apply-senv senv var))))
          (innerop-exp (inner-op)
                       (apply-cont cont
                                   (make-answer
                                    (innerop-val inner-op))))
          (if-exp (exp1 exp2 exp3)
                  (value-of/k exp1 senv
                              (if-cont exp2 exp3 env cont)))
          (let-exp (op vars exps body)
                   (if (null? exps)
                       (value-of/k body senv cont)
                       (value-of/k (car exps) senv
                                   (make-let-cont-by-op
                                    op vars (cdr exps) body cont env))))
          (letrec-exp (proc-names list-of-vars exps1 exp2)
                      (value-of/k exp2
                                  (extend-senv-rec* proc-names list-of-vars exps1 senv)
                                  cont))
          (cond-exp (exps1 exps2)
                    (if (null? exps1)
                        (eopl:error 'cond "None of the tests succeeds!")
                        (value-of/k (car exps1)
                                    senv
                                    (cond-cont (cdr exps1) exps2 env cont))))
          (proc-exp (op vars body)
                    (apply-cont cont
                                (make-answer
                                 (proc-val ((proc-operator op) vars body env)))))
          (call-exp (exp1 rands)
                    (value-of/k exp1
                                senv
                                (call-cont rands env cont)))
          (begin-exp (exp1 exps2)
                     (value-of/k exp1
                                 senv
                                 (begin-cont exps2 env cont)))
          (set-exp (ident exp1)
                   #| (printf "set ident:~s ref:~s exp1:~s\n" ident |#
                   #|         (apply-env env ident) exp1) |#
                   (value-of/k exp1
                               senv
                               (set-cont (apply-env env ident) cont)))
          (spawn-exp (exp1)
                     (value-of/k exp1
                                 senv
                                 (spawn-cont (car senv) cont)))
          ))))
  )
