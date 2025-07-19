#lang racket

(require eopl)
(require "../../common/utils.rkt")

(require "../../common/enironment.rkt")
(require "data-structures-unit.rkt")
(require "operator-functions-unit.rkt")
(require "senv-unit.rkt")
(require "store-unit.rkt")
(require "continuations-sig.rkt")
(require "continuation-interface-sig.rkt")

(provide value-of/k-imp@)

(define-unit value-of/k-imp@
  (import data-structures^ operator-fun^ senv^ store^ continuation^)
  (export cont-valueof^)
  ;; Exp * Env * Cont * ECont -> Bounce
  (define value-of/k
    (lambda (exp senv cont econt)
      (debug-trace "value-of/k" "exp:~s\n" (exp->fmt exp))
      (let ((res (value-of/k-imp exp senv cont econt)))
        (debug-trace "value-of/k" "res:~s\n" res)
        res)))
  (define value-of/k-imp
    (lambda (exp senv cont econt)
      (let* ((store (cdr senv))
             (env (car senv))
             (make-answer (lambda(eval)
                            (an-answer eval store))))
        (cases expression exp
          (const-exp (num)
                     (apply-cont cont econt
                                 (make-answer
                                  (num-val num))))
          (var-exp (var)
                   (apply-cont cont econt
                               (make-answer
                                (apply-senv senv var))))
          (innerop-exp (inner-op)
                       (apply-cont cont econt
                                   (make-answer
                                    (innerop-val inner-op))))
          (if-exp (exp1 exp2 exp3)
                  (value-of/k exp1 senv
                              (if-cont exp2 exp3 env cont)
                              econt))
          (let-exp (op vars exps body)
                   (if (null? exps)
                       (value-of/k body senv cont econt)
                       (value-of/k (car exps) senv
                                   (make-let-cont-by-op
                                    op vars (cdr exps) body cont env)
                                   econt)))
          (letrec-exp (proc-names list-of-vars exps1 exp2)
                      (value-of/k exp2
                                  (extend-senv-rec* proc-names list-of-vars exps1 senv)
                                  cont
                                  econt))
          (cond-exp (exps1 exps2)
                    (if (null? exps1)
                        (eopl:error 'cond "None of the tests succeeds!")
                        (value-of/k (car exps1)
                                    senv
                                    (cond-cont (cdr exps1) exps2 env cont)
                                    econt)))
          (proc-exp (op vars body)
                    (apply-cont cont econt
                                (make-answer
                                 (proc-val ((proc-operator op) vars body env)))))
          (call-exp (exp1 rands)
                    (value-of/k exp1
                                senv
                                (call-cont rands env cont)
                                econt))
          (begin-exp (exp1 exps2)
                     (value-of/k exp1
                                 senv
                                 (begin-cont exps2 env cont)
                                 econt))
          (set-exp (ident exp1)
                   (value-of/k exp1
                               senv
                               (set-cont (apply-env env ident) cont)
                               econt))
          (try-exp (exp1 ident exp2)
                   (value-of/k exp1
                               senv
                               (try-cont econt cont)
                               (except-cont ident exp2 (car senv) econt cont)))
          (raise-exp (exp1 exps)
                     (value-of/k exp1
                                 senv
                                 (raise-cont exps (car senv) cont)
                                 econt))
          (cur-cont-exp ()
                        (apply-cont cont
                                    econt
                                    (make-answer
                                     (cont-val (get-nextcont cont)))))
          (letcc-exp (ident exp1)
                     (value-of/k exp1
                                 (extend-senv ident
                                              (cont-val cont)
                                              senv)
                                 cont
                                 econt))
          (letcc*-exp (ident exp1)
                      (value-of/k exp1
                                  (extend-senv ident
                                               (proc-val (contproc cont))
                                               senv)
                                  cont
                                  econt))
          (throw-exp (exp1 exp2)
                     (value-of/k exp1
                                 senv
                                 (throw-cont exp2 (car senv) cont)
                                 econt))
          ))))
  (define contproc
    (lambda (save-cont)
      (lambda (vals senv cont econt)
        (let* ((store (cdr senv))
               (aw (an-answer (car vals)
                              store)))
          (apply-cont save-cont econt
                      aw)))))
  )
