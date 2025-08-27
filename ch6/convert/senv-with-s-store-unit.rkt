#lang racket

(require "./senv-sig.rkt"
         "./store-s-unit.rkt"
         "./data-structures-unit.rkt"
         "./procedure-sig.rkt"
         "../../common/enironment.rkt"
         "../../common/utils.rkt")

(provide senv^ senv-s@)

(define-unit senv-s@
  (import s-store^ data-structures^ proc-def^)
  (export senv^)

  (define empty-senv
    (lambda()
      (initialize-store!)
      (empty-env)))
  (define extend-senv
    (lambda(var eval senv)
      (debug-trace "extend-senv"
                   "var:~s eval:~s\n"
                   var eval)
      (extend-env var (newref eval)
                  senv)))
  (define extend-senv*
    (lambda (vars evals senv)
      (if (null? vars)
          senv
          (extend-senv* (cdr vars) (cdr evals)
                        (extend-senv (car vars)
                                     (car evals)
                                     senv)))))
  (define apply-senv
    (lambda (senv var)
      (debug-trace "apply-senv"
                   "var:~s\n" var)
      (let ((res
             (deref
              (apply-env senv var))))
        (debug-trace "apply-senv"
                     "res:~s\n"
                     res)
        res)))
  (define extend-senv-rec*
    (lambda (proc-names list-of-vars exps senv)
      (define make-env
        (lambda (vars s-ref env)
          (if (null? vars)
              env
              (make-env (cdr vars) (+ s-ref 1)
                        (extend-env (car vars) s-ref env)))))
      (define make-store
        (lambda (lvars exps env)
          (if (null? exps)
              env
              (begin
                (newref
                 (proc-val
                  (procedure (car lvars)
                             (car exps)
                             env)))
                (make-store (cdr lvars)
                            (cdr exps)
                            env
                            )))))
      (let ((start-ref (nextref)))
        (make-store list-of-vars exps
                    (make-env proc-names start-ref senv)))))
  )
