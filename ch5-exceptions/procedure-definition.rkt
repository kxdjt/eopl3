#lang racket

(require racket/lazy-require)
(lazy-require ["continuation-interface.rkt" (value-of/k)])
(lazy-require ["senv.rkt" (extend-senv*)])

(provide (all-defined-out))

;; Define procedure data type by scheme procedure
(define procedure
  (lambda (vars body env)
    #| (proc-env (extract-freevar-env vars body env (empty-env))) |#
    (lambda (vals senv cont)
      (let* ((store (cdr senv))
             (new-senv (cons env store)))
        (value-of/k body
                    (extend-senv* vars vals new-senv)
                    cont)))))
#| (define traceproc |#
#|   (lambda (vars body env) |#
#|     (let ((env (extract-freevar-env vars body env (empty-env)))) |#
#|       (lambda (vals cont) |#
#|         (printf "tranceproc: vars:~s vals:~s body:~s\n" vars vals body) |#
#|         (let ((res (value-of/k body |#
#|                                (extend-env* vars vals env) |#
#|                                cont))) |#
#|           (printf "tranceproc: res: ~s\n" res) |#
#|           res))))) |#
(define dynamicproc
  (lambda (vars body . _)
    (lambda (vals senv cont)
      (value-of/k body
                  (extend-senv* vars vals senv)
                  cont))))
(define proc?
  (lambda (proc)
    (procedure? proc)))
