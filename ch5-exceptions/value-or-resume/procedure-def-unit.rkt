#lang racket

(require "../procedure-sig.rkt")
(require "senv-unit.rkt")
(require "continuation-interface-sig.rkt")
(require "continuations-sig.rkt")
(require "store-unit.rkt")
(require "data-structures-unit.rkt")

(provide proc-def@)

(define-unit proc-def@
  (import senv^ cont-valueof^ continuation^ store^ data-structures^)
  (export proc-def^)

  ;; Define procedure data type by scheme procedure
  (define contproc
    (lambda (save-cont)
      (lambda (vals senv cont econt)
        (let* ((store (cdr senv))
               (aw (an-answer (car vals)
                              store)))
          (apply-cont save-cont econt
                      aw)))))
  (define procedure
    (lambda (vars body env)
      #| (proc-env (extract-freevar-env vars body env (empty-env))) |#
      (lambda (vals senv cont econt)
        (let* ((store (cdr senv))
               (new-senv (cons env store)))
          (if (equal? (length vars) (length vals))
              (value-of/k body
                          (extend-senv* vars vals new-senv)
                          cont
                          econt)
              (begin
                (printf "Call with the wrong number of arguments! vars:~s vals:~s \n" vars vals)
                (apply-cont econt
                            econt
                            (an-answer (num-val -1)
                                       store))
                ))))))
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
      (lambda (vals senv cont econt)
        (value-of/k body
                    (extend-senv* vars vals senv)
                    cont
                    econt))))
  (define proc?
    (lambda (proc)
      (procedure? proc)))
  )

#| (require racket/lazy-require) |#
#| (lazy-require ["continuation-interface.rkt" (value-of/k)]) |#


