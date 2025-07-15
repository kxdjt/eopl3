#lang racket

(require "../common/enironment.rkt")
(require "store.rkt")
(require "data-structures.rkt")

(provide (all-defined-out))
(provide (all-from-out "../common/enironment.rkt"))
(provide (all-from-out "store.rkt"))

;; Define senv -- (env . store)
(define empty-senv
  (lambda ()
    (cons (empty-env) (empty-store))))
;; var*expval*(env . store) -> (newenv . newstore)
(define extend-senv
  (lambda (var eval senv)
    (let* ((env (car senv))
           (store (cdr senv))
           (ref (store->nextref store)))
      (cons
       (extend-env var ref env)
       (extend-store ref eval store)))))
;; vars**expval*(env . store) -> (newenv . newstore)
(define extend-senv*
  (lambda (vars evals senv)
    (if (null? vars)
        senv
        (extend-senv* (cdr vars) (cdr evals)
                      (extend-senv (car vars)
                                   (car evals)
                                   senv)))))
;; (env . store)*var -> val
(define apply-senv
  (lambda (senv var)
    (let* ((env (car senv))
           (store (cdr senv))
           (ref (apply-env env var)))
      (store->findref store ref)
      )))

(define extend-senv-rec*
  (lambda (proc-names list-of-vars exps senv)
    (define make-env
      (lambda (vars s-ref env)
        (if (null? vars)
            env
            (make-env (cdr vars) (+ s-ref 1)
                      (extend-env (car vars) s-ref env)))))
    (define make-store
      (lambda (lvars exps s-ref env store)
        (if (null? exps)
            store
            (make-store (cdr lvars)
                        (cdr exps)
                        (+ s-ref 1)
                        env
                        (extend-store s-ref
                                      (proc-val
                                       (procedure (car lvars)
                                                  (car exps)
                                                  env))
                                      store)))))
    (let* ((env (car senv))
           (store (cdr senv))
           (start-ref (store->nextref store))
           (nenv (make-env proc-names start-ref env))
           (nstore (make-store list-of-vars exps start-ref nenv store)))
      (cons nenv nstore))))
