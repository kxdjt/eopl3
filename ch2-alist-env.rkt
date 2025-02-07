#lang racket
(define empty-env
  (lambda ()
    '()))
(define extend-env
  (lambda (var val env)
    (cons (list var val) env)))
(define apply-env-imp
  (lambda (env search-var err-msg)
    (cond
      ((null? env)
       (err-msg search-var))
      ((equal? (caar env) search-var)
       (cadar env))
      (else
       (apply-env-imp (cdr env) search-var err-msg)))))
(define apply-env
  (lambda (env search-var)
    (define report-no-binding-found
      (lambda (var)
        (error 'apply-env
               "No binding for ~s in ~s" var env)))
    (apply-env-imp env search-var report-no-binding-found)))
(define e
  (extend-env 'd 6
              (extend-env 'y 8
                          (extend-env 'x 7
                                      (extend-env 'y 14
                                                  (empty-env))))))
#| (apply-env e 'y) |#
#| (apply-env e 'z) |#
(define empty-env?
  (lambda (env)
    (null? env)))
#| (empty-env? (empty-env)) |#
#| (empty-env? e) |#
(define has-binding?
  (lambda (env s)
    (cond
      ((empty-env? env) #f)
      ((equal? (caar env) s) #t)
      (else
       (has-binding? (cdr env) s)))))
#| (has-binding? e 'y) |#
#| (has-binding? e 'z) |#
(define extend-env*
  (lambda (vars vals env)
    (if (null? vars)
        env
        (extend-env* (cdr vars) (cdr vals)
                     (extend-env (car vars) (car vals) env)))))
(extend-env* '(v1 v2 v3) '(1 2 3) e)
