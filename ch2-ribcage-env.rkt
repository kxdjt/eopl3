#lang racket

(provide empty-env empty-env? extend-env apply-env)

(define empty-env
  (lambda ()
    '()))
(define empty-env?
  (lambda (env)
    (null? env)))
(define extend-env
  (lambda (var val env)
    (cons (list (list var) (list val)) env)))
(define apply-env-imp
  (lambda (env search-var err-msg)
    (cond
      ((empty-env? env)
       (err-msg search-var))
      ((has-binding-in-ribs? (car env) search-var)
       (apply-env-in-ribs (car env) search-var))
      (else
       (apply-env-imp (cdr env) search-var err-msg)))))
(define has-binding-in-ribs?
  (lambda (ribs search-var)
    (if (null? (car ribs))
        #f
        (if (equal? (caar ribs) search-var)
            #t
            (has-binding-in-ribs? (list (cdar ribs) (cdadr ribs)) search-var)))))
(define apply-env-in-ribs
  (lambda (ribs search-var)
    (if (equal? (caar ribs) search-var)
        (caadr ribs)
        (apply-env-in-ribs (list (cdar ribs) (cdadr ribs)) search-var))))
(define apply-env
  (lambda (env search-var)
    (define report-no-binding-found
      (lambda (var)
        (error 'apply-env
               "No binding for ~s in ~s" var env)))
    (apply-env-imp env search-var report-no-binding-found)))
(define has-binding?
  (lambda (env s)
    (cond
      ((empty-env? env) #f)
      ((has-binding-in-ribs? (car env) s) #t)
      (else
       (has-binding? (cdr env) s)))))
(define extend-env*
  (lambda (vars vals env)
    (cons (list vars vals) env)))
#| (define e |#
#|   (extend-env 'd 6 |#
#|               (extend-env 'y 8 |#
#|                           (extend-env 'x 7 |#
#|                                       (extend-env 'y 14 |#
#|                                                   (empty-env)))))) |#
#| (apply-env e 'y) |#
#| #| (apply-env e 'z) |# |#
#| (has-binding? e 'y) |#
#| (has-binding? e 'z) |#
#| (extend-env* '(v1 v2 v3) '(1 2 3) e) |#
