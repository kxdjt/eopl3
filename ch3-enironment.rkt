#lang racket

(provide empty-env extend-env extend-env* apply-env has-binding?)

(define empty-env
  (lambda ()
    (cons
     (lambda (var)
       (error 'apply-env
              "No binding for ~s" var))
     (lambda (var)
       #f))))
(define extend-env
  (lambda (var val env)
    (cons
     (lambda (search-var)
       (if (equal? search-var var)
           val
           (apply-env env search-var)))
     (lambda (search-var)
       (if (equal? search-var var)
           #t
           (has-binding? env search-var))))))
(define extend-env*
  (lambda (vars vals env)
    (let ((make-ob-action
           (lambda (observer ret-fun)
             (lambda (search-var)
               (let ((res (ormap (lambda (var val)
                                   (if (equal? var search-var)
                                       val
                                       #f))
                                 vars vals)))
                 (if (not res)
                     (observer env search-var)
                     (ret-fun res)))))))
      (cons
       (make-ob-action apply-env (lambda (res) res))
       (make-ob-action has-binding? (lambda (_) #t))))))

(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))
(define has-binding?
  (lambda (env search-var)
    ((cdr env) search-var)))





