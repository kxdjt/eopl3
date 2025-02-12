#lang racket
(require (except-in eopl
                    list-of))
(require "ch2-datatype-utils.rkt")

(define identifier?
  (lambda (var)
    (if (not (symbol? var)) #f
        (if (equal? var 'lambda) #f
            #t))))
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))))
(define parse-expression
  (lambda (datum)
    (cond
      ((identifier? datum)
       (var-exp datum))
      ((pair? datum)
       (cond
         ((and (equal? (car datum) 'lambda)
               (not (null? (cdr datum)))
               (list? (cadr datum))
               (not (null? (cddr datum))))
          (lambda-exp (cadr datum)
                      (parse-expression (caddr datum))))
         ((and (not (null? (cdr datum)))
               (list? (cadr datum)))
          (app-exp (parse-expression (car datum))
                   (for-each-list (cadr datum)
                                  (lambda (var)
                                    (parse-expression var)))))
         (else
          (error 'parse-expression
                 "~s is invalid form" datum)))))))
(define lc-exp-list
  (list 'lambda (list 'a 'b 'c) (list 'x (list 'd 'e (list 'lambda '() 'z)))))
(parse-expression lc-exp-list)
(parse-expression '(lambda))
(parse-expression '(a b c))
