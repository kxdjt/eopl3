#lang racket
(define var-exp
  (lambda (var)
    (list 'vexp var)))
(define lambda-exp
  (lambda (var exp)
    (list 'lambda var exp)))
(define app-exp
  (lambda (exp1 exp2)
    (list 'aexp exp1 exp2)))
(define var-exp?
  (lambda (exp)
    (equal? (car exp) 'vexp)))
(define lambda-exp?
  (lambda (exp)
    (equal? (car exp) 'lambda)))
(define app-exp?
  (lambda (exp)
    (equal? (car exp) 'aexp)))
(define var-exp->var
  (lambda (exp)
    (cadr exp)))
(define lambda-exp->bound-var
  (lambda (exp)
    (cadr exp)))
(define lambda-exp->body
  (lambda (exp)
    (caddr exp)))
(define app-exp->rator
  (lambda (exp)
    (cadr exp)))
(define app-exp->rand
  (lambda (exp)
    (caddr exp)))
(define e
  (lambda-exp 'x (app-exp (var-exp 'y)
                          (lambda-exp 'z (var-exp 'w)))))
e
(lambda-exp->bound-var e)
(lambda-exp->body e)
(app-exp->rator (lambda-exp->body e))
(app-exp->rand (lambda-exp->body e))
(app-exp? (lambda-exp->body e))
(var-exp? (app-exp->rator (lambda-exp->body e)))
(lambda-exp? (app-exp->rand (lambda-exp->body e)))
