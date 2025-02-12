#lang racket
(define var-exp
  (lambda (var)
    var))
(define lambda-exp
  (lambda (var exp)
    (list 'lambda (list var) exp)))
(define app-exp
  (lambda (exp1 exp2)
    (list exp1 exp2)))
(define var-exp?
  (lambda (exp)
    (if (list? exp) #f
        #t)))
(define lambda-exp?
  (lambda (exp)
    (cond
      ((not (list? exp)) #f)
      ((null? exp) #f)
      ((list? (car exp)) #f)
      ((not (equal? (car exp) 'lambda)) #f)
      ((null? (cdr exp)) #f)
      ((null? (cadr exp)) #f)
      ((not (var-exp? (caadr exp))) #f)
      ((null? (cddr exp)) #f)
      (else
       #t))))
(define app-exp?
  (lambda (exp)
    (cond
      ((not (list? exp)) #f)
      ((null? exp) #f)
      ((null? (cdr exp)) #f)
      ((not (null? (cddr exp))) #f)
      (else
       #t))))
(define var-exp->var
  (lambda (exp)
    exp))
(define lambda-exp->bound-var
  (lambda (exp)
    (caadr exp)))
(define lambda-exp->body
  (lambda (exp)
    (caddr exp)))
(define app-exp->rator
  (lambda (exp)
    (car exp)))
(define app-exp->rand
  (lambda (exp)
    (cadr exp)))
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
