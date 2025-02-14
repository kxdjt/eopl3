#lang racket

(require (except-in eopl list-of))
(require "ch2-ribcage-env.rkt")
(require "ch2-datatype-utils.rkt")

(define identifier? symbol?)
(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))
(define explist-extractor-error
  (lambda (val)
    (eopl:error 'explist-extractors "expected: pair? given: ~s" val)))

#| Syntax data types for the LET language |#
(define-datatype program program?
  (a-program
   (exp1 expression?)))
(define-datatype expression expression?
  (const-exp
   (num number?))
  (diff-exp
   (exp1 expression?)
   (exp2 expression?))
  (zero?-exp
   (exp1 expression?))
  (if-exp
   (exp1 expression?)
   (exp2 expression?)
   (exp3 expression?))
  (var-exp
   (var identifier?))
  (let-exp
   (var identifier?)
   (exp1 expression?)
   (body expression?))
  (minus-exp
   (exp1 expression?))
  (add-exp
   (exp1 expression?)
   (exp2 expression?))
  (mulit-exp
   (exp1 expression?)
   (exp2 expression?))
  (int-quo-exp
   (exp1 expression?)
   (exp2 expression?))
  (equal?-exp
   (exp1 expression?)
   (exp2 expression?))
  (greater?-exp
   (exp1 expression?)
   (exp2 expression?))
  (less?-exp
   (exp1 expression?)
   (exp2 expression?))
  (cons-exp
   (exp1 expression?)
   (exp2 expression?))
  (car-exp
   (exp1 expression?))
  (cdr-exp
   (exp1 expression?))
  (emptylist-exp)
  (null?-exp
   (exp1 expression?))
  (list-exp
   (exps (list-of expression?)))
  )
#| Expressed values for the LET language |#
(define-datatype expval expval?
  (num-val
   (num number?))
  (bool-val
   (bool boolean?))
  (list-val
   (elist explist?)))
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else expval-extractor-error 'num val))))
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else expval-extractor-error 'bool val))))
(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (elist) elist)
      (else expval-extractor-error 'list val))))
(define-datatype explist explist?
  (empty-list)
  (cons-val
   (val expval?)
   (list2 explist?)))
(define explist->car
  (lambda (elist)
    (cases explist elist
      (empty-list ()
                  (explist-extractor-error '()))
      (cons-val (val _)
                val))))
(define explist->cdr
  (lambda (elist)
    (cases explist elist
      (empty-list ()
                  (explist-extractor-error '()))
      (cons-val (val list2)
                list2))))
(define explist->null?
  (lambda (elist)
    (cases explist elist
      (empty-list ()
                  (bool-val #t))
      (else
       (bool-val #f)))))
#| Interpreter for the LET language |#
;; run : string -> ExpVal
#| (define run |#
#|   (lambda (string) |#
#|     (value-of-program (scan&parse string)))) |#
(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
                (extend-env 'v (num-val 5)
                            (extend-env 'x (num-val 10)
                                        (empty-env))))))

;; Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))
;; Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (define predicate
      (lambda (pred exp1 exp2)
        (if (pred
             (expval->num (value-of exp1 env))
             (expval->num (value-of exp2 env)))
            (bool-val #t)
            (bool-val #f))))
    (define binary-op
      (lambda (op exp1 exp2)
        (num-val (op
                  (expval->num (value-of exp1 env))
                  (expval->num (value-of exp2 env))))))
    (cases expression exp
      (const-exp (num)
                 (num-val num))
      (if-exp (exp1 exp2 exp3)
              (if (expval->bool (value-of exp1 env))
                  (value-of exp2 env)
                  (value-of exp3 env)))
      (var-exp (var)
               (apply-env env var))
      (let-exp (var exp1 body)
               (value-of body (extend-env var
                                          (value-of exp1 env)
                                          env)))
      (minus-exp (exp1)
                 (num-val (- (expval->num (value-of exp1 env)))))
      (zero?-exp (exp1)
                 (if (zero? (expval->num (value-of exp1 env)))
                     (bool-val #t)
                     (bool-val #f)))
      (diff-exp (exp1 exp2)
                (binary-op - exp1 exp2))
      (add-exp (exp1 exp2)
               (binary-op + exp1 exp2))
      (mulit-exp (exp1 exp2)
                 (binary-op * exp1 exp2))
      (int-quo-exp (exp1 exp2)
                   (binary-op / exp1 exp2))
      (equal?-exp (exp1 exp2)
                  (predicate equal? exp1 exp2 env))
      (greater?-exp (exp1 exp2)
                    (predicate > exp1 exp2 env))
      (less?-exp (exp1 exp2)
                 (predicate < exp1 exp2 env))
      (cons-exp (exp1 exp2)
                (list-val
                 (cons-val (value-of exp1 env)
                           (expval->list (value-of exp2 env)))))
      (car-exp (exp1)
               (explist->car(expval->list exp1)))
      (cdr-exp (exp1)
               (list-val (explist->cdr (expval->list (value-of exp1 env)))))
      (emptylist-exp ()
                     (list-val (empty-list)))
      (null?-exp (elist)
                 (explist->null? elist))
      (list-exp (exps)
                (list-val
                 (foldl (lambda (exp res)
                          (cons-val (value-of exp env) res))
                        (empty-list)
                        exps)
                 ))
      )))
(define exp
  (let-exp 'y (const-exp 5) (diff-exp (var-exp 'x) (var-exp 'y))))
exp
(define pgm
  (a-program exp))
pgm
(value-of exp (init-env))
(value-of-program pgm)
(define math-exp
  (add-exp (const-exp 5)
           (diff-exp (const-exp 6)
                     (mulit-exp (const-exp 2)
                                (int-quo-exp (const-exp 4)
                                             (minus-exp (const-exp 2)))))))
(value-of math-exp (init-env))
(define list-exp1
  (let-exp 'x (const-exp 4)
           (cons-exp (var-exp 'x)
                     (cons-exp (cons-exp (diff-exp (var-exp 'x)
                                                   (const-exp 1))
                                         (emptylist-exp))
                               (emptylist-exp)))))
list-exp1
(value-of list-exp1 (init-env))
(define list-exp2
  (list-exp (list (const-exp 1) (const-exp 2)
                  (cons-exp (const-exp 3)
                            (emptylist-exp))
                  (list-exp (list (const-exp 4) (list-exp '()))))))
list-exp2
(value-of list-exp2 (init-env))
