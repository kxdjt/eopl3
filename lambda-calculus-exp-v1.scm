(define var-exp
  (lambda (var)
    var))
(define lambda-exp
  (lambda (var lc-exp)
    (list `lambda (list var) lc-exp)))
(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list lc-exp1 lc-exp2)))
(define var-exp?
  (lambda (lc-exp)
    (not (list? lc-exp))))
(define lambda-exp?
  (lambda (lc-exp)
    (and (list? lc-exp)
         (equal? (car lc-exp) `lambda)
         (list? (cadr lc-exp))
         (not (list? (caadr lc-exp))))))
(define app-exp?
  (lambda (lc-exp)
    (and (list? lc-exp)
         (not (null? lc-exp))
         (not (null? (cdr lc-exp))))))
(define var-exp->var
  (lambda (lc-exp)
    (if (var-exp? lc-exp) lc-exp
        (error lc-exp "is not var-exp"))))
(define lambda-exp->bound-var
  (lambda (lc-exp)
    (if (lambda-exp? lc-exp) (caadr lc-exp)
        (error lc-exp "is not lambda-exp"))))
(define lambda-exp->body
  (lambda (lc-exp)
    (if (lambda-exp? lc-exp) (caddr lc-exp)
        (error lc-exp "is not lambda-exp"))))
(define app-exp->rator
  (lambda (lc-exp)
    (if (app-exp? lc-exp) (car lc-exp)
        (error lc-exp "is not app-exp"))))
(define app-exp->rand
  (lambda (lc-exp)
    (if (app-exp? lc-exp) (cadr lc-exp)
        (error lc-exp "is not app-exp"))))
