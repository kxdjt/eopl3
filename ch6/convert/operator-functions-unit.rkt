#lang racket

(require "./data-structures-unit.rkt")

(provide operator-fun^ operator-fun@)

(define-signature operator-fun^
  (binary-operator
   unary-operator
   none-operator
   any-operator
   ))

(define-unit operator-fun@
  (import data-structures^)
  (export operator-fun^)

  ;; Implatement operator function
  (define make-arithmetic-op
    (lambda (op)
      (lambda (evals)
        (num-val (op
                  (expval->num (car evals))
                  (expval->num (cadr evals)))))))
  (define make-num-pred-op
    (lambda (pred)
      (lambda (evals)
        (if (pred
             (expval->num (car evals))
             (expval->num (cadr evals)))
            (bool-val #t)
            (bool-val #f)
            ))))
  (define cons-op
    (lambda (evals)
      (list-val (cons (car evals)
                      (expval->list (cadr evals))))))

  (define zero?-op
    (lambda (eval)
      (if (zero? (expval->num eval))
          (bool-val #t)
          (bool-val #f)
          )))
  (define minus-op
    (lambda (eval)
      (num-val (- (expval->num eval)))))
  (define number?-op
    (lambda(eval)
      (bool-val (expval->isnum? eval))))
  (define add1-op
    (lambda(eval)
      (num-val (+ (expval->num eval) 1))))
  (define print-op
    (lambda (eval)
      (printf "~s\n" eval)
      (num-val 29)))
  (define car-op
    (lambda (eval)
      (car (expval->list eval))))
  (define cdr-op
    (lambda (eval)
      (list-val
       (cdr (expval->list eval)))))
  (define null?-op
    (lambda(eval)
      (bool-val (null?
                 (expval->list eval)))))

  (define emptylist-op
    (lambda()
      (list-val '())))
  (define list-op
    (lambda(rands)
      (list-val rands)))
  (define begin-op
    (lambda(rands)
      (if (null? rands)
          (num-val 30)
          (car (reverse rands)))))

  (define make-fun-table
    (lambda lst
      (lambda (op)
        (cdr (assoc op lst)))))
  (define binary-operator
    (make-fun-table
     (cons "+" (make-arithmetic-op +))
     (cons "-" (make-arithmetic-op -))
     (cons "*" (make-arithmetic-op *))
     (cons "/" (make-arithmetic-op /))
     (cons "equal?" (make-num-pred-op equal?))
     (cons "greater?" (make-num-pred-op >))
     (cons "less?" (make-num-pred-op <))
     (cons "cons" cons-op)
     ))
  (define unary-operator
    (make-fun-table
     (cons "zero?" zero?-op)
     (cons "minus" minus-op)
     (cons "number?" number?-op)
     (cons "print" print-op)
     (cons "car" car-op)
     (cons "cdr" cdr-op)
     (cons "null?" null?-op)
     (cons "add1" add1-op)
     ))
  (define none-operator
    (make-fun-table
     (cons "emptylist" emptylist-op)
     ))
  (define any-operator
    (make-fun-table
     (cons "list" list-op)
     (cons "begin" begin-op)
     ))
  )
