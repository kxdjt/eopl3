#lang racket

(require "data-structures-unit.rkt")
(require "procedure-sig.rkt")

(define-signature operator-fun^
  (binary-operator
   unary-operator
   none-operator
   any-operator
   proc-operator
   ))

(provide operator-fun^ operator-fun@)


(define-unit operator-fun@
  (import data-structures^ proc-def^)
  (export operator-fun^)

  ;; Implatement operator function
  (define make-arithmetic-op
    (lambda (op)
      (lambda (eval1 eval2)
        (num-val (op
                  (expval->num eval1)
                  (expval->num eval2))))))
  (define make-num-pred-op
    (lambda (pred)
      (lambda (eval1 eval2)
        (if (pred
             (expval->num eval1)
             (expval->num eval2))
            (bool-val #t)
            (bool-val #f)
            ))))
  (define cons-op
    (lambda (eval1 eval2)
      (list-val
       (cons-val eval1
                 (expval->list eval2)))))
  (define zero?-op
    (lambda (eval)
      (if (zero? (expval->num eval))
          (bool-val #t)
          (bool-val #f)
          )))
  (define minus-op
    (lambda (eval)
      (num-val (- (expval->num eval)))))
  (define car-op
    (lambda (eval)
      (explist->car (expval->list eval))))
  (define cdr-op
    (lambda (eval)
      (list-val (explist->cdr (expval->list eval)))))
  (define null?-op
    (lambda (eval)
      (bool-val (explist->null? (expval->list eval)))))
  (define print-op
    (lambda (eval)
      (printf "~s\n" eval)
      (num-val 29)))
  (define emptylist-op
    (lambda ()
      (list-val
       (empty-list))))
  (define list-op
    (lambda (evals)
      (if (null? evals)
          (list-val (empty-list))
          (list-val
           (cons-val (car evals)
                     (expval->list (list-op (cdr evals))))))))
  ;; Register operator function
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
     (cons "car" car-op)
     (cons "cdr" cdr-op)
     (cons "null?" null?-op)
     (cons "print" print-op)
     ))
  (define none-operator
    (make-fun-table
     (cons "emptylist" emptylist-op)
     ))
  (define any-operator
    (make-fun-table
     (cons "list" list-op)
     ))
  (define proc-operator
    (make-fun-table
     (cons "proc" procedure)
     #| (cons "traceproc" traceproc) |#
     (cons "dyproc" dynamicproc)
     ))
  )
