#lang racket

(require "../data-structures-unit.rkt")
(require "../procedure-sig.rkt")
(require "../store-unit.rkt")
(require "mutex-unit.rkt")
(require "continuations-sig.rkt")
(require "scheduler-unit.rkt")
(require "thread-unit.rkt")

(define-signature operator-fun^
  (binary-operator
   unary-operator
   none-operator
   any-operator
   proc-operator
   ))

(provide operator-fun^ operator-fun@)


(define-unit operator-fun@
  (import data-structures^ proc-def^ store^ mutex^ continuation^ scheduler^ thread^)
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
  (define send-op
    (lambda (eval1 eval2)
      (let ((th-id (expval->num eval1)))
        (if (send-thread-msg th-id eval2)
            (begin
              (remove-thread-from-wait-queue!
               th-id
               (lambda (thread)
                 (place-on-ready-queue! thread)))
              (bool-val #t))
            (bool-val #f)))))
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
  (define mutex-op
    (lambda (store)
      (new-mutex store)))
  (define yield-op
    (lambda (store cont)
      (let ((time-remain (get-time-remaining)))
        (place-on-ready-queue!
         (make-thread
          (lambda (store)
            (set-time-remaining! time-remain)
            (apply-cont cont (an-answer (num-val 99)
                                        store)))))
        (run-next-thread store))))
  (define recive-op
    (lambda (store cont)
      (let ((th-id (get-cur-thread-id)))
        (if (thread-msg-list-is-empty? th-id)
            (begin
              (place-on-wait-queue!
               (make-thread
                (lambda(store)
                  (let ((msg (recive-thread-msg th-id)))
                    (apply-cont cont (an-answer msg store))))))
              (run-next-thread store))
            (apply-cont cont (an-answer
                              (recive-thread-msg th-id)
                              store))))))
  ;; Register operator function
  (define apply-cont-fun
    (lambda (store-fun)
      (lambda (store cont)
        (apply-cont cont (store-fun store)))))
  (define make-answer-fun
    (lambda (store-fun)
      (lambda (store)
        (an-answer (store-fun)
                   store))))
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
     (cons "send" send-op)
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
     (cons "emptylist" (apply-cont-fun (make-answer-fun emptylist-op)))
     (cons "mutex" (apply-cont-fun mutex-op))
     (cons "yield" yield-op)
     (cons "recive" recive-op)
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
