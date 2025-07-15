#lang racket

(require eopl)
(require "data-structures.rkt")

(provide (all-defined-out))


;; Define Store -- implement by schema list
(define-datatype store store?
  (empty-store)
  (extend-store
   (ref number?)
   (val expval?)
   (s store?)))
(define store->nextref
  (lambda (s)
    (cases store s
      (empty-store () 1)
      (extend-store (ref eval _) (+ ref 1)))))
(define store->findref
  (lambda (s s-ref)
    (cases store s
      (extend-store (ref eval nexts)
                    (cond
                      ((equal? s-ref ref) eval)
                      ((> s-ref ref)
                       (eopl:error 'store->findref "invalid ref ~s in store ~s" s-ref s))
                      (else
                       (store->findref nexts s-ref))))
      (empty-store ()
                   (eopl:error 'store->findref "invalid ref ~s in store ~s" s-ref s)))))
(define store->setref
  (lambda (s s-ref eval)
    (cases store s
      (empty-store ()
                   (eopl:error 'store->setref "invalid ref ~s in store ~s" s-ref s))
      (extend-store (ref old-eval nexts)
                    (cond
                      ((equal? s-ref ref)
                       (extend-store
                        ref
                        eval
                        nexts))
                      ((> s-ref ref)
                       (eopl:error 'store->setref "invalid ref ~s in store ~s" s-ref s))
                      (else
                       (let ((ns (store->setref nexts s-ref eval)))
                         (extend-store
                          ref
                          old-eval
                          ns))))))))

;; Define Answer -- include expval and store
(define-datatype answer answer?
  (an-answer
   (eval expval?)
   (s store?)))
(define answer->eval
  (lambda (aw)
    (cases answer aw
      (an-answer (eval _) eval))))
(define answer->store
  (lambda (aw)
    (cases answer aw
      (an-answer (_ s) s))))
