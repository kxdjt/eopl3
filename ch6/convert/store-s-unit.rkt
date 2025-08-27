#lang racket

(require eopl)

(provide s-store^ s-store@)

(define-signature s-store^
  (empty-store
   get-store
   initialize-store!
   reference?
   nextref
   newref
   deref
   setref!)
  )

(define the-store 'uninitialized)

(define report-invalid-reference
  (lambda (ref s)
    (eopl:error 'store->setref "invalid ref ~s in store ~s" ref s)))

(define-unit s-store@
  (import)
  (export s-store^)

  ;;empty-store : () → Sto
  (define empty-store
    (lambda () '()))
  ;; get-store : () → Sto
  (define get-store
    (lambda () the-store))
  ;;initialize-store! : () → Unspecified
  ;;usage: (initialize-store!) sets the-store to the empty store
  (define initialize-store!
    (lambda ()
      (set! the-store (empty-store))))
  ;; reference? : SchemeVal → Bool
  (define reference?
    (lambda (v)
      (integer? v)))
  ;; nextref : -> num
  (define nextref
    (lambda()
      (length the-store)))
  ;;newref : ExpVal → Ref
  (define newref
    (lambda (val)
      (let ((next-ref (length the-store)))
        (set! the-store (append the-store (list val)))
        next-ref)))
  ;; deref : Ref → ExpVal
  (define deref
    (lambda (ref)
      (list-ref the-store ref)))
  ;;setref! : Ref × ExpVal → Unspecified
  ;;usage: sets the-store to a state like the original, but with position ref containing val.
  (define setref!
    (lambda (ref val)
      (set! the-store
            (letrec
                ((setref-inner
                  (lambda (store1 ref1)
                    (cond
                      ((null? store1)
                       (report-invalid-reference ref the-store))
                      ((zero? ref1)
                       (cons val (cdr store1)))
                      (else
                       (cons
                        (car store1)
                        (setref-inner
                         (cdr store1) (- ref1 1))))))))
              (setref-inner the-store ref)))))
  )
