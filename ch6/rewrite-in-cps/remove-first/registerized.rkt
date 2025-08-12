#lang racket

(require eopl)
(provide remove-first)

(define-datatype continuation continuation?
  (end-cont)
  (remove-first-cont
   (var (lambda(val) #t))
   (cont continuation?)))


(define s 'undefined)
(define los 'undefined)
(define cont 'undefined)
(define val 'undefined)
(define pc 'undefined)

(define remove-first/k
  (lambda ()
    (if (null? los)
        (begin
          (set! val '())
          (set! pc apply-cont))
        (if (eqv? (car los) s)
            (begin
              (set! val (cdr los))
              (set! pc apply-cont))
            (begin
              (set! cont (remove-first-cont (car los) cont))
              (set! los (cdr los))
              (set! pc remove-first/k))))))

(define apply-cont
  (lambda()
    (cases continuation cont
      (end-cont ()
                (begin
                  (eopl:printf "End of computation.~%")
                  (eopl:printf "This sentence should appear only once.~%")
                  (set! pc #f)))
      (remove-first-cont (var save-cont)
                         (set! cont save-cont)
                         (set! val (cons var val))
                         (set! pc apply-cont)))))

(define trampolined
  (lambda()
    (if pc
        (begin
          (pc)
          (trampolined))
        val)))

(define remove-first
  (lambda (arg-s arg-los)
    (set! s arg-s)
    (set! los arg-los)
    (set! cont (end-cont))
    (set! pc remove-first/k)
    (trampolined)))

