#lang racket

(require eopl)

(provide list-sum)


(define-datatype continuation continuation?
  (end-cont)
  (list-sum-cont
   (subsum number?)
   (cont continuation?)))

(define loi 'undefined)
(define cont 'undefined)
(define val 'undefined)
(define pc 'undefined)

(define list-sum/k
  (lambda()
    (if (null? loi)
        (begin
          (set! val 0)
          (set! pc apply-cont))
        (begin
          (set! cont (list-sum-cont (car loi) cont))
          (set! loi (cdr loi))
          ))))

(define apply-cont
  (lambda()
    (cases continuation cont
      (end-cont ()
                (begin
                  (eopl:printf "End of computation.~%")
                  (eopl:printf "This sentence should appear only once.~%")
                  (set! pc #f)))
      (list-sum-cont (save-val save-cont)
                     (set! cont save-cont)
                     (set! val (+ save-val val))
                     ))))

(define trampolined
  (lambda()
    (if pc
        (begin
          (pc)
          (trampolined))
        val)))

(define list-sum
  (lambda (arg-loi)
    (set! loi arg-loi)
    (set! cont (end-cont))
    (set! pc list-sum/k)
    (trampolined)))
