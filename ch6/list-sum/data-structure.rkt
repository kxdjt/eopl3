#lang racket

(require eopl)

(provide list-sum)

(define-datatype continuation continuation?
  (end-cont)
  (list-sum-cont
   (subsum number?)
   (cont continuation?)))


(define list-sum/k
  (lambda (loi cont)
    (if (null? loi)
        (apply-cont cont 0)
        (list-sum/k (cdr loi)
                    (list-sum-cont (car loi) cont)))))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
                (begin
                  (eopl:printf "End of computation.~%")
                  (eopl:printf "This sentence should appear only once.~%")
                  val))
      (list-sum-cont (sum cont)
                     (apply-cont cont
                                 (+ sum val))))))

(define list-sum
  (lambda (loi)
    (list-sum/k loi (end-cont))))
