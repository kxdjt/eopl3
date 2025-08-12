#lang racket

(require eopl)
(provide list-sum)

(define list-sum/k
  (lambda(loi cont)
    (if (null? loi)
        (apply-cont cont 0)
        (list-sum/k (cdr loi)
                    (list-sum-cont (car loi) cont)))))

(define apply-cont
  (lambda (cont val)
    (cont val)))

(define end-cont
  (lambda()
    (lambda(val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))
(define list-sum-cont
  (lambda(sum cont)
    (lambda(val)
      (apply-cont cont
                  (+ sum val)))))

(define list-sum
  (lambda(loi)
    (list-sum/k loi (end-cont))))
