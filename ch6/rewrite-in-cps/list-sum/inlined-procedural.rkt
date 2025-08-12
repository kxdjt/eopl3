#lang racket

(require eopl)

(provide list-sum)

(define list-sum/k
  (lambda (loi cont)
    (if (null? loi)
        (cont 0)
        (list-sum/k (cdr loi)
                    (lambda(val)
                      (cont (+ (car loi) val)))))))

(define list-sum
  (lambda (loi)
    (list-sum/k loi (lambda (val)
                      (eopl:printf "End of computation.~%")
                      (eopl:printf "This sentence should appear only once.~%")
                      val))))
