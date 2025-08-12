#lang racket

(require eopl)

(provide list-sum)

(define list-sum/k
  (lambda(loi cont)
    (if (null? loi)
        cont
        (list-sum/k (cdr loi)
                    (+ (car loi) cont)))))

(define list-sum
  (lambda (loi)
    (list-sum/k loi 0)))
