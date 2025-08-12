#lang racket

(require eopl)

(provide remove-first)

(define remove-first/k
  (lambda (s los cont)
    (if (null? los)
        (cont '())
        (if (eqv? (car los) s)
            (cont (cdr los))
            (remove-first/k s (cdr los)
                            (lambda(val)
                              (cont (cons (car los)
                                          val))))))))
(define end-cont
  (lambda()
    (lambda(val)
      (begin
        (eopl:printf "End of computation.~%")
        (eopl:printf "This sentence should appear only once.~%")
        val))))

(define remove-first
  (lambda (s los)
    (remove-first/k s los (end-cont))))
