#lang racket

(require eopl)

(provide remove-first)

(define-datatype continuation continuation?
  (end-cont)
  (remove-first-cont
   (var (lambda(val) #t))
   (cont continuation?)))

(define remove-first/k
  (lambda (s los cont)
    (if (null? los)
        (apply-cont cont '())
        (if (eqv? (car los) s)
            (apply-cont cont (cdr los))
            (remove-first/k s (cdr los)
                            (remove-first-cont (car los) cont))))))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
                (begin
                  (eopl:printf "End of computation.~%")
                  (eopl:printf "This sentence should appear only once.~%")
                  val))
      (remove-first-cont (var cont)
                         (apply-cont cont
                                     (cons var val))))))
(define remove-first
  (lambda (s los)
    (remove-first/k s los (end-cont))))


