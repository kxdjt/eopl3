#lang racket

(require eopl)

(provide occurs-free?)

(define-datatype continuation continuation?
  (end-cont)
  (occurs-free-and-cont
   (val boolean?)
   (cont continuation?))
  (occurs-free-or-cont
   (var symbol?)
   (r-exp (lambda(x)#t))
   (cont continuation?)))

(define occurs-free?/k
  (lambda (var exp cont)
    (cond
      ((symbol? exp)
       (apply-cont cont (eqv? var exp)))
      ((eqv? (car exp) 'lambda)
       (occurs-free?/k var (caddr exp)
                       (occurs-free-and-cont
                        (not (eqv? var (car (cadr exp))))
                        cont)))
      (else
       (occurs-free?/k var (car exp)
                       (occurs-free-or-cont var (cadr exp) cont)))
      )))

(define apply-cont
  (lambda(cont val)
    (cases continuation cont
      (end-cont ()
                (begin
                  (eopl:printf "End of computation.~%")
                  (eopl:printf "This sentence should appear only once.~%")
                  val))
      (occurs-free-and-cont (val1 cont)
                            (apply-cont cont (and
                                              val1
                                              val)))
      (occurs-free-or-cont (var r-exp cont)
                           (if val
                               (apply-cont cont val)
                               (occurs-free?/k var r-exp cont))))))

(define occurs-free?
  (lambda (var exp)
    (occurs-free?/k var exp (end-cont))))
