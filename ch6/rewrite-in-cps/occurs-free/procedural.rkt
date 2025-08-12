#lang racket

(require eopl)
(provide occurs-free?)

(define occurs-free?/k
  (lambda(var exp cont)
    (cond
      ((symbol? exp)
       (cont (eqv? var exp)))
      ((eqv? (car exp) 'lambda)
       (occurs-free?/k var (caddr exp)
                       (occurs-free-and-cont
                        (not (eqv? var (car (cadr exp))))
                        cont)))
      (else
       (occurs-free?/k var (car exp)
                       (occurs-free-or-cont
                        var
                        (cadr exp)
                        cont))))))

(define end-cont
  (lambda()
    (lambda(val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))

(define occurs-free-and-cont
  (lambda(val1 cont)
    (lambda(val)
      (cont
       (and val1 val)))))

(define occurs-free-or-cont
  (lambda (var r-exp cont)
    (lambda(val)
      (if val
          (cont val)
          (occurs-free?/k var r-exp cont)))))

(define occurs-free?
  (lambda (var exp)
    (occurs-free?/k var exp (end-cont))))
