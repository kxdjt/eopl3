#lang racket

(require eopl)
(provide occurs-free?)

(define occurs-free?/k
  (lambda(var exp cont)
    (cond
      ((symbol? exp)
       (cont (eqv? var exp)))
      ((eqv? (car exp) 'lambda)
       (occurs-free?/k var
                       (caddr exp)
                       (lambda(val)
                         (cont (and
                                (not (eqv? var (car (cadr exp))))
                                val)))))
      (else
       (occurs-free?/k var
                       (car exp)
                       (lambda(val)
                         (if val
                             (cont val)
                             (occurs-free?/k var (cadr exp)
                                             (lambda(val)
                                               (cont val
                                                     )))))))
      )))

(define occurs-free?
  (lambda(var exp)
    (occurs-free?/k var exp
                    (lambda(val)
                      (eopl:printf "End of computation.~%")
                      (eopl:printf "This sentence should appear only once.~%")
                      val))))
