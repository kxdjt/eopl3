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

(define var 'undefined)
(define exp 'undefined)
(define cont 'undefined)
(define val 'undefined)
(define pc 'undefined)

(define occurs-free?/k
  (lambda()
    (cond
      ((symbol? exp)
       (set! val (eqv? var exp))
       (set! pc apply-cont))
      ((eqv? (car exp) 'lambda)
       (set! cont
             (occurs-free-and-cont
              (not (eqv? var (car (cadr exp))))
              cont))
       (set! exp (caddr exp)))
      (else
       (set! cont
             (occurs-free-or-cont var
                                  (cadr exp)
                                  cont))
       (set! exp (car exp))))))

(define apply-cont
  (lambda()
    (cases continuation cont
      (end-cont ()
                (begin
                  (eopl:printf "End of computation.~%")
                  (eopl:printf "This sentence should appear only once.~%")
                  (set! pc #f)))
      (occurs-free-and-cont (val1 save-cont)
                            (set! val (and val1 val))
                            (set! cont save-cont))
      (occurs-free-or-cont (var r-exp save-cont)
                           (if val
                               (set! cont save-cont)
                               (begin
                                 (set! exp r-exp)
                                 (set! cont save-cont)
                                 (set! pc occurs-free?/k))))
      )))

(define trampolined
  (lambda()
    (if pc
        (begin
          (pc)
          (trampolined))
        val)))

(define occurs-free?
  (lambda (arg-var arg-exp)
    (set! var arg-var)
    (set! exp arg-exp)
    (set! cont (end-cont))
    (set! pc occurs-free?/k)
    (trampolined)))
