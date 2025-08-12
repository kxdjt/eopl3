#lang racket

(require eopl)

(provide subst)

(define sym-or-lst?
  (lambda(x)
    (or (symbol? x)
        (list? x))))

(define-datatype continuation continuation?
  (end-cont)
  (subst-cont
   (new sym-or-lst?)
   (old sym-or-lst?)
   (lst list?)
   (cont continuation?))
  (subst-cont2
   (val sym-or-lst?)
   (cont continuation?))
  )

(define new 'undefined)
(define old 'undefined)
(define slist 'undefined)
(define sexp 'undefined)
(define cont 'undefined)
(define val 'undefined)
(define pc 'undefined)

(define subst/k
  (lambda ()
    (if (null? slist)
        (begin
          (set! val '())
          (set! pc apply-cont))
        (begin
          (set! sexp (car slist))
          (set! cont (subst-cont new
                                 old
                                 (cdr slist)
                                 cont))
          (set! pc subst-in-s-exp/k)))))

(define subst-in-s-exp/k
  (lambda()
    (if (symbol? sexp)
        (if (eqv? sexp old)
            (begin
              (set! val new)
              (set! pc apply-cont))
            (begin
              (set! val sexp)
              (set! pc apply-cont)))
        (begin
          (set! slist sexp)
          (set! pc subst/k)))))

(define apply-cont
  (lambda()
    (cases continuation cont
      (end-cont ()
                (begin
                  (eopl:printf "End of computation.~%")
                  (eopl:printf "This sentence should appear only once.~%")
                  (set! pc #f)))
      (subst-cont (save-new save-old save-lst save-cont)
                  (set! new save-new)
                  (set! old save-old)
                  (set! slist save-lst)
                  (set! cont (subst-cont2 val save-cont))
                  (set! pc subst/k))
      (subst-cont2 (val1 save-cont)
                   (set! cont save-cont)
                   (set! val (cons val1 val)))
      )))

(define trampolined
  (lambda()
    (if pc
        (begin
          (pc)
          (trampolined))
        val)))

(define subst
  (lambda(arg-new arg-old arg-slist)
    (set! new arg-new)
    (set! old arg-old)
    (set! slist arg-slist)
    (set! cont (end-cont))
    (set! pc subst/k)
    (trampolined)))


