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

(define subst/k
  (lambda (new old slist cont)
    (if (null? slist)
        (apply-cont cont '())
        (subst-in-s-exp/k new
                          old
                          (car slist)
                          (subst-cont new
                                      old
                                      (cdr slist)
                                      cont)))))

(define subst-in-s-exp/k
  (lambda (new old sexp cont)
    (if (symbol? sexp)
        (if (eqv? sexp old)
            (apply-cont cont new)
            (apply-cont cont sexp))
        (subst/k new old sexp cont))))

(define apply-cont
  (lambda (cont val)
    (cases continuation cont
      (end-cont ()
                (begin
                  (eopl:printf "End of computation.~%")
                  (eopl:printf "This sentence should appear only once.~%")
                  val))
      (subst-cont (new old lst save-cont)
                  (subst/k new old lst
                           (subst-cont2 val save-cont)))
      (subst-cont2 (val1 save-cont)
                   (apply-cont save-cont
                               (cons val1 val)))
      )))

(define subst
  (lambda (new old slist)
    (subst/k new old slist (end-cont))))

