#lang racket

(require eopl)

(provide subst)

(define subst/k
  (lambda (new old slist cont)
    (if (null? slist)
        (cont '())
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
            (cont new)
            (cont sexp))
        (subst/k new old sexp cont))))

(define subst-cont
  (lambda (new old lst cont)
    (lambda(val)
      (subst/k new old lst
               (subst-cont2 val cont)))))

(define subst-cont2
  (lambda (val1 cont)
    (lambda(val)
      (cont (cons val1 val)))))

(define end-cont
  (lambda()
    (lambda(val)
      (eopl:printf "End of computation.~%")
      (eopl:printf "This sentence should appear only once.~%")
      val)))

(define subst
  (lambda(new old slist)
    (subst/k new old slist (end-cont))))

