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
                          (lambda(val)
                            (subst/k new
                                     old
                                     (cdr slist)
                                     (lambda(val2)
                                       (cont (cons val val2)))))))))

(define subst-in-s-exp/k
  (lambda (new old sexp cont)
    (if (symbol? sexp)
        (if (eqv? sexp old)
            (cont new)
            (cont sexp))
        (subst/k new old sexp cont))))

(define subst
  (lambda (new old slist)
    (subst/k new old slist
             (lambda(val)
               (eopl:printf "End of computation.~%")
               (eopl:printf "This sentence should appear only once.~%")
               val))))
