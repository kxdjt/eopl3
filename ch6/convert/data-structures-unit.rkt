#lang racket

(require eopl)
(require "./procedure-sig.rkt"
         "./gramer-sig.rkt")

(provide data-structures^ data-structures@)

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))
(define-signature data-structures^
  (expval?
   num-val
   proc-val
   bool-val
   str-val
   innerop-val
   list-val
   expval->bool
   expval->num
   expval->str
   expval->proc
   expval->innerop
   expval->list
   expval->isnum?
   expval->isproc?
   expval->isinnerop?
   expval->schemaval
   )
  )

(define-unit data-structures@
  (import proc-def^ gramer-inf^)
  (export data-structures^)
  #| Expressed values for the CPS-OUT language |#
  (define-datatype expval expval?
    (num-val
     (num number?))
    (str-val
     (str string?))
    (proc-val
     (proc proc?))
    (innerop-val
     (innerop inner-operator?))
    (list-val
     (lst list?))
    )
  (define bool-val
    (lambda (boolean)
      (if boolean
          (num-val 1)
          (num-val 0))))
  (define expval->bool
    (lambda (val)
      (cases expval val
        (num-val (num)
                 (if (equal? num 0) #f
                     #t))
        (else expval-extractor-error 'int-bool val))))
  (define expval->num
    (lambda (val)
      (cases expval val
        (num-val (num) num)
        (else expval-extractor-error 'num val))))
  (define expval->str
    (lambda (val)
      (cases expval val
        (str-val (str) str)
        (else expval-extractor-error 'str val))))
  (define expval->proc
    (lambda (val)
      (cases expval val
        (proc-val (proc) proc)
        (else expval-extractor-error 'proc val))))
  (define expval->innerop
    (lambda (val)
      (cases expval val
        (innerop-val (innerop) innerop)
        (else expval-extractor-error 'innerop val))))
  (define expval->list
    (lambda(val)
      (cases expval val
        (list-val (lst) lst)
        (else expval-extractor-error 'list val))))
  (define expval->isnum?
    (lambda(val)
      (cases expval val
        (num-val (_) #t)
        (else #f))))
  (define expval->isproc?
    (lambda (val)
      (cases expval val
        (proc-val (_) #t)
        (else #f))))
  (define expval->isinnerop?
    (lambda(val)
      (cases expval val
        (innerop-val (_) #t)
        (else #f))))
  (define expval->schemaval
    (lambda (val)
      (cases expval val
        (num-val (num) num)
        (proc-val (proc) proc)
        (innerop-val (innerop) innerop)
        (str-val (str) str)
        (list-val (lst)
                  (if (null? lst)
                      '()
                      (cons (expval->schemaval (car lst))
                            (expval->schemaval (list-val (cdr lst)))))))))
  )
