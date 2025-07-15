#lang racket

(require eopl)
(require "data-structures.rkt")
(require "../common/enironment.rkt")

(provide (all-defined-out))


;; data representation of continuations
(define-datatype continuation continuation?
  (end-cont)
  (if-cont
   (exp2 expression?)
   (exp3 expression?)
   (env environment?)
   (cont continuation?))
  (let-cont
   (vars (list-of symbol?))
   (exps (list-of expression?))
   (body expression?)
   (cont continuation?)
   (ori-env environment?)
   (env environment?))
  (let*-cont
   (vars (list-of symbol?))
   (exps (list-of expression?))
   (body expression?)
   (cont continuation?)
   (env environment?))
  (cond-cont
   (exps1 (list-of expression?))
   (exps2 (list-of expression?))
   (env environment?)
   (cont continuation?))
  (call-cont
   (rands (list-of expression?))
   (env environment?)
   (cont continuation?))
  (binary-op-cont1
   (op string?)
   (rands (list-of expression?))
   (env environment?)
   (cont continuation?))
  (binary-op-cont2
   (op string?)
   (eval expval?)
   (cont continuation?))
  (unary-op-cont
   (op string?)
   (cont continuation?))
  (any-op-cont
   (op string?)
   (rands (list-of expression?))
   (env environment?)
   (cont continuation?)
   (vals (list-of expval?)))
  (proc-cont
   (proc proc?)
   (rands (list-of expression?))
   (env environment?)
   (cont continuation?)
   (vals (list-of expval?)))
  (begin-cont
    (exps2 (list-of expression?))
    (env environment?)
    (cont continuation?))
  (set-cont
   (ref number?)
   (cont continuation?))
  (try-cont
   (ident symbol?)
   (exp2 expression?)
   (env environment?)
   (cont continuation?))
  (raise-cont
   (cont continuation?))
  )

(define make-let-cont-by-op
  (lambda (op vars exps body cont env)
    (if (equal? op "let*")
        (let*-cont vars exps body cont env)
        (let-cont vars exps body cont env env))))

(define cont->nextcont
  (lambda (cont)
    (cases continuation cont
      (end-cont ()
                (eopl:error 'cont->nextcont "End, no found!"))
      (if-cont (_ a b cont)
               cont)
      (let-cont (_ a b cont c d)
                cont)
      (let*-cont (_ a b cont c)
                 cont)
      (cond-cont (_ a b cont)
                 cont)
      (call-cont (_ a cont)
                 cont)
      (binary-op-cont1 (_ a b cont)
                       cont)
      (binary-op-cont2 (_ a cont)
                       cont)
      (unary-op-cont (_ cont)
                     cont)
      (any-op-cont (_ a b cont c)
                   cont)
      (proc-cont (_ a b cont c)
                 cont)
      (begin-cont (_ a cont)
                  cont)
      (set-cont (_ cont)
                cont)
      (try-cont (_ a b cont)
                cont)
      (raise-cont (cont)
                  cont))))
