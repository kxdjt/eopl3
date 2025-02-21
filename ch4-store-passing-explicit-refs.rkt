#lang racket

(require (except-in eopl list-of))
#| (require "ch2-ribcage-env.rkt") |#
(require "ch3-enironment.rkt")
(require "ch2-datatype-utils.rkt")

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))

#| Syntax data types for the LET language |#
(define scanner-spec-let
  '((white-sp (whitespace) skip)
    (number ((or digit (concat "-" digit)) (arbno digit)) number)
    (identifier (letter (arbno (or letter digit "?"))) symbol)
    (binary-op ((or "+" "-" "*" "/" "equal?" "greater?" "less?" "cons")) string)
    (unary-op ((or "minus" "zero?" "car" "cdr" "null?")) string)
    (none-op ((or "emptylist")) string)
    (let-op ((or "let" "let*")) string)
    (proc-op ((or "proc" "traceproc" "dyproc")) string)
    ))
(define grammar-let
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    ;;Expression ::= let-op {identifier = expression}* in expression
    ;;              let-exp (op ids exps1 exp)
    (expression (let-op  (arbno identifier "=" expression )  "in" expression) let-exp)
    ;;Expression ::= letrec {identifier ({identifier}*) = expression}* in expression
    ;;               letrec-exp (proc-names list-of-vars exps1 exp2
    (expression ( "letrec" (arbno identifier "(" (arbno identifier) ")" "=" expression) "in" expression) letrec-exp)
    ;;Expression ::= cond { experssion ==> expression }* end
    ;;               cond-exp (list-of-cond list-of-exp)
    (expression ( "cond" "{" (arbno expression "==>" expression) "}") cond-exp)
    ;;Expression ::= proc {identifier}*(,) expression
    ;;               proc-exp (vars body)
    (expression ( proc-op "(" (separated-list identifier ",")  ")" expression) proc-exp)
    ;;Expression ::= (expression {expression}* )
    ;;               call-exp (rator rands)
    (expression ( "(" expression (arbno expression) ")" ) call-exp)
    (expression (inner-operator) innerop-exp)
    (inner-operator (none-op) none-op)
    (inner-operator (binary-op) binary-op)
    (inner-operator (unary-op) unary-op)
    ;;-----------Store Interface------------------
    ;;Expression ::= newref (expression)
    ;;               newref-exp (exp1)
    (expression ("newref" "(" expression ")" ) newref-exp)
    ;;Expression ::= deref (expression)
    ;;               deref-exp (exp1)
    (expression ("deref" "(" expression ")") deref-exp)
    ;;Expression ::= setref (expression , expression)
    ;;               setref-exp (exp1 exp2)
    (expression ("setref" "(" expression "," expression ")") setref-exp)
    ))

(sllgen:make-define-datatypes scanner-spec-let grammar-let)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-let grammar-let))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-let grammar-let))

;; Define Expressed valus
(define-datatype expval expval?
  (num-val
   (num number?))
  (proc-val
   (proc proc?))
  (innerop-val
   (innerop inner-operator?))
  (ref-val
   (ref number?))
  )
(define bool-val
  (lambda (val)
    (cond
      ((boolean? val)
       (if val
           (num-val 1)
           (num-val 0)))
      ((number? val)
       (num-val val))
      (else
       (eopl:error 'bool-val "invalid type for bool-val ~s" val)))))
(define expval->bool
  (lambda (eval)
    (cases expval eval
      (num-val (num)
               (if (equal? num 0) #f #t))
      (else
       (expval-extractor-error 'int-bool eval)))))
(define expval->num
  (lambda (eval)
    (cases expval eval
      (num-val (num) num)
      (else
       (expval-extractor-error 'num eval)))))
(define expval->proc
  (lambda (eval)
    (cases expval eval
      (proc-val (proc) proc)
      (else
       (expval-extractor-error 'proc eval)))))
(define expval->innerop
  (lambda (eval)
    (cases expval eval
      (innerop-val (innerop) innerop)
      (else
       (expval-extractor-error 'innerop eval)))))
(define expval->ref
  (lambda (eval)
    (cases expval eval
      (ref-val (ref) ref)
      (else
       (expval-extractor-error 'ref eval)))))
;; Define procedure
(define proc?
  (lambda (val)
    (procedure? val)))
;; Define Store -- implement by schema list
(define-datatype store store?
  (empty-store)
  (extend-store
   (ref number?)
   (val expval?)
   (s store?)))
(define store->nextref
  (lambda (s)
    (cases store s
      (empty-store () 1)
      (extend-store (ref . _) (+ ref 1)))))
;; Define Answer -- include expval and store
(define-datatype answer answer?
  (an-answer
   (eval expval?)
   (s store?)))
(define newref
  (lambda (s eval)
    (let ((ref (store->nextref s)))
      (an-answer
       (ref-val ref)
       (extend-store ref eval s)))))
(define deref
  (lambda (s s-ref)
    (cases store s
      (extend-store (ref eval nexts)
                    (cond
                      ((equal? s-ref ref)
                       (an-answer eval s))
                      ((> s-ref ref)
                       (eopl:error 'deref "invalid ref ~s in store ~s" s-ref s))
                      (else
                       (deref nexts s-ref))))
      (empty-store ()
                   (eopl:error 'deref "invalid ref ~s in store ~s" s-ref s)))))


