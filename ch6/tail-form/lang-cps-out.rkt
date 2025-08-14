#lang racket

(require eopl)
(provide (all-defined-out))

#| Syntax data types for the LET language |#
(define scanner-spec-let
  '((white-sp (whitespace) skip)
    (number ((or digit (concat "-" digit)) (arbno digit)) number)
    (identifier (letter (arbno (or letter digit "?" "-" "_"))) symbol)
    (binary-op ((or "+" "-" "*" "/" "equal?" "greater?" "less?" "cons"
                    )) string)
    (unary-op ((or "minus" "zero?" "car" "cdr" "null?" "number?"
                   "print" "add1")) string)
    (none-op ((or "emptylist")) string)
    (any-op ((or "list")) string)
    ))
(define grammar-let
  '((program (tfexp) a-program)
    (simpleexp (number) const-exp)
    (simpleexp (identifier) var-exp)
    (simpleexp ( "proc" "(" (separated-list identifier ",")  ")" tfexp) cps-proc-exp)
    (simpleexp (inner-operator "(" (arbno simpleexp) ")") innerop-exp)
    (tfexp (simpleexp) simple-exp->exp)
    (tfexp ("let" (arbno identifier "=" simpleexp) "in" tfexp) cps-let-exp)
    (tfexp ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" tfexp) "in" tfexp) cps-letrec-exp)
    (tfexp ("if" simpleexp "then" tfexp "else" tfexp) cps-if-exp)
    (tfexp ("(" simpleexp (arbno simpleexp) ")") cps-call-exp)
    (inner-operator (none-op) none-op)
    (inner-operator (binary-op) binary-op)
    (inner-operator (unary-op) unary-op)
    (inner-operator (any-op) any-op)
    ))

(sllgen:make-define-datatypes scanner-spec-let grammar-let)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-let grammar-let))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-let grammar-let))

