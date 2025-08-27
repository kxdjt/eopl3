#lang racket

(require eopl)
(provide (all-defined-out))

#| Syntax data types for the LET language |#
(define scanner-spec-cps-in
  '((white-sp (whitespace) skip)
    (number ((or digit (concat "-" digit)) (arbno digit)) number)
    (identifier (letter (arbno (or letter digit "?" "-" "_"))) symbol)
    (binary-op ((or "+" "-" "*" "/" "equal?" "greater?" "less?" "cons"
                    )) string)
    (unary-op ((or "minus" "zero?" "car" "cdr" "null?" "number?"
                   "print" "add1")) string)
    (none-op ((or "emptylist")) string)
    (any-op ((or "list" "begin")) string)
    ))
(define grammar-cps-in
  '((program (inpexp) a-program)
    (inpexp (number) const-exp)
    (inpexp ("if" inpexp "then" inpexp "else" inpexp) if-exp)
    (inpexp (identifier) var-exp)
    (inpexp ("let" (arbno identifier "=" inpexp) "in" inpexp) let-exp)
    (inpexp ( "letrec" (arbno identifier "(" (arbno identifier) ")" "=" inpexp) "in" inpexp) letrec-exp)
    (inpexp ( "proc" "(" (separated-list identifier ",")  ")" inpexp) proc-exp)
    (inpexp ( "(" inpexp (arbno inpexp) ")" ) call-exp)
    (inpexp (inner-operator "(" (arbno inpexp) ")") innerop-exp)
    (inner-operator (none-op) none-op)
    (inner-operator (binary-op) binary-op)
    (inner-operator (unary-op) unary-op)
    (inner-operator (any-op) any-op)
    ;;-----------Store Interface------------------
    ;;Expression ::= set identifier = expression
    (inpexp ("set" identifier "=" inpexp) set-exp)
    ))

(sllgen:make-define-datatypes scanner-spec-cps-in grammar-cps-in)

(define just-scan-cps-in
  (sllgen:make-string-scanner scanner-spec-cps-in grammar-cps-in))

(define scan&parse-cps-in
  (sllgen:make-string-parser scanner-spec-cps-in grammar-cps-in))
