#lang racket

(require eopl)
(provide (all-defined-out))

#| Syntax data types for the LET language |#
(define scanner-spec-cps-out
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
(define grammar-cps-out
  '((cps-program (tfexp) cps-a-program)
    (simpleexp (number) cps-const-exp)
    (simpleexp (identifier) cps-var-exp)
    (simpleexp ( "proc" "(" (separated-list identifier ",")  ")" tfexp) cps-proc-exp)
    (simpleexp (cps-inner-operator "(" (arbno simpleexp) ")") cps-innerop-exp)
    (tfexp (simpleexp) simple-exp->exp)
    (tfexp ("let" (arbno identifier "=" simpleexp) "in" tfexp) cps-let-exp)
    (tfexp ("letrec" (arbno identifier "(" (arbno identifier) ")" "=" tfexp) "in" tfexp) cps-letrec-exp)
    (tfexp ("if" simpleexp "then" tfexp "else" tfexp) cps-if-exp)
    (tfexp ("(" simpleexp (arbno simpleexp) ")") cps-call-exp)
    (cps-inner-operator (none-op) cps-none-op)
    (cps-inner-operator (binary-op) cps-binary-op)
    (cps-inner-operator (unary-op) cps-unary-op)
    (cps-inner-operator (any-op) cps-any-op)
    ))

(sllgen:make-define-datatypes scanner-spec-cps-out grammar-cps-out)

(define just-scan-cps-out
  (sllgen:make-string-scanner scanner-spec-cps-out grammar-cps-out))

(define scan&parse-cps-out
  (sllgen:make-string-parser scanner-spec-cps-out grammar-cps-out))

