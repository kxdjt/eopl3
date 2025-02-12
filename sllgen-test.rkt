#lang racket

(require eopl)

#| (define scanner-spec |#
#|   '((whitespace (whitespace) skip) |#
#|     (comment ("%" (arbno (not #\newline))) skip) |#
#|     (number (digit (arbno digit)) number) |#
#|     (number ("-" digit (arbno digit)) number) |#
#|     (nullary-op ("emptylist") string) |#
#|     (unary-op ((or "zero?" "minus" "car" "cdr" "null?" "print")) string) |#
#|     (binary-op ((or "+" "-" "*" "/" "equal?" "greater?" "less?" "cons")) string) |#
#|     (n-ary-op ("list") string) |#
#|     (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol))) |#
#||#
#| (define grammar |#
#|   '((program (expression) a-program) |#
#|     (expression (number) const-exp) |#
#|     (expression (nullary-op) nullary-exp) |#
#|     (expression (unary-op "(" expression ")") unary-exp) |#
#|     (expression (binary-op "(" expression "," expression ")") binary-exp) |#
#|     (expression (n-ary-op "(" (separated-list expression ",") ")") n-ary-exp) |#
#|     (expression ("if" expression "then" expression "else" expression) if-exp) |#
#|     (expression ("cond" (arbno expression "==>" expression) "end") cond-exp) |#
#|     (expression (identifier) var-exp) |#
#|     (expression ("let" identifier "=" expression "in" expression) let-exp))) |#
#||#
#| (sllgen:make-define-datatypes scanner-spec grammar) |#


(define scanner-spec-arith
  '((white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)
    (additive-op ((or "+" "-")) string)
    (mulit-op ((or "*" "/")) symbol)))

#| (define grammar-arith |#
#|   '((arith-expr (arith-term (arbno additive-op arith-term)) a-arith-expr) |#
#|     (arith-term (arith-factor (arbno mulit-op arith-factor)) a-arith-term) |#
#|     (arith-factor (number) num-factor) |#
#|     (arith-factor ("( %" arith-expr ")") expr-factor))) |#

;; Define additive-op and mulit-op as datatype
(define grammar-arith
  '((arith-expr (arith-term (arbno add-op arith-term)) a-arith-expr)
    (arith-term (arith-factor (arbno mul-op arith-factor)) a-arith-term)
    (arith-factor (number) num-factor)
    (arith-factor ("( %" arith-expr ")") expr-factor)
    (add-op (additive-op) a-add-op)
    (mul-op (mulit-op) a-mulit-op)))
#| (sllgen:list-define-datatypes scanner-spec-arith grammar-arith) |#
(sllgen:make-define-datatypes scanner-spec-arith grammar-arith)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-arith grammar-arith))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-arith grammar-arith))
