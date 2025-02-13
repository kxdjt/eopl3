#lang racket

(require (except-in eopl list-of))
(require "ch2-ribcage-env.rkt")
(require "ch2-datatype-utils.rkt")

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))
(define explist-extractor-error
  (lambda (val)
    (eopl:error 'explist-extractors "expected: pair? given: ~s" val)))

#| Syntax data types for the LET language |#
(define scanner-spec-let
  '((white-sp (whitespace) skip)
    (number (digit (arbno digit)) number)
    (identifier (letter (arbno (or letter digit "?"))) symbol)
    (binary-op ((or "+" "-" "*" "/" "equal?" "greater?" "less?" "cons")) string)
    (unary-op ((or "minus" "zero?" "car" "cdr" "null?")) string)
    (none-op ((or "emptylist")) string)
    (let-op ((or "let" "let*")) string)
    ))
(define grammar-let
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression (identifier) var-exp)
    ;;Expression ::= let-op {identifier = expression}* in expression
    ;;              let-exp (op ids exps1 exp)
    (expression (let-op  (arbno identifier "=" expression )  "in" expression) let-exp)
    ;;Expression ::= cond { experssion ==> expression }* end
    ;;               cond-exp (list-of-cond list-of-exp)
    (expression ( "cond" "{" (arbno expression "==>" expression) "}") cond-exp)
    ;;Expression ::= unpack {identifier}* = expression in expression
    ;;               unpack-exp (list-of-identifier exp1 exp2)
    (expression ( "unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
    ;;Expression ::= proc {identifier}*(,) expression
    ;;               proc-exp (vars body)
    (expression ( "proc" "(" (separated-list identifier ",")  ")" expression) proc-exp)
    ;;Expression ::= (expression {expression}* )
    ;;               call-exp (rator rands)
    (expression ( "(" expression (arbno expression) ")" ) call-exp)
    (expression (inner-operator) innerop-exp)
    (inner-operator (none-op) none-op)
    (inner-operator (binary-op) binary-op)
    (inner-operator (unary-op) unary-op)
    ))

(sllgen:make-define-datatypes scanner-spec-let grammar-let)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-let grammar-let))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-let grammar-let))

#| Expressed values for the LET language |#
(define-datatype expval expval?
  (num-val
   (num number?))
  #| (bool-val |#
  #|  (bool boolean?)) |#
  (list-val
   (elist explist?))
  (proc-val
   (proc proc?))
  (innerop-val
   (innerop inner-operator?))
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
#| (define expval->bool |#
#|   (lambda (val) |#
#|     (cases expval val |#
#|       (bool-val (bool) bool) |#
#|       (else expval-extractor-error 'bool val)))) |#
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else expval-extractor-error 'num val))))
(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (elist) elist)
      (else expval-extractor-error 'list val))))
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else expval-extractor-error 'proc val))))
(define expval->schemeval
  (lambda (val)
    (cases expval val
      (num-val (num)
               num)
      (list-val (elist)
                (explist->schemeval elist))
      (else eopl:error 'expval->schemeval "~s can not cast to schemeval" val)
      )))
(define explist->schemeval
  (lambda (elist)
    (cases explist elist
      (empty-list () '())
      (cons-val (val elist)
                (cons (expval->schemeval val)
                      (explist->schemeval elist))))))
(define-datatype explist explist?
  (empty-list)
  (cons-val
   (val expval?)
   (list2 explist?)))
(define explist->car
  (lambda (elist)
    (cases explist elist
      (empty-list ()
                  (explist-extractor-error '()))
      (cons-val (val _)
                val))))
(define explist->cdr
  (lambda (elist)
    (cases explist elist
      (empty-list ()
                  (explist-extractor-error '()))
      (cons-val (val list2)
                list2))))
(define explist->null?
  (lambda (elist)
    (cases explist elist
      (empty-list ()
                  #t)
      (else
       #f))))
;; Define procedure data type by scheme procedure
(define procedure
  (lambda (vars body env)
    (lambda (vals)
      (value-of body (extend-env* vars vals env)))))
;; Proc * Val -> ExpVal
(define apply-procedure
  (lambda (proc vals)
    (proc vals)))
(define proc?
  (lambda (proc)
    (procedure? proc)))

#| Interpreter for the LET language |#
;; run : string -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
                (extend-env 'v (num-val 5)
                            (extend-env 'x (num-val 10)
                                        (empty-env))))))

;; Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))
;; Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num)
                 (num-val num))
      (var-exp (var)
               (apply-env env var))
      (innerop-exp (inner-op)
                   (innerop-val inner-op))
      (if-exp (exp1 exp2 exp3)
              (if (expval->bool (value-of exp1 env))
                  (value-of exp2 env)
                  (value-of exp3 env)))
      (let-exp (op vars exps body)
               ((let-operator op) vars exps body env))
      (cond-exp (exps1 exps2)
                (cond-operator exps1 exps2 env))
      (unpack-exp (vars exp1 exp2)
                  (unpack-operator vars exp1 exp2 env))
      (proc-exp (vars body)
                (proc-val (procedure vars body env)))
      (call-exp (exp1 rands)
                (let ((rator (value-of exp1 env)))
                  (cases expval rator
                    (proc-val (proc)
                              (apply-procedure proc
                                               (for-each-list rands
                                                              (lambda (rand)
                                                                (value-of rand env)))))
                    (innerop-val (innerop)
                                 (cases inner-operator innerop
                                   (none-op (op)
                                            ((none-operator op) env))
                                   (binary-op (op)
                                              ((binary-operator op) (car rands) (cadr rands) env))
                                   (unary-op (op)
                                             ((unary-operator op) (car rands) env))))
                    (else
                     (eopl:error 'call-exp "can not apply on expval ~s" rator))
                    )))
      )))
(define make-arithmetic-op
  (lambda (op)
    (lambda (exp1 exp2 env)
      (num-val (op
                (expval->num (value-of exp1 env))
                (expval->num (value-of exp2 env)))))))
(define make-num-pred-op
  (lambda (pred)
    (lambda (exp1 exp2 env)
      (if (pred
           (expval->num (value-of exp1 env))
           (expval->num (value-of exp2 env)))
          (bool-val #t)
          (bool-val #f)
          ))))
(define zero?-op
  (lambda (exp1 env)
    (if (zero? (expval->num (value-of exp1 env)))
        (bool-val #t)
        (bool-val #f)
        )))
(define minus-op
  (lambda (exp1 env)
    (num-val (- (expval->num (value-of exp1 env))))))
(define car-op
  (lambda (exp1 env)
    (explist->car (expval->list (value-of exp1 env)))))
(define cdr-op
  (lambda (exp1 env)
    (list-val (explist->cdr (expval->list (value-of exp1 env))))))
(define null?-op
  (lambda (exp1 env)
    (bool-val (explist->null? (expval->list (value-of exp1 env))))))
(define cons-op
  (lambda (exp1 exp2 env)
    (list-val
     (cons-val (value-of exp1 env)
               (expval->list (value-of exp2 env))))))
(define emptylist-op
  (lambda (env)
    (list-val
     (empty-list))))
(define cond-operator
  (lambda (exps1 exps2 env)
    (if (null? exps1)
        (eopl:error 'cond "None of the tests succeeds!")
        (if (expval->bool (value-of (car exps1) env))
            (value-of (car exps2) env)
            (cond-operator (cdr exps1) (cdr exps2) env)))))
(define let-extend-env
  (lambda (vars exps env)
    (if (null? vars)
        env
        (extend-env (car vars) (value-of (car exps) env)
                    (let-extend-env (cdr vars) (cdr exps) env)))))
(define let*-extend-env
  (lambda (vars exps env)
    (if (null? vars)
        env
        (let*-extend-env (cdr vars) (cdr exps)
                         (extend-env (car vars) (value-of (car exps) env) env)))))
(define let-op
  (lambda (extend-env-fun)
    (lambda (vars exps body env)
      (let ((new-env (extend-env-fun vars exps env)))
        (value-of body new-env)))))
(define extend-env-from-elist
  (lambda (vars elist env)
    (if (null? vars)
        env
        (if (explist->null? elist)
            (eopl:error 'unpack "elements num not match!")
            (extend-env-from-elist (cdr vars) (explist->cdr elist)
                                   (extend-env (car vars) (explist->car elist) env))))))
(define unpack-operator
  (lambda (vars exp1 exp2 env)
    (let ((elst (expval->list (value-of exp1 env))))
      (value-of exp2 (extend-env-from-elist vars elst env)))))
(define make-fun-table
  (lambda lst
    (lambda (op)
      (cdr (assoc op lst)))))
(define binary-operator
  (make-fun-table
   (cons "+" (make-arithmetic-op +))
   (cons "-" (make-arithmetic-op -))
   (cons "*" (make-arithmetic-op *))
   (cons "/" (make-arithmetic-op /))
   (cons "equal?" (make-num-pred-op equal?))
   (cons "greater?" (make-num-pred-op >))
   (cons "less?" (make-num-pred-op <))
   (cons "cons" cons-op)
   ))
(define unary-operator
  (make-fun-table
   (cons "zero?" zero?-op)
   (cons "minus" minus-op)
   (cons "car" car-op)
   (cons "cdr" cdr-op)
   (cons "null?" null?-op)
   ))
(define none-operator
  (make-fun-table
   (cons "emptylist" emptylist-op)
   ))
(define let-operator
  (make-fun-table
   (cons "let" (let-op let-extend-env))
   (cons "let*" (let-op let*-extend-env))
   ))
