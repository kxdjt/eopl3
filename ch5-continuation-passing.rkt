#lang racket

(require (except-in eopl list-of))
#| (require "ch2-ribcage-env.rkt") |#
(require "ch3-enironment.rkt")
(require "ch2-datatype-utils.rkt")

(provide run)

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))
(define explist-extractor-error
  (lambda (val)
    (eopl:error 'explist-extractors "expected: pair? given: ~s" val)))
(define expenv-extractor-error
  (lambda (var exp1)
    (eopl:error 'expenv-extractors "unbind var ~s in ~s" var exp1)))

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
    (let ((proc-env (extract-freevar-env vars body env (empty-env))))
      (lambda (vals env cont)
        (value-of/k body
                    (extend-env* vars vals proc-env)
                    cont)))))
#| (define traceproc |#
#|   (lambda (vars body env) |#
#|     (let ((env (extract-freevar-env vars body env (empty-env)))) |#
#|       (lambda (vals cont) |#
#|         (printf "tranceproc: vars:~s vals:~s body:~s\n" vars vals body) |#
#|         (let ((res (value-of/k body |#
#|                                (extend-env* vars vals env) |#
#|                                cont))) |#
#|           (printf "tranceproc: res: ~s\n" res) |#
#|           res))))) |#
(define dynamicproc
  (lambda (vars body . _)
    (lambda (vals env cont)
      (value-of/k body
                  (extend-env* vars vals env)
                  cont))))
;; Proc * Vals * cont -> FinalAnswer
(define apply-procedure
  (lambda (proc vals env cont)
    (proc vals env cont)))
(define proc?
  (lambda (proc)
    (procedure? proc)))

(define extend-env-rec*
  (lambda (proc-names list-of-vars exps env)
    (cons
     (lambda (search-var)
       (let ((res (ormap (lambda(proc-name vars exp1)
                           (if (equal? search-var proc-name)
                               (proc-val (procedure vars exp1
                                                    (extend-env-rec* proc-names list-of-vars exps env)))
                               #f))
                         proc-names
                         list-of-vars
                         exps)))
         (if (not res)
             (apply-env env search-var)
             res)))
     (lambda (search-var)
       (if (list-member? search-var proc-names)
           #t
           (has-binding? env search-var))))))
(define extend-env-rec
  (lambda (proc-name vars exp1 env)
    (cons
     (lambda (search-var)
       (if (equal? search-var proc-name)
           (proc-val (procedure vars exp1
                                (extend-env-rec proc-name vars exp1 env)))
           (apply-env env search-var)))
     (lambda (search-var)
       (if (equal? proc-name search-var)
           #t
           (has-binding? env search-var))))))

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

;; Program -> FinalAnswer = ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of/k exp1 (init-env) (end-cont))))))
;; Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp env cont)
    (cases expression exp
      (const-exp (num)
                 (apply-cont cont  (num-val num)))
      (var-exp (var)
               (apply-cont cont (apply-env env var)))
      (innerop-exp (inner-op)
                   (apply-cont cont (innerop-val inner-op)))
      (if-exp (exp1 exp2 exp3)
              (value-of/k exp1 env
                          (if-cont exp2 exp3 env cont)))
      (let-exp (op vars exps body)
               (if (null? exps)
                   (value-of/k body env cont)
                   (value-of/k (car exps) env
                               (make-let-cont-by-op
                                op vars (cdr exps) body cont env))))
      (letrec-exp (proc-names list-of-vars exps1 exp2)
                  (value-of/k exp2
                              (extend-env-rec* proc-names list-of-vars exps1 env)
                              cont))
      (cond-exp (exps1 exps2)
                (value-of/k (car exps1)
                            env
                            (cond-cont (cdr exps1) exps2 env cont)))
      (proc-exp (op vars body)
                (apply-cont cont
                            (proc-val ((proc-operator op) vars body env))))
      (call-exp (exp1 rands)
                (value-of/k exp1
                            env
                            (call-cont rands env cont))))))

;; procedure representation of continuation
;; Cont * Eval -> FinalAnswer
(define apply-cont
  (lambda (cont eval)
    (cont eval)))
;; continuations
(define end-cont
  (lambda ()
    (lambda (eval)
      (printf "End of Computation, ~s\n" eval))))
(define if-cont
  (lambda (exp2 exp3 env cont)
    (lambda (eval)
      (if (expval->bool eval)
          (value-of/k exp2 env cont)
          (value-of/k exp3 env cont)))))
(define let*-cont
  (lambda (vars exps body cont env)
    (lambda (eval)
      (let ((new-env (extend-env (car vars) eval env)))
        (if (null? exps)
            (value-of/k body new-env cont)
            (value-of/k (car exps)
                        new-env
                        (let*-cont (cdr vars) (cdr exps) body cont new-env)))))))
(define let-cont
  (lambda (vars exps body cont ori-env env)
    (lambda (eval)
      (let ((new-env (extend-env (car vars) eval env)))
        (if (null? exps)
            (value-of/k body new-env cont)
            (value-of/k (car exps)
                        ori-env
                        (let-cont (cdr vars) (cdr exps) body cont ori-env new-env)))))))
(define cond-cont
  (lambda (exps1 exps2 env cont)
    (lambda (eval)
      (if (expval->bool eval)
          (value-of/k (car exps2) env cont)
          (value-of/k (car exps1)
                      env
                      (cond-cont (cdr exps1) (cdr exps2) env cont))))))
(define call-cont
  (lambda (rands env cont)
    (lambda (eval)
      (cases expval eval
        (innerop-val (innerop)
                     (cases inner-operator innerop
                       (none-op (op)
                                (apply-cont cont ((none-operator op))))
                       (binary-op (op)
                                  (value-of/k (car rands)
                                              env
                                              (binary-op-cont1 op (cdr rands) env cont)))
                       (unary-op (op)
                                 (value-of/k (car rands)
                                             env
                                             (unary-op-cont op cont)))))
        (proc-val (proc)
                  (if (null? rands)
                      (apply-procedure proc '() env cont)
                      (value-of/k (car rands)
                                  env
                                  (proc-cont proc (cdr rands) env cont '()))))
        (else
         (eopl:error 'call-exp "can not apply on expval ~s" eval))
        ))))
(define binary-op-cont1
  (lambda (op rands env cont)
    (lambda (eval)
      (value-of/k (car rands)
                  env
                  (binary-op-cont2 op eval cont)))))
(define binary-op-cont2
  (lambda (op eval1 cont)
    (lambda (eval2)
      (apply-cont cont
                  ((binary-operator op) eval1 eval2)))))
(define unary-op-cont
  (lambda (op cont)
    (lambda (eval)
      (apply-cont cont
                  ((unary-operator op) eval)))))
(define proc-cont
  (lambda (proc rands env cont vals)
    (lambda (eval)
      (let ((new-vals (append vals (list eval))))
        (if (null? rands)
            (apply-procedure proc new-vals env cont)
            (value-of/k (car rands)
                        env
                        (proc-cont proc (cdr rands) env cont new-vals)))))))

(define make-let-cont-by-op
  (lambda (op vars exps body cont env)
    (if (equal? op "let*")
        (let*-cont vars exps body cont env)
        (let-cont vars exps body cont env env))))

(define make-arithmetic-op
  (lambda (op)
    (lambda (eval1 eval2)
      (num-val (op
                (expval->num eval1)
                (expval->num eval2))))))
(define make-num-pred-op
  (lambda (pred)
    (lambda (eval1 eval2)
      (if (pred
           (expval->num eval1)
           (expval->num eval2))
          (bool-val #t)
          (bool-val #f)
          ))))
(define cons-op
  (lambda (eval1 eval2)
    (list-val
     (cons-val eval1
               (expval->list eval2)))))
(define zero?-op
  (lambda (eval)
    (if (zero? (expval->num eval))
        (bool-val #t)
        (bool-val #f)
        )))
(define minus-op
  (lambda (eval)
    (num-val (- (expval->num eval)))))
(define car-op
  (lambda (eval)
    (explist->car (expval->list eval))))
(define cdr-op
  (lambda (eval)
    (list-val (explist->cdr (expval->list eval)))))
(define null?-op
  (lambda (eval)
    (bool-val (explist->null? (expval->list eval)))))
(define emptylist-op
  (lambda ()
    (list-val
     (empty-list))))
(define extract-freevar-env-from-explist
  (lambda (bindvars exps env nenv)
    (foldl (lambda(exp nenv)
             (extract-freevar-env bindvars exp env nenv))
           nenv
           exps)))
(define extract-freevar-env-from-let*
  (lambda (bindvars vars exps env nenv)
    (if (null? vars) nenv
        (let ((new-bindvars (if (list-member? (car vars) bindvars)
                                bindvars
                                (cons (car vars) bindvars))))
          (extract-freevar-env-from-let* new-bindvars
                                         (cdr vars)
                                         (cdr exps)
                                         env
                                         (extract-freevar-env bindvars (car exps) env nenv))))))
(define extract-freevar-env
  (lambda (bind-vars exp1 env nenv)
    (cases expression exp1
      (var-exp (var)
               (if (and
                    (not (list-member? var bind-vars))
                    (not (has-binding? nenv var)))
                   (if (has-binding? env var)
                       (extend-env var (apply-env env var) nenv)
                       (expenv-extractor-error var exp1))
                   nenv))
      (if-exp (exp1 exp2 exp3)
              (extract-freevar-env bind-vars exp3 env
                                   (extract-freevar-env bind-vars exp2 env
                                                        (extract-freevar-env bind-vars exp1 env nenv))))
      (let-exp (let-op vars exps1 exp2)
               (let ((nenv (if (equal? let-op "let")
                               (extract-freevar-env-from-explist bind-vars exps1 env nenv)
                               (extract-freevar-env-from-let* bind-vars vars exps1 env nenv))))
                 (extract-freevar-env (append bind-vars vars) exp2 env nenv)))
      (cond-exp (exps1 exps2)
                (extract-freevar-env-from-explist bind-vars exps2 env
                                                  (extract-freevar-env-from-explist bind-vars exps1 env nenv)))
      (proc-exp (op vars exp1)
                (extract-freevar-env (append bind-vars vars) exp1 env nenv))
      (call-exp (exp1 exps2)
                (extract-freevar-env-from-explist bind-vars exps2 env
                                                  (extract-freevar-env bind-vars exp1 env nenv)))
      (else nenv))))
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
(define proc-operator
  (make-fun-table
   (cons "proc" procedure)
   #| (cons "traceproc" traceproc) |#
   (cons "dyproc" dynamicproc)
   ))
