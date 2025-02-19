#lang racket

(require (except-in eopl list-of))
#| (require "ch2-ribcage-env.rkt") |#
(require "ch3-enironment.rkt")
(require "ch2-datatype-utils.rkt")

(provide run translate)

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
    ;;Expression ::= unpack {identifier}* = expression in expression
    ;;               unpack-exp (list-of-identifier exp1 exp2)
    (expression ( "unpack" (arbno identifier) "=" expression "in" expression) unpack-exp)
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

(define translate
  (lambda (string)
    (translation-of-program
     (scan&parse string))))

(define run
  (lambda (string)
    (value-of-program
     (translation-of-program
      (scan&parse string)))))

(define init-env
  (lambda ()
    (extend-env 'i (num-val 1)
                (extend-env 'v (num-val 5)
                            (extend-env 'x (num-val 10)
                                        (empty-env))))))

;; Program * Senv -> Name-less-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (a-program (translation-of exp1))))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

#| Expressed values for the LET language |#
(define-datatype expval expval?
  (num-val
   (num number?))
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

;; Define ExpList
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
(define proc?
  (lambda (proc)
    (procedure? proc)))

(define procedure
  (lambda (vars body env)
    (lambda (vals . _)
      (let ((env (extract-freevar-env vars body env (empty-env))))
        (value-of body (extend-env* vars vals env))))))
(define traceproc
  (lambda (vars body env)
    (lambda (vals . _)
      (printf "tranceproc: vars:~s vals:~s body:~s\n" vars vals body)
      (let ((env (extract-freevar-env vars body env (empty-env))))
        (let ((res (value-of body (extend-env* vars vals env))))
          (printf "tranceproc: res: ~s\n" res)
          res)))))
(define dynamicproc
  (lambda (vars body . _)
    (lambda (vals env)
      (value-of body (extend-env* vars vals env)))))
;; Proc * Val -> ExpVal
(define apply-procedure
  (lambda (proc vals env)
    (proc vals env)))

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

;; Define translation
;; Exps * Senv -> Name-less-exps
(define translation-of-exps
  (lambda (exps)
    (map(lambda(exp1)
          (translation-of exp1))
        exps)))

(define alter-callee-in-call-exp
  (lambda (vars exps exp1)
    (if (null? vars)
        exp1
        (cases expression exp1
          (var-exp (s-var)
                   (if (equal? (car vars) s-var)
                       (car exps)
                       (alter-callee-in-call-exp (cdr vars) (cdr exps) exp1)))
          (else
           exp1)))))
(define find-alter-callee-in-let
  (lambda (vars exps exp)
    (cases expression exp
      (var-exp (s-var)
               (ormap (lambda (var exp)
                        (if (equal? s-var var)
                            (cons var exp)
                            #f))
                      vars exps))
      (else #f))))

(define occurs-free?-exps
  (lambda (s-var exps)
    (ormap (lambda(exp)
             (occurs-free? s-var exp))
           exps)))

(define occurs-free?
  (lambda (s-var exp)
    (cases expression exp
      (if-exp (exp1 exp2 exp3)
              (occurs-free?-exps s-var
                                 (list exp1 exp2 exp3)))
      (var-exp (var)
               (equal? var s-var))
      (let-exp (opt vars exps exp1)
               (or (index-of vars s-var)
                   (occurs-free? s-var exp1)
                   (occurs-free?-exps s-var exps)))
      (cond-exp (exps1 exps2)
                (or (occurs-free?-exps s-var exps1)
                    (occurs-free?-exps s-var exps2)))
      (unpack-exp (vars exp1 exp2)
                  (or (occurs-free? s-var exp1)
                      (index-of vars s-var)
                      (occurs-free?-exps s-var exp2)))
      (proc-exp (op vars exp1)
                (or (index-of vars s-var)
                    (occurs-free? s-var exp1)))
      (call-exp (exp1 exps2)
                (or (occurs-free? s-var exp1)
                    (occurs-free?-exps s-var exps2)))
      (else #f))))

(define alter-call-exp-in-let
  (lambda (op vars exps1 exp)
    (cases expression exp
      (call-exp (exp1 exps2)
                (let ((find-res
                       (find-alter-callee-in-let vars exps1 exp1)))
                  (if (not find-res)
                      (let-exp op vars
                               (translation-of-exps exps1)
                               (translation-of exp))
                      (let ((new-exp
                             (call-exp (translation-of (cdr find-res))
                                       (translation-of-exps exps2))))
                        (if (not (occurs-free?-exps (car find-res) exps1))
                            (let* ((idx (index-of vars (car find-res)))
                                   (new-vars (remove-by-idx vars idx))
                                   (new-exps (remove-by-idx exps1 idx)))
                              (if (null? new-vars)
                                  new-exp
                                  (let-exp op new-vars
                                           (translation-of-exps new-exps)
                                           new-exp)))
                            (let-exp op vars
                                     (translation-of-exps exps1)
                                     new-exp))))))
      (else
        (let-exp op vars
                 (translation-of-exps exps1)
                 (translation-of exp)))))) 

(define translation-of-let
  (lambda (op vars exps1 exp2)
    (let ((trans-exp2
           (cases expression exp2
             (call-exp (exp1 exps2)
                       (call-exp
                        (translation-of
                         (alter-callee-in-call-exp vars exps1 exp1))
                        (translation-of-exps exps2)))
             (else
              (translation-of exp2)))))
      (let-exp op vars
               (translation-of-exps exps1)
               trans-exp2))))

;;Exp * Senv -> Name-less-exp
(define translation-of
  (lambda (exp1)
    (cases expression exp1
      (const-exp (num)
                 (const-exp num))
      (if-exp (exp1 exp2 exp3)
              (if-exp
               (translation-of exp1)
               (translation-of exp2)
               (translation-of exp3)))
      (var-exp (var)
               (var-exp var))
      (let-exp (op vars exps1 exp2)
               (if (equal? op "let")
                   (alter-call-exp-in-let op vars exps1 exp2)
                   (let-exp op vars
                            (translation-of-exps exps1)
                            (translation-of exp2))))
      (letrec-exp (proc-names list-of-vars exps1 exp2)
                  (letrec-exp proc-names list-of-vars
                              (translation-of-exps exps1)
                              (translation-of exp2)))
      (cond-exp (exps1 exps2)
                (cond-exp
                 (translation-of-exps exps1)
                 (translation-of-exps exps2)))
      (unpack-exp (vars exp1 exp2)
                  (unpack-exp vars
                              (translation-of exp1)
                              (translation-of exp2)))
      (proc-exp (proc-op vars exp1)
                (proc-exp proc-op vars
                          (translation-of exp1)))
      (call-exp (exp1 exps2)
                (call-exp
                 (translation-of exp1)
                 (translation-of-exps exps2)))
      (innerop-exp (op)
                   (innerop-exp op))
      (else
       (eopl:error 'translation-of "untranslatable type ~s" exp1)))))

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
      (letrec-exp (proc-names list-of-vars exps1 exp2)
                  (value-of
                   exp2
                   (extend-env-rec* proc-names list-of-vars exps1 env)))
      (cond-exp (exps1 exps2)
                (cond-operator exps1 exps2 env))
      (unpack-exp (vars exp1 exp2)
                  (unpack-operator vars exp1 exp2 env))
      (proc-exp (op vars body)
                (proc-val ((proc-operator op) vars body env)))
      (call-exp (exp1 rands)
                (let ((rator (value-of exp1 env)))
                  (cases expval rator
                    (proc-val (proc)
                              (apply-procedure proc
                                               (map (lambda (rand)
                                                      (value-of rand env))
                                                    rands)
                                               env))
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
      (unpack-exp (vars exp1 exp2)
                  (extract-freevar-env (append bind-vars vars) exp2 env
                                       (extract-freevar-env bind-vars exp1 env nenv)))
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
(define let-operator
  (make-fun-table
   (cons "let" (let-op let-extend-env))
   (cons "let*" (let-op let*-extend-env))
   ))
(define proc-operator
  (make-fun-table
   (cons "proc" procedure)
   (cons "traceproc" traceproc)
   (cons "dyproc" dynamicproc)
   ))
