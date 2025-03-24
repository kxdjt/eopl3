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

#| Syntax data types for the LET language |#
(define scanner-spec-let
  '((white-sp (whitespace) skip)
    (number ((or digit (concat "-" digit)) (arbno digit)) number)
    (identifier (letter (arbno (or letter digit "?"))) symbol)
    (binary-op ((or "+" "-" "*" "/" "equal?" "greater?" "less?" "cons")) string)
    (unary-op ((or "minus" "zero?" "car" "cdr" "null?")) string)
    (none-op ((or "emptylist")) string)
    (let-op ((or "let" "let*" "letmutable" "letmutable*")) string)
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
    ;;Expression ::= begin Expression {; Expression}* end
    ;;               begin-exp (exp1 exps2)
    (expression ( "begin" expression (arbno ";" expression) "end") begin-exp)
    (expression (inner-operator) innerop-exp)
    (inner-operator (none-op) none-op)
    (inner-operator (binary-op) binary-op)
    (inner-operator (unary-op) unary-op)
    ;;-----------Store Interface------------------
    ;;Expression ::= set identifier = expression
    (expression ("set" identifier "=" expression) set-exp)
    ))

(sllgen:make-define-datatypes scanner-spec-let grammar-let)

(define just-scan
  (sllgen:make-string-scanner scanner-spec-let grammar-let))

(define scan&parse
  (sllgen:make-string-parser scanner-spec-let grammar-let))

#| Interpreter for the LET language |#
;; run : string -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
(define init-senv
  (lambda ()
    (extend-senv 'i (num-val 1)
                 (extend-senv 'v (num-val 5)
                              (extend-senv 'x (num-val 10)
                                           (empty-senv))))))

;; Program -> ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (cases answer (value-of exp1 (init-senv))
                    (an-answer (eval store)
                               eval))))))

;; Define Env Value
(define-datatype envval envval?
  (mutable-val
   (ref number?))
  (immutable-val
   (eval expval?))
  )

;; Define Expressed valus
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
(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (elist) elist)
      (else expval-extractor-error 'list val))))

;; Define list expval
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

;; Define procedure data type by schema procedure
(define proc?
  (lambda (val)
    (procedure? val)))
(define procedure
  (lambda (vars body env)
    (lambda (vals store . _)
      (value-of body (extend-senv* vars vals (cons env store))))))
(define traceproc
  (lambda (vars body env)
    (lambda (vals store . _)
      (printf "tranceproc: vars:~s vals:~s body:~s\n" vars vals body)
      (let ((res (value-of body (extend-senv* vars vals (cons env store)))))
        (printf "tranceproc: res: ~s\n" res)
        res))))
(define dynamicproc
  (lambda (vars body . _)
    (lambda (vals store env)
      (value-of body (extend-senv* vars vals (cons env store))))))
;; Proc * Val -> ExpVal
(define apply-procedure
  (lambda (proc vals store env)
    (proc vals store env)))

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
      (extend-store (ref eval _) (+ ref 1)))))
(define store->findref
  (lambda (s s-ref)
    (cases store s
      (extend-store (ref eval nexts)
                    (cond
                      ((equal? s-ref ref) eval)
                      ((> s-ref ref)
                       (eopl:error 'store->findref "invalid ref ~s in store ~s" s-ref s))
                      (else
                       (store->findref nexts s-ref))))
      (empty-store ()
                   (eopl:error 'store->findref "invalid ref ~s in store ~s" s-ref s)))))
(define store->setref
  (lambda (s s-ref eval)
    (cases store s
      (empty-store ()
                   (eopl:error 'store->setref "invalid ref ~s in store ~s" s-ref s))
      (extend-store (ref old-eval nexts)
                    (cond
                      ((equal? s-ref ref)
                       (extend-store
                        ref
                        eval
                        nexts))
                      ((> s-ref ref)
                       (eopl:error 'store->setref "invalid ref ~s in store ~s" s-ref s))
                      (else
                       (let ((ns (store->setref nexts s-ref eval)))
                         (extend-store
                          ref
                          old-eval
                          ns))))))))

(define extend-env-mutable
  (lambda (var ref env)
    (extend-env var
                (mutable-val ref)
                env)))
(define extend-env-immutable
  (lambda (var eval env)
    (extend-env var
                (immutable-val eval)
                env)))

;; Define senv -- (env . store)
(define empty-senv
  (lambda ()
    (cons (empty-env) (empty-store))))
;; var*expval*(env . store) -> (newenv . newstore)
(define extend-senv
  (lambda (var eval senv)
    (let* ((env (car senv))
           (store (cdr senv))
           (ref (store->nextref store)))
      (cons
       (extend-env-mutable var ref env)
       (extend-store ref eval store)))))
(define extend-senv-immutable
  (lambda (var eval senv)
    (let* ((env (car senv))
           (store (cdr senv)))
      (cons
       (extend-env-immutable var eval env)
       store))))
;; vars**expval*(env . store) -> (newenv . newstore)
(define extend-senv*-imp
  (lambda (extend-fun vars evals senv)
    (if (null? vars)
        senv
        (extend-senv*-imp extend-fun
                          (cdr vars) (cdr evals)
                          (extend-fun (car vars)
                                      (car evals)
                                      senv)))))
(define extend-senv*
  (lambda (vars evals senv)
    (extend-senv*-imp extend-senv
                      vars
                      evals
                      senv)))
(define extend-senv*-immutable
  (lambda (vars evals senv)
    (extend-senv*-imp extend-senv-immutable
                      vars
                      evals
                      senv)))
;; (env . store)*var -> val
(define apply-senv
  (lambda (senv var)
    (let* ((env (car senv))
           (store (cdr senv))
           (env-val (apply-env env var)))
      (cases envval env-val
        (mutable-val (ref)
                     (store->findref store ref))
        (immutable-val (eval)
                       eval)
        ))))
;; Define Answer -- include expval and store
(define-datatype answer answer?
  (an-answer
   (eval expval?)
   (s store?)))
(define answer->eval
  (lambda (aw)
    (cases answer aw
      (an-answer (eval _) eval))))
(define answer->store
  (lambda (aw)
    (cases answer aw
      (an-answer (_ s) s))))

(define value-of
  (lambda (exp senv)
    (let* ((env (car senv))
           (store (cdr senv)))
      (debug-info "value-of" "exp:~s store:~s\n" exp store)
      (let ((res (value-of-imp exp senv)))
        (debug-info "value-of" "res:~s\n" res)
        res))))

(define value-of-imp
  (lambda (exp senv)
    (let* ((env (car senv))
           (store (cdr senv)))
      (cases expression exp
        (const-exp (num)
                   (an-answer
                    (num-val num)
                    store))
        (var-exp (var)
                 (an-answer
                  (apply-senv senv var)
                  store))
        (innerop-exp (inner-op)
                     (an-answer
                      (innerop-val inner-op)
                      store))
        (if-exp (exp1 exp2 exp3)
                (let ((aw1 (value-of exp1 senv)))
                  (if (expval->bool (answer->eval aw1))
                      (value-of exp2 (cons env (answer->store aw1)))
                      (value-of exp3 (cons env (answer->store aw1))))))
        (let-exp (op vars exps1 exp2)
                 ((let-operator op) vars exps1 exp2 senv))
        (letrec-exp (proc-names list-of-vars exps1 exp2)
                    (value-of exp2
                              (extend-senv-rec*-immutable proc-names list-of-vars exps1 senv)))
        (cond-exp (exps1 exps2)
                  (cond-operator exps1 exps2 senv))
        (proc-exp (op vars exp)
                  (an-answer
                   ((proc-operator op) vars exp env)
                   store))
        (call-exp (exp1 exps2)
                  (call-operator exp1 exps2 store env))
        (begin-exp (exp1 exps2)
                   (begin-operator exp1 exps2 store env))
        (set-exp (ident exp1)
                 (set-operator ident exp1 senv))
        ))))


(define make-fun-table
  (lambda lst
    (lambda (op)
      (cdr (assoc op lst)))))


(define let-extend-env-imp
  (lambda (vars exps1 senv extend-senv-fun)
    (let* ((env (car senv))
           (store (cdr senv)))
      (foldl (lambda(var exp res)
               (let* ((last-env (car res))
                      (last-store (cdr res))
                      (aw (value-of exp (cons env last-store))))
                 (extend-senv-fun var
                              (answer->eval aw)
                              (cons last-env
                                    (answer->store aw)))))
             senv
             vars
             exps1))))
(define let*-extend-env-imp
  (lambda (vars exps1 senv extend-senv-fun)
    (let* ((env (car senv))
           (store (cdr senv)))
      (foldl (lambda(var exp res)
               (let* ((last-env (car res))
                      (last-store (cdr res))
                      (aw (value-of exp res)))
                 (extend-senv-fun var
                              (answer->eval aw)
                              (cons last-env
                                    (answer->store aw)))))
             senv
             vars
             exps1))))
(define let-extend-env-immutable
  (lambda (vars exps1 senv)
    (let-extend-env-imp vars exps1 senv extend-senv-immutable)))
(define let-extend-env
  (lambda (vars exps1 senv)
    (let-extend-env-imp vars exps1 senv extend-senv)))
(define let*-extend-env-immutable
  (lambda (vars exps1 senv)
    (let*-extend-env-imp vars exps1 senv extend-senv-immutable)))
(define let*-extend-env
  (lambda (vars exps1 senv)
    (let*-extend-env-imp vars exps1 senv extend-senv)))
(define let-op
  (lambda (extend-env-fun)
    (lambda (vars exps body senv)
      (let ((newsenv (extend-env-fun vars exps senv)))
        (value-of body newsenv)))))
(define let-operator
  (make-fun-table
   (cons "let" (let-op let-extend-env-immutable))
   (cons "let*" (let-op let*-extend-env-immutable))
   (cons "letmutable" (let-op let-extend-env))
   (cons "letmutable*" (let-op let*-extend-env))
   ))
(define extend-senv-rec*-immutable
  (lambda (proc-names list-of-vars exps senv)
    (let ((env (car senv))
          (store (cdr senv)))
      (define make-env
        (cons
          (lambda (s-var)
            (let ((res (ormap (lambda(proc-name vars exp1)
                                (if (equal? s-var proc-name)
                                    (immutable-val (proc-val (procedure vars exp1
                                                        make-env)))
                                    #f))
                              proc-names
                              list-of-vars
                              exps)))
              (if (not res)
                  (apply-env env s-var)
                  res)))
          (lambda (s-var)
            (if (list-member? s-var proc-names)
                #t
                (has-binding? env s-var)))))
      (cons
        make-env
       store))))
(define extend-senv-rec*
  (lambda (proc-names list-of-vars exps senv)
    (define make-env
      (lambda (vars s-ref env)
        (if (null? vars)
            env
            (make-env (cdr vars) (+ s-ref 1)
                      (extend-env-mutable (car vars) s-ref env)))))
    (define make-store
      (lambda (lvars exps s-ref env store)
        (if (null? exps)
            store
            (make-store (cdr lvars)
                        (cdr exps)
                        (+ s-ref 1)
                        env
                        (extend-store s-ref
                                      (proc-val
                                       (procedure (car lvars)
                                                  (car exps)
                                                  env))
                                      store)))))
    (let* ((env (car senv))
           (store (cdr senv))
           (start-ref (store->nextref store))
           (nenv (make-env proc-names start-ref env))
           (nstore (make-store list-of-vars exps start-ref nenv store)))
      (cons nenv nstore))))

(define cond-operator
  (lambda (exps1 exps2 senv)
    (if (null? exps1)
        (eopl:error 'cond "None of the tests succeeds!")
        (let ((aw (value-of (car exps1) senv)))
          (if (expval->bool (answer->eval aw))
              (value-of (car exps2) (cons (car senv) (answer->store aw)))
              (cond-operator (cdr exps1) (cdr exps2) (cons (car senv) (answer->store aw))))))))

(define make-procedure-val
  (lambda (proc)
    (lambda args
      (proc-val
       (apply proc args)))))
(define proc-operator
  (make-fun-table
   (cons "proc" (make-procedure-val procedure))
   (cons "traceproc" (make-procedure-val traceproc))
   (cons "dyproc" (make-procedure-val dynamicproc))
   ))

(define call-operator
  (lambda (exp1 exps2 store env)
    (let* ((aw (value-of exp1 (cons env store)))
           (store (answer->store aw))
           (rator (answer->eval aw)))
      (let* ((res (foldr (lambda(exp res)
                           (let* ((vals (car res))
                                  (last-store (cdr res))
                                  (aw (value-of exp (cons env last-store))))
                             (cons
                              (cons (answer->eval aw) vals)
                              (answer->store aw))))
                         (cons '() store)
                         exps2))
             (rands (car res))
             (store (cdr res)))
        (cases expval rator
          (proc-val (proc)
                    (apply-procedure proc rands store env))
          (innerop-val (innerop)
                       (an-answer
                        (cases inner-operator innerop
                          (none-op (op)
                                   ((none-operator op)))
                          (binary-op (op)
                                     ((binary-operator op) (car rands) (cadr rands)))
                          (unary-op (op)
                                    ((unary-operator op) (car rands))))
                        store))
          (else
           (eopl:error 'call-exp "can not apply on expval ~s" rator)))))))

(define make-arithmetic-op
  (lambda (op)
    (lambda (val1 val2)
      (num-val (op
                (expval->num val1)
                (expval->num val2))))))
(define make-num-pred-op
  (lambda (pred)
    (lambda (val1 val2)
      (if (pred
           (expval->num val1)
           (expval->num val2))
          (bool-val #t)
          (bool-val #f)))))
(define zero?-op
  (lambda (val1)
    (if (zero? (expval->num val1))
        (bool-val #t)
        (bool-val #f))))
(define minus-op
  (lambda (val1)
    (num-val (- (expval->num val1)))))
(define car-op
  (lambda (val1)
    (explist->car (expval->list val1))))
(define cdr-op
  (lambda (val1)
    (list-val (explist->cdr (expval->list val1)))))
(define null?-op
  (lambda (val1)
    (bool-val (explist->null? (expval->list val1)))))
(define cons-op
  (lambda (val1 val2)
    (list-val
     (cons-val val1
               (expval->list val2)))))
(define emptylist-op
  (lambda ()
    (list-val
     (empty-list))))
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

(define begin-operator
  (lambda (exp1 exps2 store env)
    (let ((aw (value-of exp1 (cons env store))))
      (if (null? exps2)
          aw
          (begin-operator (car exps2) (cdr exps2)
                          (answer->store aw) env)))))

(define set-operator
  (lambda (ident exp1 senv)
    (let* ((aw (value-of exp1 senv))
           (val (answer->eval aw))
           (env-val (apply-env (car senv) ident)))
      (cases envval env-val
        (mutable-val (ref)
          (an-answer
            val
            (store->setref
              (answer->store aw)
              ref
              val)))
        (else
          (eopl:error 'set-opt "invalid type for envval ~s:~s" ident env-val))))))
