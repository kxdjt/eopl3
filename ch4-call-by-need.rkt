#lang racket

(require (except-in eopl list-of))
#| (require "ch2-ribcage-env.rkt") |#
(require "ch3-enironment.rkt")
(require "ch2-datatype-utils.rkt")

(provide run)

(define environment?
  (lambda (e)
    (and (procedure? (car e))
         (procedure? (cdr e)))))

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
    (let-op ((or "let" "let*" "letmutable" "letmutable*" "letlazy*")) string)
    (proc-op ((or "proc" "traceproc" "dyproc" "cbrrproc" "lazyproc")) string)
    (customdt-op ((or "newpair" "left" "right" "setleft" "setright"
                      "newarray" "arrayref" "arrayset" "arraylength"
                      )) string)
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
    (expression ( proc-op "(" (separated-list proc-arg ",")  ")" expression) proc-exp)
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
    (proc-arg (identifier) value-parg)
    (proc-arg ( "&" identifier) ref-parg)
    (expression (customdt-op) customdtop-exp)
    ;;-----------Store Interface------------------
    ;;Expression ::= set identifier = expression
    (expression ("set" identifier "=" expression) set-exp)
    ;;Expression ::= setdynamic identifier = expression during expression
    (expression ("setdynamic" identifier "=" expression "during" expression) setdynamic-exp)
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
  (an-env-val
   (mutable boolean?)
   (ref number?))
  )

(define mutable-val
  (lambda (eval)
    (an-env-val
     true
     eval)))
(define immutable-val
  (lambda (eval)
    (an-env-val
     false
     eval)))

(define apply-env->ref
  (lambda (env ident)
    (cases envval (apply-env env ident)
      (an-env-val (_ ref) ref))))

;; Define Thunk
(define-datatype thunk thunk?
  (an-thunk
   (exp1 expression?)
   (env environment?)))

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
  (mutpair-val
   (ref number?))
  (array-val
   (ref number?)
   (num number?))
  (customdtop-val
   (customdtop string?))
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
(define expval->mutpair
  (lambda (val)
    (cases expval val
      (mutpair-val (ref) ref)
      (else expval-extractor-error 'mutpair val))))
(define expval->array-ref
  (lambda (val)
    (cases expval val
      (array-val (ref num) ref)
      (else expval-extractor-error 'array val))))
(define expval->array-num
  (lambda (val)
    (cases expval val
      (array-val (ref num) num)
      (else expval-extractor-error 'array val))))

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

(define proc-arg->ident
  (lambda (parg)
    (cases proc-arg parg
      (value-parg (ident) ident)
      (ref-parg (ident) ident))))

;; Define procedure data type by schema procedure
(define proc?
  (lambda (val)
    (procedure? val)))
(define procedure
  (lambda (vars body env)
    (lambda (vals store exp-env)
      (value-of body (extend-proc-senv vars vals store exp-env env)))))
(define traceproc
  (lambda (vars body env)
    (lambda (vals store exp-env)
      (printf "tranceproc: vars:~s vals:~s body:~s\n" vars vals body)
      (let ((res (value-of body (extend-proc-senv vars vals store exp-env env))))
        #| (extend-senv* vars vals (cons env store))))) |#
        (printf "tranceproc: res: ~s\n" res)
        res))))
(define dynamicproc
  (lambda (vars body proc-env)
    (lambda (vals store env)
      (value-of body (extend-proc-senv vars vals store env env)))))
;; call-by-ref-result
(define cbrrproc
  (lambda (vars body env)
    (lambda (vals store exp-env)
      (let* ((new-proc-senv (extend-proc-senv-by-val vars vals store exp-env env))
             (aw (value-of body new-proc-senv))
             (new-store (copy-res-to-refparas vars
                                              vals
                                              (answer->store aw)
                                              exp-env
                                              (car new-proc-senv))))
        (an-answer
         (answer->eval aw)
         new-store)))))
;; call-by-need
(define lazyproc
  (lambda (vars body env)
    (lambda (vals store exp-env)
      (value-of body (extend-lazy-proc-senv vars vals store exp-env env)))))

;; Proc * Val -> ExpVal
(define apply-procedure
  (lambda (proc exps store env)
    (proc exps store env)))

;; exp*store*env -> flag*ref*store
(define get-ref-from-exp
  (lambda (exp store env)
    (cases expression exp
      (var-exp (ident)
               (cons #t
                     (cons (apply-env->ref env ident)
                           store)))
      (call-exp (exp1 exps2)
                (let* ((aw (value-of exp1 (cons env store))))
                  (cases expval (answer->eval aw)
                    (customdtop-val (op)
                                    (if (equal? op "arrayref")
                                        (let* ((res (value-of-exps exps2
                                                                   (answer->store aw)
                                                                   env))
                                               (rands (car res))
                                               (a (car rands))
                                               (idx (expval->num (cadr rands))))
                                          (cons #t
                                                (cons (+ idx (expval->array-ref a))
                                                      (cdr res))))
                                        (cons #f '())))
                    (else (cons #f '())))))
      (else (cons #f '())))))

;; is-ref*exp*store*exp-env -> ref*newstore
(define value-of-operand
  (lambda (is-ref act-exp store exp-env)
    (define make-new-ref
      (lambda (exp)
        (let ((ref (store->nextref store))
              (aw (value-of exp (cons exp-env store))))
          (cons ref
                (extend-store ref
                              (answer->eval aw)
                              (answer->store aw))))))
    (let ((res (if is-ref
                   (get-ref-from-exp act-exp store exp-env)
                   (cons #f '()))))
      (if (car res)
          (cdr res)
          (make-new-ref act-exp)))))

;; exp*store*env -> eval\thunk*store
(define value-of-lazy-operand
  (lambda (exp1 store exp-env)
    (cases expression exp1
      (const-exp (num)
                 (cons (num-val num)
                       store))
      (proc-exp (op vars exp)
                (cons ((proc-operator op) vars exp exp-env)
                      store))
      (else
       (cons
        (an-thunk
         exp1
         exp-env)
        store)))))

(define extend-proc-senv-by-val
  (lambda (form-paras act-exps store exp-env proc-env)
    (if (null? form-paras)
        (cons proc-env store)
        (let* ((aw (value-of (car act-exps) (cons exp-env store)))
               (ident (cases proc-arg (car form-paras)
                        (value-parg (ident) ident)
                        (ref-parg (ident) ident)))
               (new-senv (extend-senv ident
                                      (answer->eval aw)
                                      (cons proc-env
                                            (answer->store aw)))))
          (extend-proc-senv-by-val (cdr form-paras)
                                   (cdr act-exps)
                                   (cdr new-senv)
                                   exp-env
                                   (car new-senv))))))

(define extend-proc-senv
  (lambda (form-paras act-exps store exp-env proc-env)
    (if (null? form-paras)
        (cons proc-env store)
        (let* ((arg-info (cases proc-arg (car form-paras)
                           (value-parg (ident)
                                       (cons ident #f))
                           (ref-parg (ident)
                                     (cons ident #t))))
               (res (value-of-operand (cdr arg-info)
                                      (car act-exps)
                                      store
                                      exp-env))
               (new-proc-env (extend-env-mutable (car arg-info)
                                                 (car res)
                                                 proc-env))
               (new-store (cdr res)))
          (extend-proc-senv (cdr form-paras)
                            (cdr act-exps)
                            new-store
                            exp-env
                            new-proc-env)))))

(define extend-lazy-proc-senv
  (lambda (form-paras act-exps store exp-env proc-env)
    (if (null? form-paras)
        (cons proc-env store)
        (let* ((ident (proc-arg->ident (car form-paras)))
               (res (value-of-lazy-operand (car act-exps)
                                           store
                                           exp-env))
               (new-senv (extend-senv ident
                                      (car res)
                                      (cons proc-env
                                            (cdr res)))))
          (extend-lazy-proc-senv
           (cdr form-paras)
           (cdr act-exps)
           (cdr new-senv)
           exp-env
           (car new-senv))))))

;; copy back result
;; form-paras*act-paras*store*exp-env*proc-env -> new-store
(define copy-res-to-refparas
  (lambda (form-paras act-exps store exp-env proc-env)
    (if (null? form-paras)
        store
        (copy-res-to-refparas
         (cdr form-paras)
         (cdr act-exps)
         (cases proc-arg (car form-paras)
           (value-parg (_)
                       store)
           (ref-parg (ident)
                     (let ((val (apply-senv (cons proc-env store) ident))
                           (ref-info (get-ref-from-exp (car act-exps) store exp-env)))
                       (if (car ref-info)
                           (store->setref
                            store
                            (cadr ref-info)
                            val)
                           store))))
         exp-env
         proc-env))))

;; thunk*store -> answer(eval*store)
(define value-of-thunk
  (lambda (thk store)
    (cases thunk thk
      (an-thunk (exp1 env)
                (value-of exp1 (cons env store))))))

;; Define Store -- implement by schema list
(define-datatype store store?
  (empty-store)
  (extend-store
   (ref number?)
   (val (lambda (v)
          (or (expval? v)
              (thunk? v))))
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
    (debug-info "extend-env-mutable" "var:~s ref:~s\n" var ref)
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
    (extend-senv-imp var
                     eval
                     senv
                     extend-env-mutable)))
(define extend-senv-immutable
  (lambda (var eval senv)
    (extend-senv-imp var
                     eval
                     senv
                     extend-env-immutable)))
(define extend-senv-imp
  (lambda (var eval senv extend-env-fun)
    (let* ((env (car senv))
           (store (cdr senv))
           (ref (store->nextref store)))
      (cons
       (extend-env-fun var ref env)
       (extend-store ref eval store)))))
;; vars**expval*(env . store) -> (newenv . newstore)
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
(define extend-senv*-imp
  (lambda (extend-fun vars evals senv)
    (if (null? vars)
        senv
        (extend-senv*-imp extend-fun
                          (cdr vars) (cdr evals)
                          (extend-fun (car vars)
                                      (car evals)
                                      senv)))))
;; (env . store)*var -> val
(define apply-senv
  (lambda (senv var)
    (let* ((env (car senv))
           (store (cdr senv))
           (env-val (apply-env env var)))
      (cases envval env-val
        (an-env-val (mutable ref)
                    (store->findref store ref))
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
                 (let ((val (apply-senv senv var)))
                   (if (expval? val)
                       (an-answer
                        val
                        store)
                       (let* ((aw (value-of-thunk val store))
                              (eval (answer->eval aw)))
                         (an-answer
                          eval
                          (store->setref
                           (answer->store aw)
                           (apply-env->ref (car senv) var)
                           eval))))))
        (innerop-exp (inner-op)
                     (an-answer
                      (innerop-val inner-op)
                      store))
        (customdtop-exp (cusop)
                        (an-answer
                         (customdtop-val cusop)
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
        (setdynamic-exp (ident exp1 exp2)
                        (setdy-operator ident exp1 exp2 senv))
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
(define let-lazy-extend-env-imp
  (lambda (vars exps1 senv extend-senv-fun)
    (if (null? vars)
        senv
        (let* ((env (car senv))
               (res (value-of-lazy-operand (car exps1)
                                           (cdr senv)
                                           env))
               (new-senv (extend-senv-fun
                          (car vars)
                          (car res)
                          (cons env
                                (cdr res)))))
          (let-lazy-extend-env-imp
           (cdr vars)
           (cdr exps1)
           new-senv
           extend-senv-fun)))))

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
(define let*-lazy-extend-env-immutable
  (lambda (vars exps1 senv)
    (let-lazy-extend-env-imp vars exps1 senv extend-senv-immutable)))
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
   (cons "letlazy*" (let-op let*-lazy-extend-env-immutable))
   ))
(define extend-senv-rec*
  (lambda (proc-names list-of-vars exps senv)
    (extend-senv-rec*-imp proc-names
                          list-of-vars
                          exps
                          senv
                          extend-env-mutable)))
(define extend-senv-rec*-immutable
  (lambda (proc-names list-of-vars exps senv)
    (extend-senv-rec*-imp proc-names
                          list-of-vars
                          exps
                          senv
                          extend-env-immutable)))
(define extend-senv-rec*-imp
  (lambda (proc-names list-of-vars exps senv extend-env-fun)
    (define make-env
      (lambda (vars s-ref env)
        (if (null? vars)
            env
            (make-env (cdr vars) (+ s-ref 1)
                      (extend-env-fun (car vars) s-ref env)))))
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
   (cons "cbrrproc" (make-procedure-val cbrrproc))
   (cons "lazyproc" (make-procedure-val lazyproc))
   ))

(define value-of-exps
  (lambda (exps store env)
    (if (null? exps)
        (cons '() store)
        (let* ((aw (value-of (car exps) (cons env store)))
               (res (value-of-exps (cdr exps)
                                   (answer->store aw) env)))
          (cons
           (cons (answer->eval aw) (car res))
           (cdr res))))))

(define apply-innerop
  (lambda (innerop exps1 store env)
    (let* ((res (value-of-exps exps1 store env))
           (rands (car res))
           (store (cdr res)))
      (an-answer
       (cases inner-operator innerop
         (none-op (op)
                  ((none-operator op)))
         (binary-op (op)
                    ((binary-operator op) (car rands) (cadr rands)))
         (unary-op (op)
                   ((unary-operator op) (car rands))))
       store))))

(define apply-customdtop
  (lambda (op exps store env)
    (let* ((res (value-of-exps exps store env))
           (rands (car res))
           (store (cdr res)))
      ((customdt-operator op) rands store env))))

(define call-operator
  (lambda (exp1 exps2 store env)
    (let* ((aw (value-of exp1 (cons env store)))
           (store (answer->store aw))
           (rator (answer->eval aw)))
      (cases expval rator
        (proc-val (proc)
                  (apply-procedure proc exps2 store env))
        (innerop-val (innerop)
                     (apply-innerop innerop exps2 store env))
        (customdtop-val (op)
                        (apply-customdtop op exps2 store env))
        (else
         (eopl:error 'call-exp "can not apply on expval ~s" rator))))))

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

(define storedata->new
  (lambda (def-val init-vals num store make-fun)
    (define make-store
      (lambda (init-vals num ref store)
        (if (zero? num)
            store
            (let* ((is-null (null? init-vals))
                   (val (if is-null def-val
                            (car init-vals)))
                   (next-vals (if is-null init-vals
                                  (cdr init-vals))))
              (make-store next-vals
                          (- num 1)
                          (+ ref 1)
                          (extend-store ref
                                        val
                                        store))))))
    (let ((ref (store->nextref store)))
      (an-answer
       (make-fun ref)
       (make-store init-vals
                   num
                   ref
                   store)))))

(define storedata->safe-handler
  (lambda (fun sd idx max-num . vars)
    (if (>= idx max-num)
        (eopl:error 'storedata-check "invalid idx for storedata ~s:~s" sd idx)
        (apply fun sd idx vars))))

(define storedata->getbyidx
  (lambda (sd idx store get-ref-fun)
    (let ((ref (get-ref-fun sd)))
      (an-answer
       (store->findref store (+ ref idx))
       store))))

(define storedata->safe-getbyidx
  (lambda (sd idx max-num store get-ref-fun)
    (storedata->safe-handler
     storedata->getbyidx sd idx max-num
     store
     get-ref-fun)))

(define storedata->setbyidx
  (lambda (sd idx val store get-ref-fun)
    (let ((ref (get-ref-fun sd)))
      (an-answer
       sd
       (store->setref
        store
        (+ ref idx)
        val)))))

(define storedata->safe-setbyidx
  (lambda (sd idx max-num val store get-ref-fun)
    (storedata->safe-handler
     storedata->setbyidx sd idx max-num
     val
     store
     get-ref-fun)))

(define array->new
  (lambda (rands store env)
    (debug-info "array->new" "rands:~s store:~s\n" rands store)
    (let ((def-val (cadr rands))
          (num (expval->num (car rands)))
          (init-vals '()))
      (storedata->new def-val init-vals num store
                      (lambda (ref)
                        (array-val ref num))))))

(define array->ref
  (lambda (rands store env)
    (let* ((a (car rands))
           (idx (expval->num (cadr rands)))
           (num (expval->array-num a)))
      (storedata->safe-getbyidx a
                                idx
                                num
                                store
                                expval->array-ref))))

(define array->set
  (lambda (rands store env)
    (let* ((a (car rands))
           (idx (expval->num (cadr rands)))
           (num (expval->array-num a)))
      (storedata->safe-setbyidx a
                                idx
                                num
                                (caddr rands)
                                store
                                expval->array-ref))))

(define array->len
  (lambda (rands store env)
    (an-answer
     (num-val (expval->array-num (car rands)))
     store)))

(define pairs->new
  (lambda (rands store env)
    (storedata->new (num-val 0) rands 2 store mutpair-val)))

(define pairs->left
  (lambda (rands store env)
    (storedata->getbyidx (car rands)
                         0
                         store
                         expval->mutpair)))

(define pairs->right
  (lambda (rands store env)
    (storedata->getbyidx (car rands)
                         1
                         store
                         expval->mutpair)))

(define pairs->setleft
  (lambda (rands store env)
    (storedata->setbyidx (car rands)
                         0
                         (cadr rands)
                         store
                         expval->mutpair)))

(define pairs->setright
  (lambda (rands store env)
    (storedata->setbyidx (car rands)
                         1
                         (cadr rands)
                         store
                         expval->mutpair)))

(define customdt-operator
  (make-fun-table
   (cons "newpair" pairs->new)
   (cons "left" pairs->left)
   (cons "right" pairs->right)
   (cons "setleft" pairs->setleft)
   (cons "setright" pairs->setright)
   (cons "newarray" array->new)
   (cons "arrayref" array->ref)
   (cons "arrayset" array->set)
   (cons "arraylength" array->len)
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
        (an-env-val (mutable ref)
                    (if mutable
                        (an-answer
                         val
                         (store->setref
                          (answer->store aw)
                          ref
                          val))
                        (eopl:error 'set-opt "invalid type for envval ~s:~s" ident env-val)))))))

(define setdy-operator
  (lambda (ident exp1 exp2 senv)
    (let* ((env (car senv))
           (ref (apply-env->ref env ident))
           (old-val (store->findref (cdr senv) ref))
           (aw (value-of exp1 senv))
           (res-aw (value-of exp2
                             (cons env
                                   (store->setref
                                    (answer->store aw)
                                    ref
                                    (answer->eval aw))))))
      (an-answer
       (answer->eval res-aw)
       (store->setref
        (answer->store res-aw)
        ref
        old-val)))))
