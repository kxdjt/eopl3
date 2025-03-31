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
    (unary-op ((or "not" "minus" "zero?" "car" "cdr" "null?")) string)
    (none-op ((or "emptylist")) string)
    (let-op ((or "let" "let*" "letmutable" "letmutable*")) string)
    (proc-op ((or "proc" "traceproc" "dyproc")) string)
    ))
(define grammar-let
  '((program (statement) a-program)
    ;;-----------Expression Interface------------------
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
    ;;Expression ::= setdynamic identifier = expression during expression
    (expression ("setdynamic" identifier "=" expression "during" expression) setdynamic-exp)
    ;;-----------Statement Interface--------------
    ;;Statement ::= identifier = expression
    (statement (identifier "=" expression) assign-stmt)
    ;;Statement ::= print expression
    (statement ("print" expression) print-stmt)
    ;;Statement ::= {{statement}*(;)}
    (statement ( "{" (separated-list statement ";") "}") order-stmt)
    ;;Statement ::= if expression statement statement
    (statement ( "if" expression statement statement) if-stmt)
    ;;Statement ::= while expression statement
    (statement ( "while" expression statement) while-stmt)
    ;;Statement ::= do statement while experssion
    (statement ( "do" statement "while" expression) do-while-stmt)
    ;;Statement ::= var {identifier}*(,) ; statement
    (statement ( "var" (separated-list identifier ",") ";" statement) block-stmt)
    ;;Statement ::= var {identifier = expression}*(,) ; statement
    (statement ( "var*" (separated-list identifier "=" expression ",") ";" statement) block*-stmt)
    #| ;;Statement ::= var-rec {identifier = expression ,}* {identifier = ({identifier}*(,)) expression} ; statement |#
    #| (statement ( "var-rec" (separated-list identifier "=" expression ",") "|" |#
    #|                        (separated-list identifier "(" (separated-list identifier ",") ")" "=" expression ",") |#
    #|                        ";" |#
    #|                        statement) block-rec-stmt) |#
    ;;Statement ::= vars {identifier = initializtion}*(,) ; statement
    (statement ( "pvar" (separated-list identifier "=" initializtion ",") ";" statement) block-proc-stmt)
    ;;Statement ::= identifier ( {experssion}* )
    (statement ( "[" identifier (arbno expression) "]" ) call-stmt)
    ;;Statement ::= read identitifer
    (statement ( "read" identifier) read-stmt)
    ;;-----------Initializtion Interface--------------
    (initializtion ( "proc" "(" (separated-list identifier ",") ")" expression) proc-init)
    (initializtion ( "subr" "(" (separated-list identifier ",") ")" statement) subr-init)
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
    (result-of-program (scan&parse string))))
(define init-senv
  (lambda ()
    (extend-senv 'i (num-val 1)
                 (extend-senv 'v (num-val 5)
                              (extend-senv 'x (num-val 10)
                                           (empty-senv))))))

;; Program -> none
(define result-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stmt)
                 (result-of stmt (init-senv))
                 "run success!"))))

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

;; Define subroutine data type by schema procedure
(define subroutine
  (lambda (vars body env)
    (lambda (vals store . _)
      (result-of body (extend-senv* vars vals (cons env store))))))


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
  (lambda (var ref env)
    (extend-env var
                (immutable-val ref)
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
#| (define extend-senv*-imp |#
#|   (lambda (extend-fun vars evals senv) |#
#|     (if (null? vars) |#
#|         senv |#
#|         (extend-senv*-imp extend-fun |#
#|                           (cdr vars) (cdr evals) |#
#|                           (extend-fun (car vars) |#
#|                                       (car evals) |#
#|                                       senv))))) |#
(define extend-senv*-imp
  (lambda (extend-fun vars evals senv)
    (if (null? vars)
        senv
        (let* ((is-uninit (null? evals))
               (next-evals (if is-uninit
                               evals
                               (cdr evals)))
               (eval (if is-uninit
                         (num-val 0)
                         (car evals))))
          (extend-senv*-imp extend-fun
                            (cdr vars) next-evals
                            (extend-fun (car vars)
                                        eval
                                        senv))))))
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

;; expression * senv -> answer(eval,store)
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
(define not-op
  (lambda (val1)
    (if (not (expval->bool val1))
        (bool-val #t)
        (bool-val #f))))
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
   (cons "not" not-op)
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
           (ref (cases envval (apply-env env ident)
                  (an-env-val (mutable ref) ref)))
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

;; statement * senv -> store
(define result-of
  (lambda (stmt senv)
    (let* ((env (car senv))
           (store (cdr senv)))
      (debug-info "result-of" "stmt:~s store:~s\n" stmt store)
      (let ((res (result-of-imp stmt senv)))
        (debug-info "result-of" "res:~s\n" res)
        res))))

(define result-of-imp
  (lambda (stmt senv)
    (cases statement stmt
      (assign-stmt (ident exp1)
                   (assign-handler ident exp1 senv))
      (print-stmt (exp1)
                  (print-handler exp1 senv))
      (order-stmt (stmts)
                  (order-handler stmts senv))
      (if-stmt (exp1 stmt1 stmt2)
               (if-handler exp1 stmt1 stmt2 senv))
      (while-stmt (exp1 stmt1)
                  (while-handler exp1 stmt1 senv))
      (do-while-stmt (stmt1 exp1)
                     (do-while-handler stmt1 exp1 senv))
      (block-stmt (ident stmt)
                  (block-handler ident stmt senv))
      (block*-stmt (idents exps stmt)
                   (block-handler* idents exps stmt senv))
      (block-proc-stmt (idents inits stmt)
                       (block-proc-handler idents inits stmt senv))
      (read-stmt (ident)
                 (read-handler ident senv))
      (call-stmt (ident exps)
                 (call-handler ident exps senv))
      )))

(define assign-handler
  (lambda (ident exp1 senv)
    (let* ((env (car senv))
           (ref (cases envval (apply-env env ident)
                  (an-env-val (_ ref) ref)))
           (aw (value-of exp1 senv)))
      (store->setref
       (answer->store aw)
       ref
       (answer->eval aw)))))

(define print-handler
  (lambda (exp1 senv)
    (let ((aw (value-of exp1 senv)))
      (printf "print: ~s\n" (answer->eval aw))
      (answer->store aw))))

(define order-handler
  (lambda (stmts senv)
    (let ((env (car senv))
          (store (cdr senv)))
      (if (null? stmts)
          store
          (order-handler
           (cdr stmts)
           (cons env
                 (result-of (car stmts) senv)))))))

(define if-handler
  (lambda (exp1 stmt1 stmt2 senv)
    (let* ((env (car senv))
           (store (cdr senv))
           (aw (value-of exp1 senv))
           (real-stmt ((if (expval->bool (answer->eval aw))
                           stmt1
                           stmt2))))
      (result-of real-stmt
                 (cons env
                       (answer->store aw))))))

(define while-handler
  (lambda (exp1 stmt1 senv)
    (let* ((env (car senv))
           (aw (value-of exp1 senv))
           (store (answer->store aw)))
      (if (expval->bool (answer->eval aw))
          (while-handler exp1
                         stmt1
                         (cons env
                               (result-of stmt1
                                          (cons env
                                                store))))
          store))))

(define do-while-handler
  (lambda (stmt1 exp1 senv)
    (let* ((env (car senv))
           (store (result-of stmt1 senv))
           (aw (value-of exp1 (cons env store)))
           (new-store (answer->store aw)))
      (if (expval->bool (answer->eval aw))
          (do-while-handler
           stmt1 exp1 (cons env
                            new-store))
          new-store))))

(define block-handler
  (lambda (idents stmt senv)
    (result-of stmt
               (extend-senv*
                idents
                '()
                senv))))

(define block-handler*
  (lambda (idents exps stmt senv)
    (if (null? idents)
        (result-of stmt senv)
        (block-handler*
         (cdr idents)
         (cdr exps)
         stmt
         (let* ((aw (value-of (car exps) senv)))
           (extend-senv
            (car idents)
            (answer->eval aw)
            (cons (car senv)
                  (answer->store aw))))))))

(define read-handler
  (lambda (ident senv)
    (let* ((env (car senv))
           (store (cdr senv))
           (ref (cases envval (apply-env env ident)
                  (an-env-val (_ ref) ref))))
      (store->setref store
                     ref
                     (num-val (read))))))

(define block-proc-handler
  (lambda (idents inits stmt senv)
    (define make-env
      (lambda (vars s-ref env)
        (if (null? vars)
            env
            (make-env (cdr vars) (+ s-ref 1)
                      (extend-env-mutable
                       (car vars)
                       s-ref
                       env)))))
    (define make-store
      (lambda (inits s-ref env store)
        (if (null? inits)
            store
            (make-store (cdr inits) (+ s-ref 1) env
                        (extend-store s-ref
                                      (cases initializtion (car inits)
                                        (proc-init (idents exp1)
                                                   (proc-val
                                                    (procedure idents
                                                               exp1
                                                               env)))
                                        (subr-init (idents stmt1)
                                                   (proc-val
                                                    (subroutine idents
                                                                stmt1
                                                                env))))
                                      store)))))
    (let* ((store (cdr senv))
           (ref (store->nextref store))
           (env (make-env idents ref (car senv))))
      (result-of stmt
                 (cons env
                       (make-store inits ref
                                   env
                                   store))))))


#| (define block-rec-handler |#
#|   (lambda (idents1 exps1 idents2 list-of-vars exps2 stmt senv) |#
#|     (define extend-vars-senv |#
#|       (lambda (vars exps senv) |#
#|         (if (null? vars) |#
#|             senv |#
#|             (extend-vars-senv (cdr vars) (cdr exps) |#
#|                               (let ((aw (value-of (car exps) senv))) |#
#|                                 (extend-senv |#
#|                                  (car vars) |#
#|                                  (answer->eval aw) |#
#|                                  (cons (car senv) |#
#|                                        (answer->store aw)))))))) |#
#|     (let ((new-senv (extend-senv-rec*-immutable |#
#|                      idents2 list-of-vars exps2 |#
#|                      (extend-vars-senv |#
#|                       idents1 exps1 senv)))) |#
#|       (result-of stmt new-senv)))) |#

(define call-handler
  (lambda (ident exps senv)
    (let* ((rator (apply-senv senv ident))
           (ostore (cdr senv))
           (env (car senv))
           (res (foldl (lambda(exp res)
                         (let* ((vals (car res))
                                (last-store (cdr res))
                                (aw (value-of exp (cons env last-store))))
                           (cons
                            (cons (answer->eval aw) vals)
                            (answer->store aw))))
                       (cons '() ostore)
                       exps))
           (rands (car res))
           (store (cdr res)))
      (cases expval rator
        (proc-val (proc)
                  (apply-procedure proc rands store env))
        (else
         (eopl:error 'call-stmt "can not apply on expval ~s" rator))))))

