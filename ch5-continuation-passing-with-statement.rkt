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
    (identifier (letter (arbno (or letter digit "?" "-"))) symbol)
    (binary-op ((or "+" "-" "*" "/" "equal?" "greater?" "less?" "cons")) string)
    (unary-op ((or "not" "minus" "zero?" "car" "cdr" "null?")) string)
    (none-op ((or "emptylist")) string)
    (any-op ((or "list")) string)
    (let-op ((or "let" "let*")) string)
    (proc-op ((or "proc" "traceproc" "dyproc")) string)
    ))
(define grammar-let
  '((program (statement) a-program)
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
    (inner-operator (any-op) any-op)
    ;;-----------Store Interface------------------
    ;;Expression ::= set identifier = expression
    (expression ("set" identifier "=" expression) set-exp)
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
      (a-program (exp1)
                 (result-of/k exp1 (init-senv) (end-com-cont))
                 "run success!"))))

#| Format print |#
(define expval->fmt
  (lambda (val)
    (cases expval val
      (num-val (num)
               num)
      (list-val (elist)
                (explist->fmt elist))
      (else eopl:error 'expval->schemeval "~s can not cast to schemeval" val)
      )))
(define explist->fmt
  (lambda (elist)
    (cases explist elist
      (empty-list () '())
      (cons-val (val elist)
                (cons (expval->fmt val)
                      (explist->fmt elist))))))
(define make-format-fun
  (lambda (left right)
    (lambda strs
    (string-append
     (apply string-append left strs)
     right))))
(define make-stmt-str
  #| (make-format-fun "<|" "|>")) |#
  (make-format-fun "" ""))
(define make-exp-str
  #| (make-format-fun "<<" ">>")) |#
  (make-format-fun "" ""))
(define tostring
  (lambda (val)
    (format "~s" val)))
(define lsts-null?
  (lambda (lsts)
    (or (null? lsts)
        (null? (car lsts)))))
(define cars
  (lambda (lsts)
    (if (null? lsts)
        '()
        (cons (caar lsts)
              (cars (cdr lsts))))))
(define cdrs
  (lambda (lsts)
    (if (null? lsts)
        '()
        (cons (cdar lsts)
              (cdrs (cdr lsts))))))
(define make-seplst-str
  (lambda (transfun sep-str . lsts)
    #| (printf "lsts:~s\n" lsts) |#
    (if (lsts-null? lsts)
        ""
        (string-append (apply transfun (cars lsts))
                       (if (lsts-null? (cdrs lsts))
                           ""
                           sep-str)
                       (apply make-seplst-str transfun
                              sep-str
                              (cdrs lsts))))))
(define stmt->fmt
  (lambda (stmt)
    (cases statement stmt
      (assign-stmt (ident exp1)
                   (make-stmt-str (tostring ident) "=" (exp->fmt exp1)))
      (print-stmt (exp1)
                  (make-stmt-str "print " (exp->fmt exp1)))
      (order-stmt (stmts)
                  (make-stmt-str "{"
                                 (make-seplst-str stmt->fmt
                                                  ";"
                                                  stmts)
                                 "}"))
      (if-stmt (exp1 stmt1 stmt2)
               (make-stmt-str "if " (exp->fmt exp1)
                              " then " (stmt->fmt stmt1)
                              " else " (stmt->fmt stmt2)))
      (while-stmt (exp1 stmt1)
               (make-stmt-str "while "
                              (exp->fmt exp1)
                              (stmt->fmt stmt1)))
      (do-while-stmt (stmt1 exp1)
                (make-stmt-str "do "
                               (stmt->fmt stmt1)
                               " while "
                               (exp->fmt exp1)))
      (block-stmt (idents stmt)
                (make-stmt-str "var "
                               (make-seplst-str (lambda(ident) 
                                                  (tostring ident))
                                                ","
                                                idents)
                               ";"
                               (stmt->fmt stmt)))
      (block*-stmt (idents exps stmt)
                (make-stmt-str "var* "
                               (make-seplst-str (lambda(ident exp)
                                                  (string-append (tostring ident)
                                                                 "="
                                                                 (exp->fmt exp)))
                                                ","
                                                idents
                                                exps)
                               ";\n"
                               (stmt->fmt stmt)))
      (block-proc-stmt (idents inits stmt)
                (make-stmt-str "pvar "
                               (make-seplst-str (lambda(ident init)
                                                  (string-append (tostring ident)
                                                                 "="
                                                                 (init->fmt init)))
                                                ",\n"
                                                idents
                                                inits)
                               ";\n"
                               (stmt->fmt stmt)))
      (call-stmt (ident exps)
                (make-stmt-str "["
                               (tostring ident)
                               " "
                               (make-seplst-str (lambda(exp)
                                                  (exp->fmt exp))
                                                " "
                                                exps)
                               "]"))
      (read-stmt (ident)
                 (make-stmt-str "read "
                                (tostring ident)))
      )))
(define exp->fmt
  (lambda (exp)
    (cases expression exp
      (const-exp (num)
          (make-exp-str (tostring num)))
      (if-exp (exp1 exp2 exp3)
          (make-exp-str "if "
                        (exp->fmt exp1)
                        " then "
                        (exp->fmt exp2)
                        " else "
                        (exp->fmt exp3)))
      (var-exp (ident)
          (make-exp-str (tostring ident)))
      (let-exp (op vars exps body)
          (make-exp-str op
                        (make-seplst-str (lambda(var exp)
                                           (string-append var
                                                          "="
                                                          (exp->fmt exp)))
                                         " "
                                         vars
                                         exps)
                        " in "
                        (exp->fmt body)))
      (letrec-exp (proc-names list-of-vars exps1 exp2)
          (make-exp-str "letrec "
                        (make-seplst-str (lambda(proc-name vars exp)
                                           (string-append proc-name
                                                          "("
                                                          (make-seplst-str (lambda(var)
                                                                             var)
                                                                           " "
                                                                           vars)
                                                          ")"
                                                          (exp->fmt exp)))
                                         " "
                                         proc-names
                                         list-of-vars
                                         exps1)
                        " in "
                        (exp->fmt exp2)))
      (cond-exp (exps1 exps2)
          (make-exp-str "cond"
                        "{"
                        (make-seplst-str (lambda(exp1 exp2)
                                           (string-append (exp->fmt exp1)
                                                          "==>"
                                                          (exp->fmt exp2)))
                                         " "
                                         exps1
                                         exps2)
                        "}"))
      (proc-exp (op vars body)
          (make-exp-str op
                        "("
                        (make-seplst-str (lambda(var)
                                           (tostring var))
                                         ","
                                         vars)
                        ")"
                        (exp->fmt body)))
      (call-exp (exp1 exps2)
          (make-exp-str "("
                        (exp->fmt exp1)
                        (make-seplst-str (lambda(exp)
                                           (exp->fmt exp))
                                         " "
                                         exps2)
                        ")"))
      (begin-exp (exp1 exps2)
          (make-exp-str "begin"
                        (exp->fmt exp1)
                        (make-seplst-str (lambda(exp)
                                           (exp->fmt exp))
                                         ";"
                                         exps2)
                        "end"))
      (set-exp (ident exp1)
          (make-exp-str "set"
                        (tostring ident)
                        "="
                        (exp->fmt exp1)))
      (innerop-exp (inner-op)
          (innerop->fmt inner-op))
      )))
(define innerop->fmt
  (lambda (inner-op)
    (cases inner-operator inner-op
        (none-op (op) op)
        (binary-op (op) op)
        (unary-op (op) op)
        (any-op (op) op))))
(define init->fmt
  (lambda (init)
    (cases initializtion init
      (proc-init (idents exp1)
        (string-append "proc "
                       "("
                       (make-seplst-str (lambda(ident)
                                          (tostring ident))
                                        ","
                                        idents)
                       ")"
                       (exp->fmt exp1)))
      (subr-init (idents stmt)
        (string-append "subr "
                       "("
                       (make-seplst-str (lambda(ident)
                                          (tostring ident))
                                        ","
                                        idents)
                       ")"
                       (stmt->fmt stmt))))))

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
    #| (proc-env (extract-freevar-env vars body env (empty-env))) |#
    (lambda (vals senv cont)
      (let* ((store (cdr senv))
             (new-senv (cons env store)))
        (value-of/k body
                    (extend-senv* vars vals new-senv)
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
    (lambda (vals senv cont)
      (value-of/k body
                  (extend-senv* vars vals senv)
                  cont))))
;; Proc * Vals * Senv * Cont -> FinalAnswer
(define apply-procedure
  (lambda (proc vals senv cont)
    (debug-trace "apply-procedure" "proc:~s vals:~s\n" proc vals)
    (proc vals senv cont)))
(define proc?
  (lambda (proc)
    (procedure? proc)))
;; Define subroutine data type by schema procedure
(define subroutine
  (lambda (vars body env)
    (lambda (vals senv ccont)
      (result-of/k body (extend-senv* vars vals (cons env (cdr senv))) ccont))))
;; Define stmt procedure data type by scheme procedure
(define procedure-stmt
  (lambda (vars body env)
    (lambda (vals senv ccont)
      (let* ((store (cdr senv))
             (new-senv (cons env store))
             (aw (value-of body
                           (extend-senv* vars vals new-senv))))
        (apply-command-cont ccont
                            (answer->store aw))))))

(define extend-senv-rec*
  (lambda (proc-names list-of-vars exps senv)
    (define make-env
      (lambda (vars s-ref env)
        (if (null? vars)
            env
            (make-env (cdr vars) (+ s-ref 1)
                      (extend-env (car vars) s-ref env)))))
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
       (extend-env var ref env)
       (extend-store ref eval store)))))
;; vars**expval*(env . store) -> (newenv . newstore)
(define extend-senv*
  (lambda (vars evals senv)
    (if (null? vars)
        senv
        (let* ((is-uninit (null? evals))
               (next-evals (if is-uninit
                               evals
                               (cdr evals)))
               (eval (if is-uninit
                         (num-val 0)
                         (car evals))))
          (extend-senv* (cdr vars) next-evals
                        (extend-senv (car vars)
                                     eval
                                     senv))))))
;; (env . store)*var -> val
(define apply-senv
  (lambda (senv var)
    #| (debug-trace "apply-senv" "senv:~s var:~s\n" senv var) |#
    (let* ((env (car senv))
           (store (cdr senv))
           (ref (apply-env env var)))
      (store->findref store ref)
      )))

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

;; Exp * Env * Cont -> FinalAnswer
(define value-of/k
  (lambda (exp senv cont)
    (debug-trace "value-of/k" "exp:~s\n env:~s\n store:~s\n" (exp->fmt exp) (car senv) (cdr senv))
      (let ((res (value-of/k-imp exp senv cont)))
        #| (debug-trace "value-of/k" "res:~s\n" res) |#
        res)))
(define value-of/k-imp
  (lambda (exp senv cont)
    (let* ((store (cdr senv))
           (env (car senv))
           (make-answer (lambda(eval)
                          (an-answer eval store))))
      (cases expression exp
        (const-exp (num)
                   (apply-cont cont
                               (make-answer
                                (num-val num))))
        (var-exp (var)
                 #| (debug-trace "test" "is-var:~s cont:~s\n" var cont) |#
                 (apply-cont cont
                             (make-answer
                              (apply-senv senv var))))
        (innerop-exp (inner-op)
                     (apply-cont cont
                                 (make-answer
                                  (innerop-val inner-op))))
        (if-exp (exp1 exp2 exp3)
                (value-of/k exp1 senv
                            (if-cont exp2 exp3 env cont)))
        (let-exp (op vars exps body)
                 (if (null? exps)
                     (value-of/k body senv cont)
                     (value-of/k (car exps) senv
                                 (make-let-cont-by-op
                                  op vars (cdr exps) body cont env))))
        (letrec-exp (proc-names list-of-vars exps1 exp2)
                    (value-of/k exp2
                                (extend-senv-rec* proc-names list-of-vars exps1 senv)
                                cont))
        (cond-exp (exps1 exps2)
                  (if (null? exps1)
                      (eopl:error 'cond "None of the tests succeeds!")
                      (value-of/k (car exps1)
                                  senv
                                  (cond-cont (cdr exps1) exps2 env cont))))
        (proc-exp (op vars body)
                  (apply-cont cont
                              (make-answer
                               (proc-val ((proc-operator op) vars body env)))))
        (call-exp (exp1 rands)
                  (value-of/k exp1
                              senv
                              (call-cont rands env cont)))
        (begin-exp (exp1 exps2)
                   (value-of/k exp1
                               senv
                               (begin-cont exps2 env cont)))
        (set-exp (ident exp1)
                 (value-of/k exp1
                             senv
                             (set-cond (apply-env env ident) cont)))
        ))))

;; procedure representation of continuation
;; Cont * Answer -> FinalAnswer
(define apply-cont
  (lambda (cont aw)
    #| (debug-trace "apply-cont" "cont:~s aw:~s\n" cont aw) |#
    (cont aw)))
;; continuations
(define end-cont
  (lambda ()
    (lambda (aw)
      (printf "End of Computation, ~s\n" (answer->eval aw)))))
(define if-cont
  (lambda (exp2 exp3 env cont)
    (lambda (aw)
      (let* ((eval (answer->eval aw))
             (store (answer->store aw))
             (senv (cons env store)))
        (if (expval->bool eval)
            (value-of/k exp2 senv cont)
            (value-of/k exp3 senv cont))))))
(define let*-cont
  (lambda (vars exps body cont env)
    (lambda (aw)
      (let* ((eval (answer->eval aw))
             (store (answer->store aw))
             (new-senv (extend-senv (car vars) eval
                                    (cons env store))))
        (if (null? exps)
            (value-of/k body new-senv cont)
            (value-of/k (car exps)
                        new-senv
                        (let*-cont (cdr vars) (cdr exps) body cont
                                   (car new-senv))))))))
(define let-cont
  (lambda (vars exps body cont ori-env env)
    (lambda (aw)
      (let* ((eval (answer->eval aw))
             (store (answer->store aw))
             (new-senv (extend-senv (car vars) eval
                                    (cons env store))))
        (if (null? exps)
            (value-of/k body new-senv cont)
            (value-of/k (car exps)
                        (cons ori-env (cdr new-senv))
                        (let-cont (cdr vars) (cdr exps) body cont ori-env
                                  (car new-senv))))))))
(define cond-cont
  (lambda (exps1 exps2 env cont)
    (lambda (aw)
      (let* ((eval (answer->eval aw))
             (store (answer->store aw))
             (senv (cons env store)))
        (if (expval->bool eval)
            (value-of/k (car exps2) senv cont)
            (if (null? exps1)
                (eopl:error 'cond "None of the tests succeeds!")
                (value-of/k (car exps1)
                            senv
                            (cond-cont (cdr exps1) (cdr exps2) env cont))))))))
(define call-cont
  (lambda (rands env cont)
    (lambda (aw)
      (let* ((eval (answer->eval aw))
             (store (answer->store aw))
             (senv (cons env store))
             (make-answer (lambda(eval)
                            (an-answer eval store))))
        (cases expval eval
          (innerop-val (innerop)
                       (cases inner-operator innerop
                         (none-op (op)
                                  (apply-cont cont
                                              (make-answer ((none-operator op)))))
                         (binary-op (op)
                                    (value-of/k (car rands)
                                                senv
                                                (binary-op-cont1 op (cdr rands) env cont)))
                         (unary-op (op)
                                   (value-of/k (car rands)
                                               senv
                                               (unary-op-cont op cont)))
                         (any-op (op)
                                 (if (null? rands)
                                     (apply-cont cont
                                                 (make-answer ((any-operator op) '())))
                                     (value-of/k (car rands)
                                                 senv
                                                 (any-op-cont op (cdr rands) env cont '()))))
                         ))
          (proc-val (proc)
                    (if (null? rands)
                        (apply-procedure proc '() senv cont)
                        (value-of/k (car rands)
                                    senv
                                    (proc-cont proc (cdr rands) env cont '()))))
          (else
           (eopl:error 'call-exp "can not apply on expval ~s" eval))
          )))))
(define binary-op-cont1
  (lambda (op rands env cont)
    (lambda (aw)
      (let* ((eval (answer->eval aw))
             (store (answer->store aw))
             (senv (cons env store)))
        (value-of/k (car rands)
                    senv
                    (binary-op-cont2 op eval cont))))))
(define binary-op-cont2
  (lambda (op eval1 cont)
    (lambda (aw)
      (apply-cont cont
                  (an-answer
                   ((binary-operator op) eval1
                                         (answer->eval aw))
                   (answer->store aw))))))
(define unary-op-cont
  (lambda (op cont)
    (lambda (aw)
      #| (debug-trace "unary-op-cont" "op:~s cont:~s\n" op cont) |#
      (apply-cont cont
                  (an-answer
                   ((unary-operator op) (answer->eval aw))
                   (answer->store aw))))))
(define any-op-cont
  (lambda (op rands env cont vals)
    (lambda (aw)
      (let* ((eval (answer->eval aw))
             (store (answer->store aw))
             (new-vals (append vals (list eval))))
        (if (null? rands)
            (apply-cont cont
                        (an-answer
                         ((any-operator op) vals)
                         store))
            (value-of/k (car rands)
                        (cons env store)
                        (any-op-cont op (cdr rands) env cont new-vals)))))))
(define proc-cont
  (lambda (proc rands env cont vals)
    (lambda (aw)
      (let* ((eval (answer->eval aw))
             (store (answer->store aw))
             (senv (cons env store))
             (new-vals (append vals (list eval))))
        (if (null? rands)
            (apply-procedure proc new-vals senv cont)
            (value-of/k (car rands)
                        senv
                        (proc-cont proc (cdr rands) env cont new-vals)))))))
(define begin-cont
  (lambda (exps2 env cont)
    (lambda (aw)
      (if (null? exps2)
          (apply-cont cont aw)
          (value-of/k (car exps2)
                      (cons env (answer->store aw))
                      (begin-cont (cdr exps2) env cont))))))
(define make-let-cont-by-op
  (lambda (op vars exps body cont env)
    (if (equal? op "let*")
        (let*-cont vars exps body cont env)
        (let-cont vars exps body cont env env))))
(define set-cond
  (lambda (ref cont)
    (lambda (aw)
      (let* ((eval (answer->eval aw))
             (store (answer->store aw)))
        (apply-cont cont
                    (an-answer eval
                               (store->setref
                                store
                                ref
                                eval)))))))

;; Implatement operator function
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
(define not-op
  (lambda (eval)
    (bool-val
      (not (expval->bool eval)))))
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
(define list-op
  (lambda (evals)
    (if (null? evals)
        (list-val (empty-list))
        (list-val
         (cons-val (car evals)
                   (expval->list (list-op (cdr evals))))))))

;; Extract freevar for procedure
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

;; Register operator function
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
   (cons "not" not-op)
   ))
(define none-operator
  (make-fun-table
   (cons "emptylist" emptylist-op)
   ))
(define any-operator
  (make-fun-table
   (cons "list" list-op)
   ))
(define proc-operator
  (make-fun-table
   (cons "proc" procedure)
   #| (cons "traceproc" traceproc) |#
   (cons "dyproc" dynamicproc)
   ))


;; statement * senv * CCont -> Final
(define result-of/k
  (lambda (stmt senv ccont)
    (let* ((env (car senv))
           (store (cdr senv)))
      (debug-trace "result-of" "stmt:~s\n store:~s\n" (stmt->fmt stmt) store)
      (let ((res (result-of/k-imp stmt senv ccont)))
        #| (debug-trace "result-of" "res:~s\n" res) |#
        res))))

(define end-cont-st
  (lambda ()
    (lambda (aw)
      aw)))
(define value-of
  (lambda (exp1 senv)
    (debug-trace "value-of" "exp:~s\n env:~s\n store:~s\n" (exp->fmt exp1) (car senv) (cdr senv))
    (let ((res (value-of/k exp1 senv (end-cont-st))))
        (debug-trace "value-of" "res:~s\n" res)
        res)))

(define result-of/k-imp
  (lambda (stmt senv ccont)
    (cases statement stmt
      (assign-stmt (ident exp1)
                   (apply-command-cont ccont
                                       (assign-handler ident exp1 senv)))
      (print-stmt (exp1)
                  (apply-command-cont ccont
                                      (print-handler exp1 senv)))
      (order-stmt (stmts)
                  (if (null? stmts)
                      (apply-command-cont ccont
                                          (cdr senv))
                      (result-of/k (car stmts) senv
                                   (order-ccont (cdr stmts) (car senv) ccont))))
      (if-stmt (exp1 stmt1 stmt2)
               (if-handler exp1 stmt1 stmt2 senv ccont))
      (while-stmt (exp1 stmt1)
                  (while-handler exp1 stmt1 senv ccont))
      (do-while-stmt (stmt1 exp1)
                     (result-of/k stmt1 senv
                                  (do-while-ccont stmt1 exp1 (car senv) ccont)))
      (block-stmt (idents stmt)
                  (result-of/k stmt
                               (extend-senv* idents
                                             '()
                                             senv)
                               ccont))
      (block*-stmt (idents exps stmt)
                   (block-handler* idents exps stmt senv ccont))
      (block-proc-stmt (idents inits stmt)
                       (block-proc-handler idents inits stmt senv ccont))
      (read-stmt (ident)
                 (read-handler ident senv ccont))
      (call-stmt (ident exps)
                 (call-handler ident exps senv ccont))
      )))

(define assign-handler
  (lambda (ident exp1 senv)
    (let ((aw (value-of exp1 senv))
          (ref (apply-env (car senv) ident)))
      (store->setref
       (answer->store aw)
       ref
       (answer->eval aw)))))
(define print-handler
  (lambda (exp1 senv)
    (let ((aw (value-of exp1 senv)))
      (printf "print: ~s\n" (answer->eval aw))
      (answer->store aw))))
(define if-handler
  (lambda (exp1 stmt1 stmt2 senv ccont)
    (let* ((aw (value-of exp1 senv))
           (new-senv (cons (car senv)
                           (answer->store aw))))
      (if (expval->bool (answer->eval aw))
          (result-of/k stmt1 new-senv ccont)
          (result-of/k stmt2 new-senv ccont)))))
(define while-handler
  (lambda (exp1 stmt1 senv ccont)
    (let* ((aw (value-of exp1 senv))
           (new-senv (cons (car senv)
                           (answer->store aw))))
      (if (expval->bool (answer->eval aw))
          (result-of/k stmt1 new-senv
                       (while-ccont exp1 stmt1 (car senv) ccont))
          (apply-command-cont ccont (answer->store aw))))))
(define block-handler*
  (lambda (idents exps stmt senv ccont)
    (if (null? idents)
        (result-of/k stmt senv ccont)
        (block-handler* (cdr idents)
                        (cdr exps)
                        stmt
                        (let* ((aw (value-of (car exps) senv)))
                          (extend-senv
                           (car idents)
                           (answer->eval aw)
                           (cons (car senv)
                                 (answer->store aw))))
                        ccont))))
(define block-proc-handler
  (lambda (idents inits stmt senv ccont)
    (define make-env
      (lambda (vars s-ref env)
        (if (null? vars)
            env
            (make-env (cdr vars) (+ s-ref 1)
                      (extend-env
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
      (result-of/k stmt
                   (cons env
                         (make-store inits ref
                                     env
                                     store))
                   ccont))))
(define read-handler
  (lambda (ident senv ccont)
    (apply-command-cont ccont
                        (store->setref (cdr senv)
                                       (apply-env (car senv) ident)
                                       (num-val (read))))))
(define call-handler
  (lambda (ident exps senv ccont)
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
                  (apply-procedure proc
                                   rands
                                   (cons env store)
                                   (lambda(res)
                                     (if (answer? res)
                                       (ccont (answer->store res))
                                       (ccont res))))
                  )
        (else
         (eopl:error 'call-stmt "can not apply on expval ~s" rator))))))

(define end-com-cont
  (lambda ()
    (lambda (store)
      (printf "End of Computation\n" ))))
(define order-ccont
  (lambda (stmts env ccont)
    (lambda (store)
      (if (null? stmts)
          (apply-command-cont ccont store)
          (result-of/k (car stmts) (cons env store)
                       (order-ccont (cdr stmts) env ccont))))))
(define while-ccont
  (lambda (exp1 stmt1 env ccont)
    (lambda (store)
      (let* ((aw (value-of exp1 (cons env store)))
             (new-senv (cons env (answer->store aw))))
        (if (expval->bool (answer->eval aw))
            (result-of/k stmt1 new-senv
                         (while-ccont exp1 stmt1 env ccont))
            (apply-command-cont ccont (answer->store aw)))))))
(define do-while-ccont
  (lambda (stmt1 exp1 env ccont)
    (lambda (store)
      (let* ((aw (value-of exp1 (cons env store)))
             (new-senv (cons env (answer->store aw))))
        (if (expval->bool (answer->eval aw))
            (result-of/k stmt1 new-senv
                         (do-while-ccont stmt1 exp1 env ccont))
            (apply-command-cont ccont (answer->store aw)))))))
(define apply-command-cont
  (lambda (ccont store)
    (ccont store)))

