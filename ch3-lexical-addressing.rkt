#lang racket

(require (except-in eopl list-of))
#| (require "ch2-ribcage-env.rkt") |#
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
    ;; ---- translator expression
    ;;Expression ::= %lexref number
    ;;               nameless-var-exp (number)
    (expression ( "%lexref" number ) nameless-var-exp)
    ;;Expression ::= %let let-op {expression}* in expression
    ;;               nameless-let-exp (exps exp)
    (expression ( "%let" let-op (arbno expression) "in" expression) nameless-let-exp)
    ;;Expression ::= %letrec expression expression
    ;;                nameless-letrec-exp (exps1 exp2)
    (expression ( "%letrec" (arbno expression) "in" expression) nameless-letrec-exp)
    ;;Expression ::= %lexproc proc-op expression
    ;;                nameless-proc-exp (proc-op exp1)
    (expression ( "%lexproc" proc-op expression ) nameless-proc-exp)
    ;;Expression ::= %lexunpack expression expression
    ;;                nameless-unpack-exp (exp1 exp2)
    (expression ( "%lexunpack" expression expression) nameless-unpack-exp)
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

(define init-senv
  (lambda ()
    (extend-senv
     (extend-senv
      (extend-senv
       (empty-senv)
       'x)
      'v)
     'i)))
(define init-nenv
  (lambda ()
    (extend-nenv
     (extend-nenv
      (extend-nenv
       (empty-nenv)
       (num-val 10))
      (num-val 5))
     (num-val 1))))

;; Program * Senv -> Name-less-program
(define translation-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (a-program (translation-of exp1 (init-senv)))))))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-nenv))))))

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

;; Proc * Val -> ExpVal
(define apply-procedure
  (lambda (proc vals)
    (debug-info "apply-procedure" "proc:~s vals:~s\n" proc vals)
    (let ((res (proc vals)))
      (debug-info "apply-procedure" "res:~s\n" res)
      res)))

(define procedure
  (lambda (exp1 nenv)
    (debug-info "procedure" "exp1:~s nenv:~s\n" exp1 nenv)
    (lambda (vals)
      (value-of exp1
                (extend-nenv* nenv vals)))))
(define traceproc
  (lambda (exp1 nenv)
    (lambda (vals)
      (debug-info "tranceproc" "vals:~s exp1:~s nenv:~s\n" vals exp1 nenv)
      (let ((res (value-of exp1
                           (extend-nenv* nenv vals))))
        (debug-info "tranceproc" "res: ~s\n" res)
        res))))

;; Define static envirorment
(define empty-senv
  (lambda ()
    '()))
(define extend-senv
  (lambda (senv var)
    (debug-info  "extend-senv" "senv:~s var:~s\n" senv var)
    (cons var senv)))
(define extend-senv*
  (lambda (senv vars)
    (if (null? vars)
        senv
        (extend-senv (extend-senv* senv (cdr vars))
                     (car vars)))))
(define apply-senv
  (lambda (senv var)
    (debug-info "apply-senv" "senv:~s var:~s\n" senv var)
    (index-of senv var)))

;; Define name-less environment
(define-datatype nameless-env nameless-env?
  (empty-nenv)
  (extend-rec-nenv
   (nenv nameless-env?)
   (exps (list-of expression?)))
  (extend-nenv
   (nenv nameless-env?)
   (val expval?))
  (extend-nenv*
   (nenv nameless-env?)
   (vals (list-of expval?)))
  )
(define apply-nenv
  (lambda (nenv num)
    (debug-info "apply-nenv" "nenv:~s num:~s\n" nenv num)
    (let ((res (apply-nenv-imp nenv num)))
      (debug-info "apply-nenv" "res:~s\n" res)
      res)))
(define apply-nenv-imp
  (lambda (nenv num)
    (cases nameless-env nenv
      (empty-nenv ()
                  (eopl:error 'apply-nenv "can not apply from empty env"))
      (extend-nenv (nenv val)
                   (if (equal? num 0)
                       val
                       (apply-nenv nenv (- num 1))))
      (extend-nenv* (nenv vars)
                    (let ((len (length vars)))
                      (if (< num len)
                          (list-ref vars num)
                          (apply-nenv nenv (- num len)))))
      (extend-rec-nenv (lnenv exps)
                       (let ((len (length exps)))
                         (if (< num len)
                             (proc-val
                              (procedure (list-ref exps num)
                                         nenv))
                             (apply-nenv lnenv (- num len)))))
      )))

#| (define empty-nenv |#
#|   (lambda () |#
#|     '())) |#
#| (define extend-nenv |#
#|   (lambda (nenv val) |#
#|     (debug-info "extend-nenv" "nenv:~s val:~s\n" nenv val) |#
#|     (cons val nenv))) |#
#| (define extend-nenv* |#
#|   (lambda (nenv vals) |#
#|     (if (null? vals) |#
#|         nenv |#
#|         (extend-nenv* |#
#|          (extend-nenv nenv |#
#|                       (car vals)) |#
#|          (cdr vals))))) |#
#| (define apply-nenv |#
#|   (lambda (nenv num) |#
#|     (list-ref nenv num))) |#



;; Define translation
;; Exps * Senv -> Name-less-exps
(define translation-of-exps
  (lambda (exps senv)
    (map(lambda(exp1)
          (translation-of exp1 senv))
        exps)))

;;Exp * Senv -> Name-less-exp
(define translation-of
  (lambda (exp1 senv)
    (cases expression exp1
      (const-exp (num)
                 (const-exp num))
      (if-exp (exp1 exp2 exp3)
              (if-exp
               (translation-of exp1 senv)
               (translation-of exp2 senv)
               (translation-of exp3 senv)))
      (var-exp (var)
               (nameless-var-exp (apply-senv senv var)))
      (let-exp (op vars exps1 exp2)
               (nameless-let-exp
                op
                (translation-of-exps exps1 senv)
                (translation-of exp2
                                (extend-senv* senv vars))))
      (letrec-exp (proc-names list-of-vars exps1 exp2)
                  (let ((new-senv (extend-senv* senv proc-names)))
                    (nameless-letrec-exp
                     (map (lambda(vars exp1)
                            (translation-of exp1
                                            (extend-senv* new-senv vars)))
                          list-of-vars
                          exps1)
                     (translation-of exp2 new-senv))))
      (cond-exp (exps1 exps2)
                (cond-exp
                 (translation-of-exps exps1 senv)
                 (translation-of-exps exps2 senv)))
      (unpack-exp (vars exp1 exp2)
                  (nameless-unpack-exp
                   (translation-of exp1 senv)
                   (translation-of exp2
                                   (extend-senv* senv vars))))
      (proc-exp (proc-op vars exp1)
                (nameless-proc-exp
                 proc-op
                 (translation-of exp1
                                 (extend-senv* senv vars))))
      (call-exp (exp1 exps2)
                (call-exp
                 (translation-of exp1 senv)
                 (translation-of-exps exps2 senv)))
      (innerop-exp (op)
                   (innerop-exp op))
      (else
       (eopl:error 'translation-of "untranslatable type ~s" exp1)))))

(define value-of
  (lambda (nexp nenv)
    (debug-info "value-of" "nexp:~s nenv:~s\n" nexp nenv)
    (let ((res (value-of-imp nexp nenv)))
      (debug-info "value-of" "res:~s\n" res)
      res)))

;; Exp * Env -> ExpVal
(define value-of-imp
  (lambda (nexp nenv)
    (cases expression nexp
      (nameless-var-exp (num)
                        (apply-nenv nenv num))
      (nameless-let-exp (op exps body)
                        ((let-operator op) exps body nenv))
      (nameless-proc-exp (op exp1)
                         (proc-val ((proc-operator op) exp1 nenv)))
      (nameless-unpack-exp (exp1 exp2)
                           (unpack-operator exp1 exp2 nenv))
      (nameless-letrec-exp (exps1 exp2)
                           (value-of
                            exp2
                            (extend-rec-nenv nenv exps1)))
      (const-exp (num)
                 (num-val num))
      (innerop-exp (inner-op)
                   (innerop-val inner-op))
      (if-exp (exp1 exp2 exp3)
              (if (expval->bool (value-of exp1 nenv))
                  (value-of exp2 nenv)
                  (value-of exp3 nenv)))
      (cond-exp (exps1 exps2)
                (cond-operator exps1 exps2 nenv))
      (call-exp (exp1 rands)
                (let ((rator (value-of exp1 nenv)))
                  (cases expval rator
                    (proc-val (proc)
                              (apply-procedure proc
                                               (map (lambda (rand)
                                                      (value-of rand nenv))
                                                    rands)))
                    (innerop-val (innerop)
                                 (cases inner-operator innerop
                                   (none-op (op)
                                            ((none-operator op) nenv))
                                   (binary-op (op)
                                              ((binary-operator op) (car rands) (cadr rands) nenv))
                                   (unary-op (op)
                                             ((unary-operator op) (car rands) nenv))))
                    (else
                     (eopl:error 'call-exp "can not apply on expval ~s" rator))
                    )))
      (else
       (eopl:error 'value-of "~s unsuported for value-of!" nexp))
      )))
(define extend-env-from-elist
  (lambda (nenv elist)
    (if (explist->null? elist)
        nenv
        (extend-env-from-elist
         (extend-nenv nenv (explist->car elist))
         (explist->cdr elist)))))

(define let-extend-nenv
  (lambda (exps nenv)
    (foldl (lambda(exp1 res-nenv)
             (extend-nenv
              res-nenv
              (value-of exp1 nenv)))
           nenv
           exps)))
(define let*-extend-nenv
  (lambda (exps nenv)
    (foldl (lambda(exp1 res-nenv)
             (extend-nenv
              res-nenv
              (value-of exp1 res-nenv)))
           nenv
           exps)))
(define let-op
  (lambda (extend-nenv-fun)
    (lambda (exps body nenv)
      (let ((new-nenv (extend-nenv-fun exps nenv)))
        (value-of body new-nenv)))))
(define unpack-operator
  (lambda (exp1 exp2 nenv)
    (let ((elst (expval->list (value-of exp1 nenv))))
      (value-of exp2 (extend-env-from-elist nenv elst)))))
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
   (cons "let" (let-op let-extend-nenv))
   (cons "let*" (let-op let*-extend-nenv))
   ))
(define proc-operator
  (make-fun-table
   (cons "proc" procedure)
   (cons "traceproc" traceproc)
   (cons "dyproc" procedure)
   ))
