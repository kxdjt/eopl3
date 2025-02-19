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
    (extend-free-senv
     (extend-free-senv
      (extend-free-senv
       (empty-senv)
       'x)
      'v)
     'i)))
(define init-nenv
  (lambda ()
    (extend-free-nenv
     (extend-free-nenv
      (extend-free-nenv
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
    (debug-info "procedure" "exp1:~s nenv:~s\n" exp1 (nenv->humaninfo nenv))
    (let ((new-nenv (extract-closure-trimmed-nenv nenv exp1 (init-trimmed-nenv nenv))))
      (debug-info "procedure" "new-nenv:~s\n" (nenv->humaninfo new-nenv))
      (lambda (vals)
        (value-of exp1
                  (extend-bind-nenv* new-nenv vals))))))
(define traceproc
  (lambda (exp1 nenv)
    (let ((new-nenv (extract-closure-trimmed-nenv nenv exp1 (init-trimmed-nenv nenv))))
      (lambda (vals)
        (debug-info "tranceproc" "vals:~s exp1:~s nenv:~s\n" vals exp1 (nenv->humaninfo nenv))
        (let ((res (value-of exp1
                             (extend-bind-nenv* new-nenv vals))))
          (debug-info "tranceproc" "res: ~s\n" res)
          res)))))

;; Define static envirorment
(define-datatype static-env static-env?
  (empty-senv)
  (extend-senv
   (bindnum number?)
   (freenum number?)
   (vars static-env-vars?)
   (senv static-env?)))
(define-datatype static-env-vars static-env-vars?
  (senv-bind-var
   (var symbol?))
  (senv-bind-vars
   (vars (list-of symbol?)))
  (senv-free-var
   (var symbol?))
  (senv-free-vars
   (vars (list-of symbol?))))
(define senv->bindnum
  (lambda (senv)
    (cases static-env senv
      (empty-senv () 0)
      (extend-senv (bindnum freenum vars senv)
                   bindnum))))
(define senv->freenum
  (lambda (senv)
    (cases static-env senv
      (empty-senv () 0)
      (extend-senv (bindnum freenum vars senv)
                   freenum))))
(define extend-free-senv
  (lambda (senv var)
    (let ((free-num (senv->freenum senv))
          (bind-num (senv->bindnum senv)))
      (extend-senv bind-num (+ free-num 1)
                   (senv-free-var var)
                   senv))))
(define extend-free-senv*
  (lambda (senv vars)
    (let ((free-num (senv->freenum senv))
          (bind-num (senv->bindnum senv))
          (num (length vars)))
      (extend-senv bind-num (+ free-num num)
                   (senv-free-vars vars)
                   senv))))
(define extend-bind-senv
  (lambda (senv var)
    (let ((free-num (senv->freenum senv))
          (bind-num (senv->bindnum senv)))
      (extend-senv (+ bind-num 1) free-num
                   (senv-bind-var var)
                   senv))))
(define extend-bind-senv*
  (lambda (senv vars)
    (let ((free-num (senv->freenum senv))
          (bind-num (senv->bindnum senv))
          (num (length vars)))
      (extend-senv (+ bind-num num) free-num
                   (senv-bind-vars vars)
                   senv))))

(define apply-senv
  (lambda (osenv svar)
    (define apply-senv-imp
      (lambda (senv)
        (cases static-env senv
          (empty-senv ()
                      (eopl:error 'apply-senv "can not find ~s in env:~s!" svar osenv))
          (extend-senv (bnum fnum vars senv)
                       (cases static-env-vars vars
                         (senv-bind-var (var)
                                        (if (equal? var svar)
                                            (- 0 bnum)
                                            (apply-senv-imp senv)))
                         (senv-bind-vars (vars)
                                         (let ((idx (index-of vars svar)))
                                           (if (not idx)
                                               (apply-senv-imp senv)
                                               (- 0 (- bnum idx)))))
                         (senv-free-var (var)
                                        (if (equal? var svar)
                                            fnum
                                            (apply-senv-imp senv)))
                         (senv-free-vars (vars)
                                         (let ((idx (index-of vars svar)))
                                           (if (not idx)
                                               (apply-senv-imp senv)
                                               (- fnum idx)))))))))
    (apply-senv-imp osenv)))

;; Define name-less environment
(define-datatype nameless-env nameless-env?
  (a-nenv
   (bindvals nameless-val-list?)
   (freevals nameless-val-list?)))
(define-datatype nameless-val-list nameless-val-list?
  (empty-nvals
   (end-idx number?))
  (extend-nvals
   (nvals nameless-val-list?)
   (vals nameless-val?)
   (end-idx number?)))
(define-datatype nameless-val nameless-val?
  (nenv-val
   (val expval?))
  (nenv-vals
   (vals (list-of expval?)))
  (nenv-rec-vals
   (vals (list-of expression?))))
(define nenv->humaninfo
  (lambda (nenv)
    (cases nameless-env nenv
      (a-nenv (bindvals freevals)
              (cons (list "bindvals:" (nvals->humaninfo bindvals))
                    (list "freevals:" (nvals->humaninfo freevals)))))))
(define nvals->humaninfo
  (lambda (nvals)
    (cases nameless-val-list nvals
      (empty-nvals (end-idx)
                   (list end-idx))
      (extend-nvals (nvals vals end-idx)
                    (list end-idx (nval->humaninfo vals)
                          (nvals->humaninfo nvals))))))
(define nval->humaninfo
  (lambda (vals)
    (cases nameless-val vals
      (nenv-val (val) val)
      (nenv-vals (vals) vals)
      (nenv-rec-vals (vals) vals))))
(define nenv->bindvals
  (lambda (nenv)
    (cases nameless-env nenv
      (a-nenv (bindvals freevals) bindvals))))
(define nenv->freevals
  (lambda (nenv)
    (cases nameless-env nenv
      (a-nenv (bindvals freevals) freevals))))
(define nvals->end-idx
  (lambda (nvals)
    (cases nameless-val-list nvals
      (empty-nvals (end-idx) end-idx)
      (extend-nvals (nvals vals end-idx) end-idx))))
(define empty-nenv
  (lambda ()
    (a-nenv (empty-nvals 0) (empty-nvals 0))))
(define extend-bind-nenv
  (lambda (nenv val)
    (let* ((bindvals (nenv->bindvals nenv))
           (end-idx (nvals->end-idx bindvals)))
      (a-nenv
       (extend-nvals bindvals
                     (nenv-val val)
                     (+ end-idx 1))
       (nenv->freevals nenv)))))
(define extend-bind-nenv*
  (lambda (nenv vals)
    (let* ((bindvals (nenv->bindvals nenv))
           (end-idx (nvals->end-idx bindvals)))
      (a-nenv
       (extend-nvals bindvals
                     (nenv-vals vals)
                     (+ end-idx (length vals)))
       (nenv->freevals nenv)))))
(define extend-free-nenv
  (lambda (nenv val)
    (let* ((freevals (nenv->freevals nenv))
           (end-idx (nvals->end-idx freevals)))
      (a-nenv
       (nenv->bindvals nenv)
       (extend-nvals freevals
                     (nenv-val val)
                     (+ end-idx 1))))))
(define extend-free-nenv*
  (lambda (nenv vals)
    (let* ((freevals (nenv->freevals nenv))
           (end-idx (nvals->end-idx freevals)))
      (a-nenv
       (nenv->bindvals nenv)
       (extend-nvals freevals
                     (nenv-vals vals)
                     (+ end-idx (length vals)))))))
(define extend-nenv-with-idx
  (lambda (nenv oidx new-nenv)
    (define extend-nvals-with-idx
      (lambda (nvals val idx)
        (cases nameless-val-list nvals
          (empty-nvals (end-idx)
                       (if (<= idx end-idx)
                           (extend-nvals (empty-nvals (- idx 1))
                                         (nenv-val val)
                                         idx)
                           nvals))
          (extend-nvals (next-nvals vals end-idx)
                        (cond
                          ((equal? idx end-idx)
                           nvals)
                          ((> idx end-idx)
                           (extend-nvals nvals
                                         (nenv-val val)
                                         idx))
                          (else
                           (let* ((h (lambda ()
                                       (extend-nvals
                                        (extend-nvals-with-idx next-nvals val idx)
                                        vals
                                        end-idx)))
                                  (g (lambda (vals)
                                       (if (>= (length vals) (- end-idx idx))
                                           nvals
                                           (h)))))
                             (cases nameless-val vals
                               (nenv-vals (vals)
                                          (g vals))
                               (nenv-rec-vals (vals)
                                              (g vals))
                               (else
                                (h))))))))))
    (let* ((isbind (< oidx 0))
           (allvals (if isbind
                        (nenv->bindvals nenv)
                        (nenv->freevals nenv)))

           (idx (if isbind (- 0 oidx) oidx)))
      (if (< (nvals->end-idx allvals) idx)
          new-nenv
          (let ((vals (if isbind
                          (nenv->bindvals new-nenv)
                          (nenv->freevals new-nenv)))
                (val (apply-nenv nenv oidx)))
            (if isbind
                (a-nenv
                 (extend-nvals-with-idx vals val idx)
                 (nenv->freevals new-nenv))
                (a-nenv
                 (nenv->bindvals new-nenv)
                 (extend-nvals-with-idx vals val idx))))))))

(define extend-rec-nenv
  (lambda (nenv exps)
    (let* ((freevals (nenv->freevals nenv))
           (end-idx (nvals->end-idx freevals)))
      (a-nenv
       (nenv->bindvals nenv)
       (extend-nvals freevals
                     (nenv-rec-vals exps)
                     (+ end-idx (length exps)))))))

;;Nameless-env * Number -> ExpVal
(define apply-nenv
  (lambda (nenv idx)
    (debug-info "apply-nenv" "nenv:~s num:~s\n" (nenv->humaninfo nenv) idx)
    (define apply-nenv-in-vals
      (lambda (nvals idx)
        (debug-info "apply-nenv-in-vals" "nvals:~s idx:~s\n" (nvals->humaninfo nvals) idx)
        (cases nameless-val-list nvals
          (empty-nvals (end-idx)
                       (eopl:error 'apply-nenv "can not find ~s from ~s\n" idx nvals))
          (extend-nvals (nvals vals end-idx)
                        (if (< end-idx idx)
                            (eopl:error 'apply-nenv "can not find ~s from ~s\n" idx nvals)
                            (cases nameless-val vals
                              (nenv-val (val)
                                        (if (equal? end-idx idx)
                                            val
                                            (apply-nenv-in-vals nvals idx)))
                              (nenv-vals (vals)
                                         (let* ((num (length vals))
                                                (dnum (- end-idx idx)))
                                           (if (>= dnum num)
                                               (apply-nenv-in-vals nvals idx)
                                               (list-ref vals dnum))))
                              (nenv-rec-vals (vals)
                                             (let* ((num (length vals))
                                                    (dnum (- end-idx idx)))
                                               (if (> dnum num)
                                                   (apply-nenv-in-vals nvals idx)
                                                   (proc-val
                                                    (procedure (list-ref vals dnum)
                                                               nenv)))))))))))
    (cases nameless-env nenv
      (a-nenv (bindvals freevals)
              (let ((res (if (< idx 0)
                             (apply-nenv-in-vals bindvals (- 0 idx))
                             (apply-nenv-in-vals freevals idx))))
                (debug-info "apply-nenv" "res:~s\n" res)
                res)))))

(define init-trimmed-nenv
  (lambda (nenv)
    (a-nenv (empty-nvals (nvals->end-idx (nenv->bindvals nenv)))
            (empty-nvals (nvals->end-idx (nenv->freevals nenv))))))

(define extract-closure-trimmed-nenv
  (lambda (nenv exp1 new-nenv)
    (cases expression exp1
      (nameless-var-exp (number)
                        new-nenv
                        (extend-nenv-with-idx nenv
                                              number
                                              new-nenv))
      (nameless-let-exp (op exps1 exp2)
                        (extract-closure-trimmed-nenv
                         nenv exp2
                         (foldl (lambda (exp res-nenv)
                                  (extract-closure-trimmed-nenv
                                   nenv exp res-nenv))
                                new-nenv
                                exps1)))
      (nameless-letrec-exp (exps1 exp2)
                           (extract-closure-trimmed-nenv
                            nenv exp2
                            (foldl (lambda (exp res-nenv)
                                     (extract-closure-trimmed-nenv
                                      nenv exp res-nenv))
                                   new-nenv
                                   exps1)))
      (nameless-proc-exp (op exp)
                         (extract-closure-trimmed-nenv nenv exp new-nenv))
      (nameless-unpack-exp (exp1 exp2)
                           (extract-closure-trimmed-nenv
                            nenv exp2
                            (extract-closure-trimmed-nenv
                             nenv exp1 new-nenv)))
      (if-exp (exp1 exp2 exp3)
              (extract-closure-trimmed-nenv
               nenv exp1
               (extract-closure-trimmed-nenv
                nenv exp2
                (extract-closure-trimmed-nenv
                 nenv exp3 new-nenv))))
      (cond-exp (exps1 exps2)
                (foldl (lambda (exp1 exp2 res-nenv)
                         (extract-closure-trimmed-nenv
                          nenv exp2
                          (extract-closure-trimmed-nenv
                           nenv exp1 res-nenv)))
                       new-nenv
                       exps1
                       exps2))
      (call-exp (exp1 exps2)
                (let ((new-nenv
                       (extract-closure-trimmed-nenv nenv exp1 new-nenv)))
                  (foldl (lambda (exp res-nenv)
                           (extract-closure-trimmed-nenv
                            nenv exp res-nenv))
                         new-nenv
                         exps2)))
      (else
       new-nenv))))

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
                                (extend-free-senv* senv vars))))
      (letrec-exp (proc-names list-of-vars exps1 exp2)
                  (let ((new-senv (extend-free-senv* senv proc-names)))
                    (nameless-letrec-exp
                     (map (lambda(vars exp1)
                            (translation-of exp1
                                            (extend-bind-senv* new-senv vars)))
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
                                   (extend-free-senv* senv vars))))
      (proc-exp (proc-op vars exp1)
                (nameless-proc-exp
                 proc-op
                 (translation-of exp1
                                 (extend-bind-senv* senv vars))))
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
    (debug-info "value-of" "nexp:~s nenv:~s\n" nexp (nenv->humaninfo nenv))
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
         (extend-free-nenv nenv (explist->car elist))
         (explist->cdr elist)))))

(define let-extend-nenv
  (lambda (exps nenv)
    (foldl (lambda(exp1 res-nenv)
             (extend-free-nenv
              res-nenv
              (value-of exp1 nenv)))
           nenv
           exps)))
(define let*-extend-nenv
  (lambda (exps nenv)
    (foldl (lambda(exp1 res-nenv)
             (extend-free-nenv
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
