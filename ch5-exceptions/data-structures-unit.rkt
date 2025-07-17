#lang racket

(require eopl)
(require "lang-try.rkt")
(require "procedure-sig.rkt")

(provide (all-from-out "lang-try.rkt"))

(provide data-structures^ data-structures@)

(define expval-extractor-error
  (lambda (variant value)
    (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                variant value)))
(define explist-extractor-error
  (lambda (val)
    (eopl:error 'explist-extractors "expected: pair? given: ~s" val)))

(define-signature data-structures^
  (expval?
   num-val
   list-val
   proc-val
   innerop-val
   bool-val
   expval->bool
   expval->num
   expval->list
   expval->proc
   expval->innerop
   expval->isinnerop?
   expval->isproc?
   explist?
   empty-list
   cons-val
   explist->car
   explist->cdr
   explist->null?
   exp->fmt
   ))

(define-unit data-structures@
  (import proc-def^)
  (export data-structures^)

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
  (define expval->innerop
    (lambda (val)
      (cases expval val
        (innerop-val (innerop) innerop)
        (else expval-extractor-error 'innerop val))))
  (define expval->isinnerop?
    (lambda (val)
      (cases expval val
        (innerop-val (_) #t)
        (else #f))))
  (define expval->isproc?
    (lambda (val)
      (cases expval val
        (proc-val (_) #t)
        (else #f))))
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
                                                  (string-append (tostring var)
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
                                                     (string-append (tostring proc-name)
                                                                    "("
                                                                    (make-seplst-str (lambda(var)
                                                                                       (tostring var))
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
        (try-exp (exp1 ident exp2)
                 (make-exp-str "try "
                               (exp->fmt exp1)
                               " catch "
                               "("
                               (tostring ident)
                               ")"
                               (exp->fmt exp2)))
        (raise-exp (exp1)
                   (make-exp-str "raise "
                                 (exp->fmt exp1)))
        )))
  (define innerop->fmt
    (lambda (inner-op)
      (cases inner-operator inner-op
        (none-op (op) op)
        (binary-op (op) op)
        (unary-op (op) op)
        (any-op (op) op))))
  )
