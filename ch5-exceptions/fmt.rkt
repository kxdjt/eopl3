#lang racket

(require eopl)
(require "data-structures.rkt")

(provide exp->fmt)

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

