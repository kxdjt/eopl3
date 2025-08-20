#lang racket

(require "./lang-cps-in.rkt")
(require eopl)

(provide exp->fmt)

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
    (cases inpexp exp
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
      (let-exp (vars exps body)
               (make-exp-str "let "
                             (make-seplst-str (lambda(var exp)
                                                (string-append (tostring var)
                                                               "="
                                                               (exp->fmt exp)))
                                              " "
                                              vars
                                              exps)))
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
      (proc-exp (vars body)
                (make-exp-str "proc"
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
      (innerop-exp (op exps)
                   (make-exp-str (tostring op)
                                 "("
                                 (make-seplst-str (lambda(exp)
                                                    (exp->fmt exp))
                                                  " "
                                                  exps)
                                 ")"))
      )))

