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

(define innerop-srt
  (lambda (innerop)
    (cases inner-operator innerop
      (none-op (op) op)
      (binary-op (op) op)
      (unary-op (op) op)
      (any-op (op) op))))
(define INDENT-BASE "    ")
(define indent-str
  (lambda(num)
    (let helper ((time num)
                 (str "\n"))
      (if (zero? time)
          str
          (helper (- time 1)
                  (string-append str INDENT-BASE))))))
(define exp->fmt-indent
  (lambda (exp indent)
    (cases inpexp exp
      (const-exp (num)
                 (make-exp-str (tostring num)))
      (str-exp (str)
               (make-exp-str str))
      (if-exp (exp1 exp2 exp3)
              (make-exp-str "if "
                            (exp->fmt-indent exp1 (+ 1 indent))
                            (indent-str indent) "then "
                            (exp->fmt-indent exp2 (+ 1 indent))
                            (indent-str indent) "else "
                            (exp->fmt-indent exp3 (+ 1 indent))))
      (var-exp (ident)
               (make-exp-str (tostring ident)))
      (let-exp (vars exps body)
               (make-exp-str "let\n"
                             (make-seplst-str
                              (lambda(var exp)
                                (string-append (indent-str (+ 1 indent))
                                               (tostring var)
                                               "="
                                               (exp->fmt-indent exp
                                                                (+ 2 indent))))
                              ""
                              vars
                              exps)
                             (indent-str indent) "in"
                             (indent-str (+ 1 indent)) (exp->fmt-indent body (+ 2 indent))))
      (letrec-exp (proc-names list-of-vars exps1 exp2)
                  (make-exp-str "letrec"
                                (make-seplst-str
                                 (lambda(proc-name vars exp)
                                   (string-append (indent-str (+ 1 indent))
                                                  (tostring proc-name)
                                                  "("
                                                  (make-seplst-str (lambda(var)
                                                                     (tostring var))
                                                                   " "
                                                                   vars)
                                                  ") ="
                                                  (indent-str (+ 2 indent))
                                                  (exp->fmt-indent exp (+ 2 indent))))
                                 ""
                                 proc-names
                                 list-of-vars
                                 exps1)
                                (indent-str indent) "in"
                                (indent-str (+ 1 indent)) (exp->fmt-indent exp2 (+ 2 indent))))
      (proc-exp (vars body)
                (make-exp-str "proc"
                              "("
                              (make-seplst-str (lambda(var)
                                                 (tostring var))
                                               ","
                                               vars)
                              ")"
                              (indent-str (+ 1 indent)) (exp->fmt-indent body (+ 1 indent))))
      (call-exp (exp1 exps2)
                (let* ((need-indent (ormap is-complex-exp?
                                           (cons exp1 exps2)))
                       (ind-str (if need-indent
                                    (indent-str (+ 1 indent))
                                    ""))
                       (next-indent (if need-indent
                                        (+ 2 indent)
                                        (+ 1 indent))))
                  (make-exp-str "("
                                (exp->fmt-indent exp1 (+ 1 indent))
                                " "
                                (make-seplst-str (lambda(exp)
                                                   (string-append ind-str
                                                                  (exp->fmt-indent exp next-indent)))
                                                 " "
                                                 exps2)
                                ")")))
      (innerop-exp (op exps)
                   (let* ((need-indent (ormap is-complex-exp?
                                              exps))
                          (ind-str (if need-indent
                                       (indent-str (+ 1 indent))
                                       ""))
                          (next-indent (if need-indent
                                           (+ 2 indent)
                                           (+ 1 indent))))
                     (make-exp-str (innerop-srt op)
                                   "("
                                   (make-seplst-str (lambda(exp)
                                                      (string-append ind-str
                                                                     (exp->fmt-indent exp next-indent)))
                                                    " "
                                                    exps)
                                   ")")))
      (set-exp (ident exp1)
               (make-exp-str "set "
                             (tostring ident)
                             " = "
                             (exp->fmt-indent exp1 (+ 1 indent))
                             ))

      )))
(define exp->fmt
  (lambda(exp)
    (exp->fmt-indent exp 0)))
(define is-complex-exp?
  (lambda(exp)
    (cases inpexp exp
      (if-exp (v1 v2 v3) #t)
      (let-exp (v1 v2 v3) #t)
      (letrec-exp (v1 v2 v3 v4) #t)
      (proc-exp (v1 v2) #t)
      (call-exp (v1 v2) #t)
      (innerop-exp (op v1)
                   (cases inner-operator op
                     (any-op (iop)
                             (if (equal? iop "list")
                                 #t
                                 #f))
                     (else #f)))
      (else #f))))

