#lang racket

(require "./lang-cps-out.rkt")
(require "../../common/utils.rkt")
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
(define INDENT-BASE "    ")
(define indent-str
  (lambda(num)
    (let helper ((time num)
                 (str "\n"))
      (if (zero? time)
          str
          (helper (- time 1)
                  (string-append str INDENT-BASE))))))
(define innerop-srt
  (lambda(innerop)
    (cases cps-inner-operator innerop
      (cps-none-op (op) op)
      (cps-binary-op (op) op)
      (cps-unary-op (op) op)
      (cps-any-op (op) op))))
(define is-proc-simp?
  (lambda (simp)
    (cases simpleexp simp
      (cps-proc-exp (v1 v2) #t)
      (else #f))))
(define exp->fmt-simple
  (lambda (simp indent)
    #| (debug-trace "exp->fmt-simple" "simp:~s\n" simp) |#
    (cases simpleexp simp
      (cps-const-exp (num)
                     (make-exp-str (tostring num)))
      (cps-var-exp (ident)
                   (make-exp-str (tostring ident)))
      (cps-set-exp (ident simp)
                   (make-exp-str "set "
                                 (tostring ident)
                                 " = "
                                 (exp->fmt-simple simp (+ 1 indent))
                                 ))
      (cps-proc-exp (vars p-body)
                    (make-exp-str "proc"
                                  "("
                                  (make-seplst-str (lambda(var)
                                                     (tostring var))
                                                   ","
                                                   vars)
                                  ")"
                                  (indent-str (+ 1 indent))
                                  (exp->fmt-tfexp p-body (+ 1 indent))))
      (cps-innerop-exp (op simps)
                       (let* ((need-indent (ormap is-proc-simp?
                                                  simps))
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
                                                                         (exp->fmt-simple exp next-indent)))
                                                        " "
                                                        simps)
                                       ")"))))))
(define exp->fmt-tfexp
  (lambda (exp indent)
    #| (debug-trace "exp->fmt-tfexp" "exp:~s\n" exp) |#
    (cases tfexp exp
      (simple-exp->exp (simp)
                       (exp->fmt-simple simp indent))
      (cps-let-exp (vars simps body)
                   (make-exp-str "let"
                                 (make-seplst-str
                                  (lambda(var exp)
                                    (string-append (indent-str (+ 1 indent))
                                                   (tostring var)
                                                   "="
                                                   (exp->fmt-simple exp
                                                                    (+ 2 indent))))
                                  ""
                                  vars
                                  simps)
                                 (indent-str indent) "in"
                                 (indent-str (+ 1 indent)) (exp->fmt-tfexp body (+ 2 indent))))
      (cps-letrec-exp (proc-names list-of-vars exps1 exp2)
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
                                                      (exp->fmt-tfexp exp (+ 2 indent))))
                                     ""
                                     proc-names
                                     list-of-vars
                                     exps1)
                                    (indent-str indent) "in"
                                    (indent-str (+ 1 indent)) (exp->fmt-tfexp exp2 (+ 2 indent))))
      (cps-if-exp (simp1 exp1 exp2)
                  (make-exp-str "if "
                                (exp->fmt-simple simp1 (+ 1 indent))
                                (indent-str indent) "then "
                                (exp->fmt-tfexp exp1 (+ 1 indent))
                                (indent-str indent) "else "
                                (exp->fmt-tfexp exp2 (+ 1 indent))))
      (cps-call-exp (simp1 simps)
                    (let* ((need-indent (ormap is-proc-simp?
                                               (cons simp1 simps)))
                           (ind-str (if need-indent
                                        (indent-str (+ 1 indent))
                                        ""))
                           (next-indent (if need-indent
                                            (+ 2 indent)
                                            (+ 1 indent))))
                      (make-exp-str "("
                                    (exp->fmt-simple simp1 (+ 1 indent))
                                    " "
                                    (make-seplst-str (lambda(exp)
                                                       (string-append ind-str
                                                                      (exp->fmt-simple exp next-indent)))
                                                     " "
                                                     simps)
                                    ")"))))))
(define exp->fmt
  (lambda (exp)
    (if (tfexp? exp)
        (exp->fmt-tfexp exp 0)
        (exp->fmt-simple exp 0))))
