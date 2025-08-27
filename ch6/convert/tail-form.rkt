#lang racket

(require eopl)

(require "./lang-cps-in.rkt")
(require "../../common/utils.rkt")

(provide run simple-exp?)

(define simple-exp?
  (lambda (exp)
    (cases inpexp exp
      (const-exp (_) #t)
      (var-exp (_) #t)
      (proc-exp (b-vars p-body)
                (tail-form? p-body))
      (innerop-exp (inner-op exps)
                   (andmap simple-exp? exps))
      (set-exp (ident exp)
               (simple-exp? exp))
      (else
       (begin
         (debug-trace "simple-exp?" "exp:~s\n" exp)
         #f)))))

(define tail-form?
  (lambda(exp)
    (cases inpexp exp
      (const-exp (num) #t)
      (var-exp (ident) #t)
      (if-exp (exp1 exp2 exp3)
              (and (simple-exp? exp1)
                   (tail-form? exp2)
                   (tail-form? exp3)))
      (let-exp (idents exps1 exp2)
               (and
                (andmap simple-exp? exps1)
                (tail-form? exp2)))
      (letrec-exp (p-names b-varss p-bodyies letrec-body)
                  (and
                   (andmap tail-form? p-bodyies)
                   (tail-form? letrec-body)))
      (call-exp (exp1 exps2)
                (and
                 (simple-exp? exp1)
                 (andmap simple-exp? exps2)))
      (innerop-exp (inner-op exps)
                   (simple-exp? exp))
      (proc-exp (b-vars p-body)
                (tail-form? p-body))
      (set-exp (ident exp)
               (simple-exp? exp)))))

(define run
  (lambda (string)
    (cases program (scan&parse-cps-in string)
      (a-program (exp)
                 (tail-form? exp)))))
