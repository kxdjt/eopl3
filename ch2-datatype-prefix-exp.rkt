#lang racket
(require (except-in eopl list-of))
(require "ch2-datatype-utils.rkt")

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))
(define parse-expression-imp
  (lambda (plist)
    (cond
      ((not (pair? plist))
       (error 'parse-expression "~s should be pair" plist))
      ((integer? (car plist))
       (list (const-exp (car plist)) (cdr plist)))
      ((equal? (car plist) '-)
       (if (null? (cdr plist))
           (error 'parse-expression "~s expect operand1 for diff-exp but empty" plist)
           (let* ((operand1-res (debug-fun parse-expression-imp (cdr plist)))
                  (operand1 (car operand1-res))
                  (op2-plist (if (null? (cadr operand1-res))
                                 (error 'parse-expression "~s expect operand2 for diff-exp but empty" plist)
                                 (cadr operand1-res)))
                  (operand2-res (debug-fun parse-expression-imp op2-plist))
                  (operand2 (car operand2-res))
                  (last-plist (cadr operand2-res)))
             (list (diff-exp operand1 operand2)
                   last-plist))))
      (else
       (error 'parse-expression "~s is invalid prefix-list!" plist)))))
(define parse-expression
  (lambda (plist)
    (car (debug-fun parse-expression-imp plist))))
(parse-expression '(- - 3 2 - 4 - 12 7))
