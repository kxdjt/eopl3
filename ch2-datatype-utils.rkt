#lang racket

(provide list-of for-each-list debug-fun fold-list)

(define list-of
  (lambda (pred)
    (lambda (var)
      (or (null? var)
          (and (pair? var)
               (pred (car var))
               ((list-of pred) (cdr var)))))))
(define for-each-list
  (lambda (lst fun)
    (if (null? lst)
        '()
        (cons (fun (car lst))
              (for-each-list (cdr lst) fun)))))
(define fold-list
  (lambda (lst fold-fun fold-term-fun fun)
    (let ((fold (lambda(slst) (fold-list slst fold-fun fold-term-fun fun))))
      (if (null? lst)
          (fold-term-fun)
          (fold-fun (fun (car lst)) (fold (cdr lst)))))))
(define forward
  (lambda (x . xs)
    x))
(define debug-fun
  (lambda (fun args)
    (define res
      (lambda (res)
        (forward res (printf "~s: res: ~s\n" fun res))))
    (define g
      (lambda (args-print)
        (res (fun args))))
    (g (printf "~s: args: ~s\n" fun args))))
