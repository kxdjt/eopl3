#lang racket

(provide list-of list-member? debug-fun printf-hlmsg debug-info)

(define DEBUG #t)

(define list-of
  (lambda (pred)
    (lambda (var)
      (or (null? var)
          (and (pair? var)
               (pred (car var))
               ((list-of pred) (cdr var)))))))
(define foldr
  (lambda (fun lst init-res)
    (if (null? lst)
        init-res
        (fun (car lst) (foldr fun (cdr lst) init-res)))))
(define foldl
  (lambda (fun lst init-res)
    (if (null? lst)
        init-res
        (foldl fun (cdr lst) (fun (car lst) init-res)))))
(define list-member?
  (lambda (search-v lst)
    (if (null? lst)
        #f
        (if (equal? search-v (car lst))
            #t
            (list-member? search-v (cdr lst))))))
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
(define printf-hlmsg
  (lambda (key form . vars)
    (apply printf
           (string-append "\x1b[34m" key "\x1b[0m" ": " form)
           vars)))
(define debug-info
  (lambda (key form . vars)
    (if DEBUG
        (apply printf-hlmsg key form vars)
        'none)))
