#lang racket

(provide list-member? list-member remove-by-idx
         debug-fun printf-hlmsg debug-info debug-trace
         debug-notice list-head make-debug-fun
         debug-thread debug-expfmt)

(define DEBUG #f)
(define TRACE #f)

(define CUR-LEVEL 6)
(define EXPFMT-LEVEL 6)
(define THREAD-LEVEL 5)
(define NOTICE-LEVEL 3)
(define TRACE-LEVEL 2)
(define INFO-LEVEL 1)

(define greater-than-cur-level
  (lambda (level)
    (>= level CUR-LEVEL)))
(define equal-with-cur-level
  (lambda (level)
    (= level CUR-LEVEL)))

(define make-debug
  (lambda (check)
    (lambda (level)
      (lambda (key form . vars)
        (if (check level)
            (apply printf-hlmsg key form vars)
            'none)))))
(define make-greater-debug
  (make-debug greater-than-cur-level))
(define make-equal-debug
  (make-debug equal-with-cur-level))
(define make-tmp-debug
  (make-debug (lambda(level)
                (or (equal? TRACE-LEVEL level)
                    (equal? INFO-LEVEL level)
                    ))))

(define make-debug-fun make-equal-debug)

(define debug-trace
  (make-debug-fun TRACE-LEVEL))
(define debug-info
  (make-debug-fun INFO-LEVEL))
(define debug-notice
  (make-debug-fun NOTICE-LEVEL))
(define debug-thread
  (make-debug-fun THREAD-LEVEL))
(define debug-expfmt
  (make-debug-fun EXPFMT-LEVEL))

(define remove-by-idx
  (lambda (lst idx)
    (if (equal? idx 0)
        (cdr lst)
        (cons (car lst)
              (remove-by-idx (cdr lst) (- idx 1))))))

(define list-head
  (lambda (lst num)
    (if (zero? num)
        '()
        (cons (car lst)
              (list-head (cdr lst) (- num 1))))))
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
  (lambda (search-v lst . pred)
    (define list-member?-tmp
      (lambda (lst pred?)
        (if (null? lst)
            #f
            (if (pred? search-v (car lst))
                #t
                (list-member?-tmp (cdr lst) pred?)))))
    (let ((pred? (if (null? pred) equal?
                     (car pred))))
      (list-member?-tmp lst pred?))))
(define list-member
  (lambda (search-v lst . pred)
    (define list-member-tmp
      (lambda (lst pred?)
        (if (null? lst)
            #f
            (if (pred? search-v (car lst))
                (car lst)
                (list-member-tmp (cdr lst) pred?)))))
    (let ((pred? (if (null? pred) equal?
                     (car pred))))
      (list-member-tmp lst pred?))))
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
