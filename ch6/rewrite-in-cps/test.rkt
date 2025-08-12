#lang racket

(provide (all-defined-out))

(define remove-first-test
  (list
   (cons (list 2 '(1 4 2 9 3)) '(1 4 9 3))
   (cons (list 2 '(2 1 4 9 3)) '(1 4 9 3))
   (cons (list 2 '(1 4 9 3 2)) '(1 4 9 3))))

(define list-sum-test
  (list
   (cons (list '()) 0)
   (cons (list '(1 2 3)) 6)
   ))

(define occurs-free-test
  (list
   (cons (list 'x 'x) #t)
   (cons (list 'x 'y) #f)
   (cons (list 'x '(lambda (x) (x y))) #f)
   (cons (list 'x '(lambda (y) (x y))) #t)
   (cons (list 'x '((lambda (x) x) (x y))) #t)
   (cons (list 'x '(lambda (y) (lambda (z) (x (y z))))) #t)
   ))

(define subst-test
  (list
   (cons (list 'a 'b '((b c) (b () d))) '((a c) (a () d)))
   (cons (list 'a 'b '()) '())
   (cons (list 'a 'b '(b)) '(a))
   (cons (list 'a 'b '(() b)) '(() a))
   ))
