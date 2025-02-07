#lang racket
(define empty-stack
  (lambda ()
    (list
     (lambda () #t)
     (lambda () error 'stack "is empty stack!")
     (lambda () error 'stack "is empty stack!"))))
(define push
  (lambda (val stack)
    (list
     (lambda () #f)
     (lambda () val)
     (lambda () stack))))
(define empty-stack?
  (lambda (stack)
    ((car stack))))
(define top
  (lambda (stack)
    ((cadr stack))))
(define pop
  (lambda (stack)
    ((caddr stack))))
(define s
  (push 'a (push 'b (push 'c (empty-stack)))))
(empty-stack? s)
(empty-stack? (empty-stack))
(top s)
(top (pop s))
(top (pop (pop (pop s))))
