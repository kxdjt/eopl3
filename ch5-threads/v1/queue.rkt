#lang racket

(provide empty-queue enqueue dequeue)

(define empty-queue
  (lambda ()
    '()))
(define enqueue
  (lambda (q val)
    (append q (list val))))
(define dequeue
  (lambda (q f)
    (f (car q) (cdr q))))
(define empty?
  (lambda (q)
    (null? q)))
