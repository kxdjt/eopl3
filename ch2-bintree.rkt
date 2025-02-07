#lang racket
(define number->bintree
  (lambda (num)
    (list num '() '())))
(define current-element
  (lambda (btree)
    (car btree)))
(define move-to-left
  (lambda (btree)
    (cadr btree)))
(define move-to-right
  (lambda (btree)
    (caddr btree)))
(define at-leaf?
  (lambda (btree)
    (null? btree)))
(define insert-to-left
  (lambda (num btree)
    (list (current-element btree)
          (list num
                (move-to-left btree)
                '())
          (move-to-right btree))))
(define insert-to-right
  (lambda (num btree)
    (list (current-element btree)
          (move-to-left btree)
          (list num
                '()
                (move-to-right btree)))))
(number->bintree 13)
(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))
t1
(move-to-left t1)
(current-element (move-to-left t1))
(at-leaf? (move-to-right (move-to-left t1)))
(insert-to-left 15 t1)
