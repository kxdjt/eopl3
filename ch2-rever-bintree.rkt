#lang racket
(define number->bintree
  (lambda (num)
    (list (list num '() '()) '())))
(define bintree->curtree
  (lambda (btree)
    (car btree)))
(define bintree->uptree
  (lambda (btree)
    (cadr btree)))
(define current-element
  (lambda (btree)
    (car (bintree->curtree btree))))
(define bintree->left
  (lambda (btree)
    (cadr (bintree->curtree btree))))
(define bintree->right
  (lambda (btree)
    (caddr (bintree->curtree btree))))
(define uptree->num
  (lambda (uptree)
    (current-element uptree)))
(define uptree->brotree
  (lambda (uptree)
    (bintree->left uptree)))
(define uptree->flag
  (lambda (uptree)
    (bintree->right uptree)))
(define move-to-left
  (lambda (btree)
    (list (bintree->left btree)
          (list
           (list (current-element btree)
                 (bintree->right btree)
                 'Left)
           (bintree->uptree btree)))))
(define move-to-right
  (lambda (btree)
    (list (bintree->right btree)
          (list
           (list (current-element btree)
                 (bintree->left btree)
                 'Right)
           (bintree->uptree btree)))))
(define insert-to-left
  (lambda (num btree)
    (list
     (list (current-element btree)
           (list num
                 (bintree->left btree)
                 '())
           (bintree->right btree))
     (bintree->uptree btree))))
(define insert-to-right
  (lambda (num btree)
    (list
     (list (current-element btree)
           (bintree->left btree)
           (list num
                 '()
                 (bintree->right btree)))
     (bintree->uptree btree))))
(define at-leaf?
  (lambda (btree)
    (null? (bintree->curtree btree))))
(define at-root?
  (lambda (btree)
    (null? (bintree->uptree btree))))
(define move-up
  (lambda (btree)
    (define uptree (bintree->uptree btree))
    (list
     (if (equal? (uptree->flag uptree) 'Left)
         (list (uptree->num uptree)
               (bintree->curtree btree)
               (uptree->brotree uptree))
         (list (uptree->num uptree)
               (uptree->brotree uptree)
               (bintree->curtree btree)))
     (bintree->uptree uptree))))
(number->bintree 13)
(define t1 (insert-to-right 14
                            (insert-to-left 12
                                            (number->bintree 13))))
t1
(move-to-left t1)
(current-element (move-to-left t1))
(at-leaf? (move-to-right (move-to-left t1)))
(insert-to-left 15 t1)
(at-root? t1)
(move-up (move-to-right (move-to-left t1)))
(at-root? (move-up (move-to-right (move-to-left t1))))
