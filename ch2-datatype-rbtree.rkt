#lang racket
(require (except-in eopl list-of))
(require "ch2-datatype-utils.rkt")

(define-datatype red-blue-tree red-blue-tree?
  (a-rb-tree
   (rbs red-blue-subtree?)))
(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
   (rbs1 red-blue-subtree?)
   (rbs2 red-blue-subtree?))
  (blue-node
   (rbss (list-of red-blue-subtree?)))
  (leaf-node
   (ival integer?)))
(define mark-leaves-with-red-depth-imp
  (lambda (rbt depth)
    (cases red-blue-subtree rbt
      (red-node (rbs1 rbs2)
                (red-node
                 (mark-leaves-with-red-depth-imp rbs1 (+ depth 1))
                 (mark-leaves-with-red-depth-imp rbs2 (+ depth 1))))
      (blue-node (rbss)
                 (blue-node (map  (lambda (rbs)
                                    (mark-leaves-with-red-depth-imp rbs depth))
                                  rbss)))
      (leaf-node (ival)
                 (leaf-node depth)))))
(define mark-leaves-with-red-depth
  (lambda (rbt)
    (cases red-blue-tree rbt
      (a-rb-tree (rbs)
                 (mark-leaves-with-red-depth-imp rbs 0)))))
(define rbs
  (red-node (blue-node (list (leaf-node 1) (leaf-node 4)))
            (red-node (leaf-node 2) (blue-node (list (leaf-node 3) (leaf-node 4))))))
(define rbt (a-rb-tree rbs))
(mark-leaves-with-red-depth rbt)
