#lang racket
(define zero
  '(diff (one) (one)))
(define one
  '(one))
(define negative-one
  (list 'diff zero one))
(define cast-num
  (lambda (x)
    (if (equal? (car x) 'one)
        1
        (- (cast-num (cadr x)) (cast-num (caddr x))))))
(define is-zero?
  (lambda (x)
    (equal? (cast-num x) 0)))
(define successor
  (lambda (x)
    (list 'diff x negative-one)))
(define predecessor
  (lambda (x)
    (list 'diff x one)))
(cast-num zero)
(cast-num one)
(cast-num negative-one)
(cast-num (successor zero))
(cast-num (predecessor zero))
(define diff-tree-substract
  (lambda (x y)
    (list 'diff x y)))
(define negative
  (lambda (x)
    (if (equal? (car x) 'one)
        negative-one
        (list 'diff (caddr x) (cadr x)))))
(define diff-tree-plus
  (lambda (x y)
    (diff-tree-substract x (negative y))))
(diff-tree-plus '(one) '(diff (diff (one) (one)) (one)))

