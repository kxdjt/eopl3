#lang racket
(define number->sequence
  (lambda (num)
    (list num '() '())))
(define current-element
  (lambda (seq)
    (car seq)))
(define seq->left
  (lambda (seq)
    (cadr seq)))
(define seq->right
  (lambda (seq)
    (caddr seq)))
(define move-to-left
  (lambda (seq)
    (if (null? (seq->left seq))
        (error 'move-to-left
               "is at the left end of ~s" seq)
        (list (car (seq->left seq))
              (cdr (seq->left seq))
              (cons (current-element seq) (seq->right seq))))))
(define move-to-right
  (lambda (seq)
    (if (null? (seq->right seq))
        (error 'move-to-right
               "is at the right end of ~s" seq)
        (list (car (seq->right seq))
              (cons (current-element seq) (seq->left seq))
              (cdr (seq->right seq))))))
(define insert-to-left
  (lambda (num seq)
    (list (current-element seq)
          (cons num (seq->left seq))
          (seq->right seq))))
(define inssert-to-right
  (lambda (num seq)
    (list (current-element seq)
          (seq->left seq)
          (cons num (seq->right seq)))))
(number->sequence 7)
(define seq '(6 (5 4 3 2 1) (7 8 9)))
(current-element seq)
(move-to-left seq)
(move-to-right seq)
(insert-to-left 13 seq)
(inssert-to-right 13 seq)
