#lang racket
(define zero
  (lambda ()
    '()))
(define is-zero?
  (lambda (n)
    (null? n)))
(define successor-imp
  (lambda (n N)
    (if (is-zero? n)
        '(1)
        (if (< (car n) (- N 1))
            (cons (+ (car n) 1) (cdr n))
            (cons 0 (successor-imp (cdr n) N))))))
(define predecessor-imp
  (lambda (n N)
    (if (is-zero? n)
        (error 'predecessor
               "can't predece from zero!")
        (if (> (car n) 0)
            (if (equal? (car n) 1)
                (if (null? (cdr n))
                    '()
                    (cons 0 (cdr n)))
                (cons (- (car n) 1) (cdr n)))
            (cons (- N 1) (predecessor-imp (cdr n) N))))))
#| (define N 10) |#
(define successor
  (lambda (n)
    (successor-imp n N)))
(define predecessor
  (lambda (n)
    (predecessor-imp n N)))
(define plus
  (lambda (x y)
    (if (is-zero? x)
        y
        (plus (predecessor x) (successor y)))))
(define multiply
  (lambda (x y)
    (if (is-zero? x)
        (zero)
        (plus y (multiply (predecessor x) y)))))
(define factorial
  (lambda (x)
    (if (is-zero? x)
        (zero)
        (if (is-zero? (predecessor x))
            x
            (multiply x (factorial (predecessor x)))))))
#| (define N 2) |#
#| (successor (successor (successor (successor (zero))))) |#
#| (predecessor (predecessor (predecessor (successor (successor (successor (successor (zero)))))))) |#
#| (predecessor (predecessor (predecessor (predecessor (successor (successor (successor (successor (zero))))))))) |#
#| (plus '(0 0 1) '(1 1 1)) |#
(define N 10)
(factorial '(0 1))
