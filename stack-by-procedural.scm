(define empty-stack
  (lambda ()
    (lambda (flag)
      (cond ((equal? flag `is-empty) #t)
            (else
              (error "stack is empty"))))))
(define push
  (lambda (var stack)
    (lambda (flag)
      (cond ((equal? flag `top) var)
            ((equal? flag `pop) stack)
            ((equal? flag `is-empty) #f)))))
(define pop
  (lambda (stack)
      (stack `pop)))
(define top
  (lambda (stack)
      (stack `top)))
(define empty-stack?
  (lambda (stack)
      (stack `is-empty)))


