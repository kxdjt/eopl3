(define empty-stack
  (lambda () `()))
(define push
  (lambda (var stack)
    (cons var stack)))
(define pop
  (lambda (stack)
    (if (null? stack) stack
        (cdr stack))))
(define top
  (lambda (stack)
    (if (null? stack) `()
        (car stack))))
(define empty-stack?
  (lambda (stack)
    (null? stack)))
