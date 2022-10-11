(define number->sequence
  (lambda (num)
    (list num `() `())))
(define current-element
  (lambda (seq)
    (car seq)))
(define move-to-left
  (lambda (seq)
    (if (null? (cadr seq))
        (error seq "left list is null")
        (list (car (cadr seq)) (cdr (cadr seq)) (cons (car seq) (caddr
                                                                  seq))))))
(define move-to-right
  (lambda (seq)
    (if (null? (caddr seq))
        (error seq "right list is null")
        (list (car (caddr seq)) (cons (car seq) (cadr seq)) (cdaddr
                                                              seq)))))
(define insert-to-left
  (lambda (var seq)
    (list (car seq) (cons var (cadr seq)) (caddr seq))))
(define insert-to-right
  (lambda (var seq)
    (list (car seq) (cadr seq) (cons var (caddr seq)))))
