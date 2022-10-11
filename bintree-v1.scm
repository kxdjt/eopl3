(define make-bintree
  (lambda (cur left right)
    (list cur left right)))
(define number->bintree
  (lambda (num)
    (make-bintree num `() `())))
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
  (lambda (var btree)
    (if (at-leaf? btree)
        (error btree "is at leaf")
        (list (car btree) (list var (cadr btree) `()) (caddr btree)))))
(define insert-to-right
  (lambda (var btree)
    (if (at-leaf? btree)
        (error btree "is at leaf")
        (list (car btree) (cadr btree) (list var (caddr btree) `())))))
