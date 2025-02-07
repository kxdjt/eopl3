#lang racket
(define nth-element
  (lambda (lst n)
    (if (null? lst)
        (report-list-too-short n)
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1))))))

(define report-list-too-short
  (lambda (n)
    (error 'nth-element
           "List too short by ~s elements.~%" (+ n 1))))
(define nth-element-v2-imp
  (lambda (lst n err)
    (if (null? lst)
        (err)
        (if (zero? n)
            (car lst)
            (nth-element-v2-imp (cdr lst) (- n 1) err)))))
(define nth-element-v2
  (lambda (lst n)
    (define report-list-too-short-v2
      (lambda ()
        (error 'nth-element-v2'
               "~s does not have ~s elements." lst (+ n 1))))
    (nth-element-v2-imp lst n report-list-too-short-v2)))
(define remove-first
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (remove-first s (cdr los))
            (cons (car los) (remove-first s (cdr los)))))))
(define duple
  (lambda (n s)
    (if (zero? n)
        '()
        (cons s (duple (- n 1) s)))))
#| (duple 2 3) |#
#| (duple 4 '(ha ha)) |#
#| (duple 0 '(blah)) |#
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (cadar lst) (caar lst))
              (invert (cdr lst))))))
#| (invert '((a 1) (a 2) (1 b) (2 b))) |#
(define down
  (lambda (lst)
    (if (null? lst)
        '()
        (cons (list (car lst))
              (down (cdr lst))))))
#| (down '(1 2 3)) |#
#| (down '((a) (fine) (idea))) |#
#| (down '(a (more (complicated)) object)) |#
(define swapper
  (lambda (s1 s2 slist)
    (if (equal? s1 s2)
        slist
        (if (null? slist)
            '()
            (if (list? slist)
                (cons (swapper s1 s2 (car slist)) (swapper s1 s2 (cdr slist)))
                (if (equal? s1 slist)
                    s2
                    (if (equal? s2 slist)
                        s1
                        slist)))))))
#| (swapper 'a 'd '(a b c d)) |#
#| (swapper 'a 'd '(a d () c d)) |#
#| (swapper 'x 'y '((x) y (z (x)))) |#
(define list-set
  (lambda (lst n x)
    (if (null? lst)
        '()
        (if (equal? n 0)
            (cons x (cdr lst))
            (cons (car lst) (list-set (cdr lst) (- n 1) x))))))
#| (list-set '(a b c d) 2 '(1 2)) |#
#| (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3) |#
(define count-occurrences
  (lambda (s slist)
    (if (null? slist)
        0
        (if (list? slist)
            (+ (count-occurrences s (car slist)) (count-occurrences s (cdr slist)))
            (if (equal? s slist)
                1
                0)))))
#| (count-occurrences 'x '((f x) y (((x z) x)))) |#
#| (count-occurrences 'x '((f x) y (((x z) () x)))) |#
#| (count-occurrences 'w '((f x) y (((x z) x)))) |#
(define product-imp
  (lambda (s sos slift)
    (if (null? sos)
        slift
        (cons (list s (car sos)) (product-imp s (cdr sos) slift)))))
(define product
  (lambda (sos1 sos2)
    (if (null? sos1)
        '()
        (product-imp (car sos1) sos2
                     (product (cdr sos1) sos2)))))
#| (product '(a b c) '(x y)) |#
(define filter-in
  (lambda (pred lst)
    (if (null? lst)
        '()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst))))))
#| (filter-in number? '(a 2 (1 3) b 7)) |#
#| (filter-in symbol? '(a (b c) 17 foo)) |#
(define list-index-imp
  (lambda (pred lst index)
    (if (null? lst)
        '#f
        (if (pred (car lst))
            index
            (list-index-imp pred (cdr lst) (+ index 1))))))
(define list-index
  (lambda (pred lst)
    (list-index-imp pred lst 0)))
#| (list-index number? '(a 2 (1 3) b 7)) |#
#| (list-index symbol? '(a (b c) 17 foo)) |#
#| (list-index symbol? '(1 2 (a b) 3)) |#
(define every?
  (lambda (pred lst)
    (if (null? lst)
        '#t
        (if (pred (car lst))
            (every? pred (cdr lst))
            '#f))))
#| (every? number? '(a b c 3 e)) |#
#| (every? number? '(1 2 3 4 5 4)) |#
(define exists?
  (lambda (pred lst)
    (if (null? lst)
        '#f
        (if (pred (car lst))
            '#t
            (exists? pred (cdr lst))))))
#| (exists? number? '(a b c 3 e)) |#
#| (exists? number? '(a b c d e)) |#
(define up-imp
  (lambda (l-lst r-lst)
    (if (pair? l-lst)
        (cons (car l-lst)
              (up-imp (cdr l-lst) r-lst))
        (if (null? l-lst)
            (up r-lst)
            (cons l-lst (up r-lst))))))
(define up
  (lambda (lst)
    (if (null? lst)
        '()
        (up-imp (car lst) (cdr lst)))))
#| (up '((1 2) (3 4))) |#
#| (up '((x (y)) z)) |#
(define flatten-imp
  (lambda (slist slift)
    (if (null? slist)
        slift
        (if (symbol? slist)
            (cons slist slift)
            (flatten-imp (car slist)
                         (flatten-imp (cdr slist) slift))))))
(define flatten
  (lambda (slist)
    (if (null? slist)
        '()
        (flatten-imp (car slist)
                     (flatten-imp (cdr slist) '())))))
#| (flatten '(a b c)) |#
#| (flatten '((a) () (b ()) () (c))) |#
#| (flatten '((a b) c(((d)) e))) |#
#| (flatten '(a b (() (c)))) |#
(define merge
  (lambda (loi1 loi2)
    (if (null? loi1)
        loi2
        (if (null? loi2)
            loi1
            (if (<= (car loi1) (car loi2))
                (cons (car loi1) (merge (cdr loi1) loi2))
                (cons (car loi2) (merge loi1 (cdr loi2))))))))
#| (merge '(1 4) '(1 2 8)) |#
#| (merge '(35 62 81 90 91) '(3 83 85 90)) |#
(define sort-insert/pred
  (lambda (pred s loi)
    (if (null? loi)
        (cons s '())
        (if (pred s (car loi))
            (cons s loi)
            (cons (car loi) (sort-insert/pred pred s (cdr loi)))))))
(define sort
  (lambda (loi)
    (if (null? loi)
        '()
        (sort-insert/pred <= (car loi) (sort (cdr loi))))))
#| (sort '(8 2 5 2 3)) |#
(define sort/predicate
  (lambda (pred loi)
    (if (null? loi)
        '()
        (sort-insert/pred pred (car loi) (sort/predicate pred (cdr loi))))))
#| (sort/predicate < '(8 2 5 2 3)) |#
#| (sort/predicate > '(8 2 5 2 3)) |#
(define leaf
  (lambda (i)
    i))
(define interior-node
  (lambda (s lt rt)
    (list s lt rt)))
(define leaf?
  (lambda (t)
    (if (integer? t)
        #t
        #f)))
(define lson
  (lambda (t)
    (car (cdr t))))
(define rson
  (lambda (t)
    (car (cdr (cdr t)))))
(define contents-of
  (lambda (t)
    (if (leaf? t)
        t
        (car t))))
(define double-tree
  (lambda (t)
    (if (leaf? t)
        (leaf (* 2 (contents-of t)))
        (interior-node (contents-of t)
                       (double-tree (lson t))
                       (double-tree (rson t))))))
(define test-btree
  (interior-node 'sym1
                 (interior-node 'sym2
                                (leaf 1)
                                (leaf 2))
                 (interior-node 'sym3
                                (leaf 3)
                                (leaf 4))))
#| test-btree |#
#| (lson test-btree) |#
#| (rson test-btree) |#
#| (contents-of test-btree) |#
#| (contents-of (leaf 5)) |#
#| (double-tree test-btree) |#
#| (double-tree (leaf 5)) |#
(define mark-leaves-with-red-depth-imp
  (lambda (t n)
    (if (leaf? t)
        (leaf n)
        (if (equal? (contents-of t) 'red)
            (interior-node (contents-of t)
                           (mark-leaves-with-red-depth-imp (lson t)
                                                           (+ n 1))
                           (mark-leaves-with-red-depth-imp (rson t)
                                                           (+ n 1)))
            (interior-node (contents-of t)
                           (mark-leaves-with-red-depth-imp (lson t) n)
                           (mark-leaves-with-red-depth-imp (rson t) n))))))
(define mark-leaves-with-red-depth
  (lambda (t)
    (mark-leaves-with-red-depth-imp t 0)))
#| (mark-leaves-with-red-depth |#
#|  (interior-node 'red |#
#|                 (interior-node 'bar |#
#|                                (leaf 26) |#
#|                                (leaf 12)) |#
#|                 (interior-node 'red |#
#|                                (leaf 11) |#
#|                                (interior-node 'quux |#
#|                                               (leaf 117) |#
#|                                               (leaf 14))))) |#
(define path
  (lambda (num bst)
    (if (null? bst)
        #f
        (if (equal? (car bst) num)
            '()
            (if (false? (path num (cadr bst)))
                (if (false? (path num (caddr bst)))
                    #f
                    (cons 'ritht (path num (caddr bst))))
                (cons 'left (path num (cadr bst))))))))
#| (path 17 '(14 (7 () (12 () ())) |#
#|               (26 (20 (17 () ()) |#
#|                       ()) |#
#|                   (31 () ())))) |#
#| (path 17 '(14 (7 () (12 () ())) |#
#|               (26 (20 (31 () ()) |#
#|                       ()) |#
#|                   (17 () ())))) |#
(define leaf-num
  (lambda (bt)
    (if (leaf? bt)
        1
        (+ (leaf-num (lson bt)) (leaf-num (rson bt))))))
(define number-leaves-imp
  (lambda (bt num)
    (if (leaf? bt)
        (leaf (+ num 1))
        (interior-node (contents-of bt)
                       (number-leaves-imp (lson bt) num)
                       (number-leaves-imp (rson bt) (+ num (leaf-num (lson bt))))))))
(define number-leaves
  (lambda (bt)
    (number-leaves-imp bt -1)))
#| (number-leaves (interior-node 'foo |#
#|                               (interior-node 'bar |#
#|                                              (leaf 26) |#
#|                                              (leaf 12)) |#
#|                               (interior-node 'baz |#
#|                                              (leaf 11) |#
#|                                              (interior-node 'quux |#
#|                                                             (leaf 117) |#
#|                                                             (leaf 14))))) |#
(define g
  (lambda (ls1 ls2)
    (if (null? ls2)
        (list ls1)
        (cons ls1
              (g (cons (+ (caar ls2) 1)
                       (cdar ls2))
                 (cdr ls2))))))
(define number-elements
  (lambda (lst)
    (if (null? lst)
        '()
        (g (list 0 (car lst)) (number-elements (cdr lst))))))
#| (number-elements '(v0 v1 v2 v3 v4 v5)) |#
#| (number-elements '(v0)) |#
#| (number-elements '()) |#
