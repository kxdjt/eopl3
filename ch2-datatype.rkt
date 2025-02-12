#lang racket
(require eopl)
#| (define identifier? symbol?) |#
(define identifier?
  (lambda (v)
    (if (not (symbol? v)) #f
        (if (equal? 'lambda v) #f
            #t))))

(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-vars (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rands (list-of lc-exp?))))
(define-datatype env-exp env-exp?
  (empty-env)
  (extend-env
   (var symbol?)
   (val schemavalue?)
   (env env-exp?)))
(define schemavalue?
  (lambda (val)
    #t))
(define has-binding?
  (lambda (env s)
    (cases env-exp env
      (empty-env () #f)
      (extend-env (var val env)
                  (if (equal? var s)
                      #t
                      (has-binding? env s))))))
#| (define e (extend-env 'v1 1 (extend-env 'v2 2 (empty-env)))) |#
#| e |#
#| (has-binding? e 'v2) |#
#| (has-binding? e 'v4) |#
(define-datatype stack stack?
  (empty-stack)
  (push
   (val schemavalue?)
   (pre-stack stack?)))
(define pop
  (lambda (s)
    (cases stack s
      (empty-stack ()
                   (error 'pop "~s is empty!" s))
      (push (val pre-stack) pre-stack))))
(define top
  (lambda (s)
    (cases stack s
      (empty-stack ()
                   (error 'top "~s is empty!" s))
      (push (val _) val))))
(define empty-stack?
  (lambda (s)
    (cases stack s
      (empty-stack () #t)
      (else #f))))
#| (define s |#
#|   (push 'a (push 'b (push 'c (empty-stack))))) |#
#| (empty-stack? s) |#
#| (empty-stack? (empty-stack)) |#
#| (top s) |#
#| (top (pop s)) |#
#| (top (pop (pop (pop s)))) |#
(define-datatype bintree bintree?
  (leaf-node
   (num  integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))
(define bintree-to-list
  (lambda (btree)
    (cases bintree btree
      (leaf-node (num)
                 (list 'leaf-node num))
      (interior-node (key left right)
                     (list 'interior-node
                           key
                           (bintree-to-list left)
                           (bintree-to-list right))))))
#| (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4))) |#
(define max-interior-imp
  (lambda (btree)
    (cases bintree btree
      (leaf-node (num)
                 (list num num))
      (interior-node (key left right)
                     (let* ((left-max (max-interior-imp left))
                            (right-max (max-interior-imp right))
                            (cur-num (+ (car left-max) (car right-max))))
                       (cond
                         ((and
                           (null? (cddr left-max))
                           (not (null? (cddr right-max)))
                           (< cur-num (cadr right-max)))
                          (list cur-num (cadr right-max) (caddr right-max)))
                         ((and
                           (null? (cddr right-max))
                           (not (null? (cddr left-max)))
                           (< cur-num (cadr left-max)))
                          (list cur-num (cadr left-max) (caddr left-max)))
                         (else
                          (list cur-num cur-num key))))))))
(define max-interior
  (lambda (btree)
    (caddr (max-interior-imp btree))))
#| (define tree-1 (interior-node 'foo (leaf-node 2) (leaf-node 3))) |#
#| (define tree-2 (interior-node 'bar (leaf-node -1) tree-1)) |#
#| (define tree-3 (interior-node 'baz tree-2 (leaf-node 1))) |#
#| (max-interior tree-2) |#
#| (max-interior tree-3) |#
(define-datatype red-blue-tree red-blue-tree?
  (a-rb-tree
   (rbs red-blue-subtree?)))
(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
   (rbs1 red-blue-subtree?)
   (rbs2 red-blue-subtree?))
  (blue-node
   (rbss (list-of reb-blue-subtree?)))
  (leaf-node
   (ival integer?)))
(define list-of
  (lambda (pred)
    (lambda (var)
      (or (null? var)
          (and (pair? var)
               ((list-of pred) (cdr var)))))))
