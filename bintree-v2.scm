;(CurBTree UpTree)
;UpTree := ()|(UpValue.UpTree)
;UpValue := (Value Flag NearTree)
;Flag := `left | `right
;CurBTree := btree
;NearTree := btree
(load "./bintree-v1.scm")
(define push-uptree
  (lambda (val flag btree upt)
    (cons (list val flag btree) upt)))
(define pop-uptree
  (lambda (upt)
    (if (null? upt)
        (error upt "is null")
        (cdr upt))))
(define top-uptree
  (lambda (upt)
    (if (null? upt)
        (error upt "is null")
        (car upt))))
(define upvalue->value
  (lambda (uv)
    (if (null? uv)
        (error "upvalue" uv "is null")
        (car uv))))
(define upvalue->flag
  (lambda (uv)
    (if (null? uv)
        (error "upvalue" uv "is null")
        (cadr uv))))
(define upvalue->near-tree
  (lambda (uv)
    (if (null? uv)
        (error "upvalue" uv "is null")
        (caddr uv))))
(define make-bintree-ctx
  (lambda (curbtree uptree)
    (list curbtree uptree)))
(define number->bintree-ctx
  (lambda (num)
    (make-bintree-ctx (number->bintree num) `())))
(define bintree-ctx->current-btree
  (lambda (bctx)
    (if (null? bctx)
        (error bctx "is null")
        (car bctx))))
(define bintree-ctx->up-tree
  (lambda (bctx)
    (if (null? bctx)
        (error bctx "is null")
        (cadr bctx))))
(define move-to-left-ctx
  (lambda (bctx)
    (if (at-leaf? (bintree-ctx->current-btree bctx))
        (error bctx "is at leaf")
        (make-bintree-ctx
          (move-to-left (bintree-ctx->current-btree bctx))
          (push-uptree (current-element (bintree-ctx->current-btree bctx))
                       `left
                       (move-to-right (bintree-ctx->current-btree bctx))
                       (bintree-ctx->up-tree bctx))))))
(define move-to-right-ctx
  (lambda (bctx)
    (if (at-leaf? (bintree-ctx->current-btree bctx))
        (error bctx "is at leaf")
        (make-bintree-ctx
          (move-to-right (bintree-ctx->current-btree bctx))
          (push-uptree (current-element (bintree-ctx->current-btree bctx))
                       `right
                       (move-to-left (bintree-ctx->current-btree bctx))
                       (bintree-ctx->up-tree bctx))))))
(define move-up
  (lambda (bctx)
    (if (null? (bintree-ctx->up-tree bctx))
        (error bctx "up tree is null")
        (cond ((equal? `left (upvalue->flag (top-uptree (bintree-ctx->up-tree
                                                         bctx))))
               (make-bintree-ctx
                 (make-bintree (upvalue->value (top-uptree
                                                 (bintree-ctx->up-tree
                                                   bctx)))
                               (bintree-ctx->current-btree bctx)
                               (upvalue->near-tree (top-uptree
                                                     (bintree-ctx->up-tree
                                                       bctx))))
                 (pop-uptree (bintree-ctx->up-tree bctx))))
              ((equal? `right (upvalue->flag (top-uptree (bintree-ctx->up-tree
                                                           bctx))))
               (make-bintree-ctx
                 (make-bintree (upvalue->value (top-uptree
                                                 (bintree-ctx->up-tree
                                                   bctx)))
                               (upvalue->near-tree (top-uptree
                                                     (bintree-ctx->up-tree
                                                       bctx)))
                               (bintree-ctx->current-btree bctr))
                 (pop-uptree (bintree-ctx->up-tree bctx))))
              (else
                (error bctx "flag is neither left nor right"))))))
(define insert-to-left-ctx
  (lambda (var bctx)
    (make-bintree-ctx
      (insert-to-left var (bintree-ctx->current-btree bctx))
      (bintree-ctx->up-tree bctx))))
(define insert-to-right-ctx
  (lambda (var bctx)
    (make-bintree-ctx
      (insert-to-right var (bintree-ctx->current-btree bctx))
      (bintree-ctx->up-tree bctx))))
(define at-leaf-ctx?
  (lambda (bctx)
    (at-leaf? (bintree-ctx->current-btree bctx))))
(define at-root-ctx?
  (lambda (bctx)
    (null? (bintree-ctx->up-tree bctx))))
