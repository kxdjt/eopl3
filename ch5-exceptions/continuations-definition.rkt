#lang racket


(require (for-syntax racket/base syntax/parse))
(begin-for-syntax
  (define use-dt? #t))  ; 编译时决定

(define-syntax (require-provide-conditional stx)
  (syntax-parse stx
    [(_ mod1 mod2)
     #:with mod (if use-dt?  #'mod1 #'mod2)
     #'(begin
         (require mod)
         (provide (all-from-out mod)))]))
(provide require-provide-conditional)

;; 使用方式（不再需要传 condition，直接由宏读取编译时变量）
(require-provide-conditional "continuations-by-dt.rkt" "continuations-by-fc.rkt")
