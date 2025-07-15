#lang racket

(provide apply-cont)

;; for continuation-by-fc
(define apply-cont
  (lambda (cont aw)
    (cont aw)))
