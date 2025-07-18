#lang racket

(define-signature cont-valueof^
  (value-of/k))
(define-signature apply-procedure^
  (apply-procedure/k))

(define-unit apply-procedure@
  (import)
  (export apply-procedure^)
  (define apply-procedure/k
    (lambda (proc vals senv cont econt)
      (proc vals senv cont econt)))
  )
(provide cont-valueof^ apply-procedure^ apply-procedure@)
