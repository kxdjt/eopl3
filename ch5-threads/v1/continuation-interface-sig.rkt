#lang racket

(define-signature cont-valueof^
  (value-of/k))
(define-signature apply-procedure^
  (apply-procedure/k))

(define-unit apply-procedure@
  (import)
  (export apply-procedure^)
  (define apply-procedure/k
    (lambda (proc vals senv cont)
      #| (printf "apply-procedure: proc:~s vals:~s senv:~s cont:~s\n" |#
      #|         proc vals senv cont) |#
      (proc vals senv cont)))
  )
(provide cont-valueof^ apply-procedure^ apply-procedure@)
