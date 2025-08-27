#lang racket

(provide senv^)

(define-signature senv^
  (empty-senv
   extend-senv
   extend-senv*
   apply-senv
   extend-senv-rec*
   )
  )
