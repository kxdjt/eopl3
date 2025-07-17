#lang racket

(define-signature continuation^
  (apply-cont
   end-cont
   if-cont
   let-cont
   let*-cont
   cond-cont
   call-cont
   binary-op-cont1
   binary-op-cont2
   unary-op-cont
   any-op-cont
   proc-cont
   begin-cont
   set-cont
   try-cont
   raise-cont
   try-handler
   apply-handler
   make-let-cont-by-op)
  )

(provide continuation^)



