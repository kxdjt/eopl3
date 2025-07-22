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
   make-let-cont-by-op
   spawn-cont
   end-main-thread-cont
   wait-cont
   signal-cont)
  )

(provide continuation^)



