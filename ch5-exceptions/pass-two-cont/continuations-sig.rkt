#lang racket

(define-signature continuation^
  (apply-cont
   continuation?
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
   except-cont
   try-cont
   empty-econt
   make-let-cont-by-op
   change-nextcont)
  )

(provide continuation^)



