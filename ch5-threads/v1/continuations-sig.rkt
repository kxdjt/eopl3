#lang racket

(define-signature continuation^
  (apply-cont
   end-cont
   if-cont
   let-cont
   let*-cont
   cond-cont
   call-cont
   begin-cont
   set-cont
   make-let-cont-by-op
   spawn-cont
   end-main-thread-cont
   wait-cont
   signal-cont)
  )

(provide continuation^)



