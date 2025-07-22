#lang racket

(define-signature scheduler^
  (initialize-scheduler!
   place-on-ready-queue!
   run-next-thread
   set-final-answer!
   time-expired?
   decrement-timer!))

(provide scheduler^)

