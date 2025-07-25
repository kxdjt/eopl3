#lang racket

(define-signature scheduler^
  (initialize-scheduler!
   place-on-ready-queue!
   remove-thread-from-ready-queue!
   run-next-thread
   set-final-answer!
   time-expired?
   decrement-timer!
   set-time-remaining!
   get-time-remaining))

(provide scheduler^)

