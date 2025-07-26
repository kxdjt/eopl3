#lang racket

(define-signature scheduler^
  (initialize-scheduler!
   place-on-ready-queue!
   place-on-wait-queue!
   remove-thread-from-ready-queue!
   remove-thread-from-wait-queue!
   run-next-thread
   set-final-answer!
   time-expired?
   decrement-timer!
   set-time-remaining!
   get-time-remaining
   remove-thread))

(provide scheduler^)

