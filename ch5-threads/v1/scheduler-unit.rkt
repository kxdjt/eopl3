#lang racket

(require "scheduler-sig.rkt")
(require "queue.rkt")
(require "../store-unit.rkt")
(require "../../common/utils.rkt")

(define the-ready-queue '())
(define the-final-answer 'uninit)
(define the-max-time-slice 5)
(define the-time-remaining 5)

(provide scheduler@ scheduler^)

(define-unit scheduler@
  (import store^)
  (export scheduler^)

  (define initialize-scheduler!
    (lambda (ticks)
      (set! the-ready-queue (empty-queue))
      (set! the-final-answer 'uninitialized)
      (set! the-max-time-slice ticks)
      (set! the-time-remaining the-max-time-slice)))

  (define place-on-ready-queue!
    (lambda (thread)
      (set! the-ready-queue
            (enqueue the-ready-queue thread))
      (debug-trace "place-orq" "the-ready-queue:~s\n" the-ready-queue)))

  (define run-next-thread
    (lambda (store)
      (if (empty? the-ready-queue)
          (answer->eval the-final-answer)
          (dequeue the-ready-queue
                   (lambda(n-thread rest-p)
                     (set! the-ready-queue rest-p)
                     (set! the-time-remaining the-max-time-slice)
                     (debug-trace "run-nt" "n-thread:~s the-ready-queue:~s\n" n-thread the-ready-queue)
                     (n-thread store))))))
  (define set-final-answer!
    (lambda (aw)
      (set! the-final-answer aw)))
  (define time-expired?
    (lambda ()
      (zero? the-time-remaining)))
  (define decrement-timer!
    (lambda()
      (set! the-time-remaining
            (- the-time-remaining 1))))
  )
