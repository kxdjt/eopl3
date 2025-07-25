#lang racket

(require "scheduler-sig.rkt")
(require "queue.rkt")
(require "../store-unit.rkt")
(require "../../common/utils.rkt")
(require "thread-unit.rkt")

(define the-ready-queue '())
(define the-final-answer 'uninit)
(define the-max-time-slice 5)
(define the-time-remaining 5)

(provide scheduler@ scheduler^)

(define-unit scheduler@
  (import store^ thread^)
  (export scheduler^)

  (define initialize-scheduler!
    (lambda (ticks)
      (set! the-ready-queue (empty-queue))
      (set! the-final-answer 'uninitialized)
      (set! the-max-time-slice ticks)
      (set! the-time-remaining the-max-time-slice)))

  (define place-on-ready-queue!
    (lambda (thread)
      (debug-thread "place-orq" "thread:~s the-ready-queue:~s\n" thread the-ready-queue)
      (set! the-ready-queue
            (enqueue the-ready-queue thread))
      ))

  (define run-next-thread
    (lambda (store)
      (if (empty? the-ready-queue)
          (answer->eval the-final-answer)
          (dequeue the-ready-queue
                   (lambda(n-thread rest-p)
                     (set! the-ready-queue rest-p)
                     (set! the-time-remaining the-max-time-slice)
                     (set-cur-thread-id! (get-thread-id n-thread))
                     (debug-thread "run-nt" "n-thread:~s the-ready-queue:~s\n" n-thread the-ready-queue)
                     (apply-thread n-thread store))))))
  (define remove-thread-from-ready-queue!
    (lambda (th-id)
      (define pred?
        (lambda (id thread)
          (equal? id (get-thread-id thread))))
      (if (list-member? th-id the-ready-queue pred?)

      (let ((new-q (remove th-id
                           the-ready-queue
                           pred?)))
        (debug-thread "remove-readyq" "thid:~s old:~s new:~s\n" th-id the-ready-queue new-q)
        (set! the-ready-queue new-q))
      'none)))
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
  (define get-time-remaining
    (lambda()
      the-time-remaining))
  (define set-time-remaining!
    (lambda(t)
      (set! the-time-remaining t)))
  )
