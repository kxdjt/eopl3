#lang racket

(require "../store-unit.rkt")
(require "../data-structures-unit.rkt")
(require "queue.rkt")
(require eopl)

(provide mutex^ mutex@)
#| (define make-mutex |#
#|   (lambda () |#
#|     (define close #f) |#
#|     (define w-q '()) |#
#|     (define set-close |#
#|       (lambda (x) |#
#|         (set! close x))) |#
#|     (define get-close |#
#|       (lambda() |#
#|         close)) |#
#|     (list get-close |#
#|           set-close))) |#
(define-signature mutex^
  (new-mutex
   mutex->place-on-wait-queue!
   mutex->isclose?
   mutex->open
   mutex->close
   mutex->waitq-empty?
   mutex->waitq-dequeue
   ))
(define-unit mutex@
  (import store^ data-structures^)
  (export mutex^)

  (define new-mutex
    (lambda (store)
      (storedata->new 0
                      (list #f '())
                      2
                      store
                      (lambda (ref)
                        (mutex-val ref)))))
  (define mutex->isclose?
    (lambda (mutex store)
      (storedata->getbyidx mutex
                           0
                           store
                           expval->mutex)))
  (define mutex->get-waitq
    (lambda (mutex store)
      (storedata->getbyidx mutex
                           1
                           store
                           expval->mutex)))
  (define mutex->set-close
    (lambda (mutex store c)
      (storedata->setbyidx mutex
                           0
                           c
                           store
                           expval->mutex)))
  (define mutex->set-waitq
    (lambda (mutex store wq)
      (storedata->setbyidx mutex
                           1
                           wq
                           store
                           expval->mutex)))
  (define mutex->close
    (lambda (mutex store)
      (mutex->set-close mutex store #t)))
  (define mutex->open
    (lambda (mutex store)
      (mutex->set-close mutex store #f)))
  (define mutex->place-on-wait-queue!
    (lambda (mutex store thread)
      (let* ((waitq (mutex->get-waitq mutex store))
             (new-waitq (enqueue waitq thread)))
        (mutex->set-waitq mutex store new-waitq))))
  (define mutex->waitq-empty?
    (lambda (mutex store)
      (let ((waitq (mutex->get-waitq mutex store)))
        (null? waitq))))
  (define mutex->waitq-dequeue
    (lambda (mutex store fun)
      (let ((waitq (mutex->get-waitq mutex store)))
        (fun (car waitq))
        (mutex->set-waitq mutex store (cdr waitq)))))
  )





