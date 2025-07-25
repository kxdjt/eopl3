#lang racket

(require "../store-unit.rkt")
(require "../data-structures-unit.rkt")
(require "../../common/utils.rkt")
(require "queue.rkt")
(require "thread-unit.rkt")
(require eopl)

(define mutex-list '())

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
  (initialize-mutex!
   new-mutex
   mutex->place-on-wait-queue!
   mutex->isclose?
   mutex->open
   mutex->close
   mutex->waitq-empty?
   mutex->waitq-dequeue
   remove-thread-from-mutexq!
   ))
(define-unit mutex@
  (import store^ data-structures^ thread^)
  (export mutex^)

  (define initialize-mutex!
    (lambda ()
      (set! mutex-list '())))
  (define new-mutex
    (lambda (store)
      (storedata->new 0
                      (list #f '())
                      2
                      store
                      (lambda (ref)
                        (let* ((mutex (mutex-val ref))
                               (new-mutex-list (cons mutex mutex-list)))
                          (set! mutex-list new-mutex-list)
                          mutex)))))
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
        (debug-thread "mutex->place-orq" "thread:~s wait-queue:~s\n" thread waitq)
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
  (define remove-thread-from-mutexq!
    (lambda(th-id store)
      (define pred?
        (lambda (id thread)
          (equal? id (get-thread-id thread))))
      (define remove-from-queue
        (lambda (lst)
          (remove th-id lst pred?)))
      (define remove-th
        (lambda (m-list store)
          (if (null? m-list)
              store
              (let* ((mutex (car m-list))
                     (waitq (mutex->get-waitq mutex store)))
                (if (list-member? th-id waitq pred?)
                    (let* ((new-q (remove-from-queue waitq))
                           (aw (mutex->set-waitq mutex
                                                 store
                                                 new-q)))
                      (debug-thread "remove-waitq" "thid:~s old:~s new:~s\n"
                                    th-id waitq new-q)
                      (remove-th (cdr m-list) (answer->store aw))
                      #| (answer->store aw) |#
                      )
                    (remove-th (cdr m-list) store))))))
      (remove-th mutex-list store)
      ))

  )





