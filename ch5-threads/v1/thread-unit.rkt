#lang racket

(provide thread^ thread@)

(define cur-thread-id 0)
(define next-thread-id 1)

(define-signature thread^
  (initialize-thread!
   make-thread
   new-thread
   get-thread-id
   get-cur-thread-id
   set-cur-thread-id!
   apply-thread))

(define-unit thread@
  (import)
  (export thread^)

  (define initialize-thread!
    (lambda ()
      (set! cur-thread-id 0)
      (set! next-thread-id 1)))

  (define assign-thread-id
    (lambda ()
      (let ((thread-id next-thread-id))
        (set! next-thread-id (+ next-thread-id 1))
        thread-id)))

  (define new-thread
    (lambda(fun)
      (cons (assign-thread-id)
            fun)))
  (define make-thread
    (lambda (fun)
      (cons cur-thread-id
            fun)))
  (define get-thread-id
    (lambda (th)
      (car th)))
  (define get-cur-thread-id
    (lambda ()
      cur-thread-id))
  (define set-cur-thread-id!
    (lambda (id)
      (set! cur-thread-id id)))
  (define apply-thread
    (lambda (th store)
      ((cdr th) store)))
  )

