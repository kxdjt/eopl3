#lang racket

(require "../../common/utils.rkt")

(provide thread^ thread@)

(define cur-thread-id 0)
(define next-thread-id 1)
(define message-list '())

(define-signature thread^
  (initialize-thread!
   make-thread
   new-thread
   get-thread-id
   get-cur-thread-id
   set-cur-thread-id!
   apply-thread
   remove-message-list-by-id!
   send-thread-msg
   thread-msg-list-is-empty?
   recive-thread-msg
   ))


(define make-msg-list
  (lambda ()
    (define m-lst '())
    (define is-empty?
      (lambda ()
        (null? m-lst)))
    (define recive-msg
      (lambda()
        (let ((msg (car m-lst)))
          (set! m-lst (cdr m-lst))
          msg)))
    (define send-msg
      (lambda(msg)
        (set! m-lst
              (append m-lst (list msg)))
        #t))
    (list is-empty?
          recive-msg
          send-msg)))


(define-unit thread@
  (import)
  (export thread^)

  (define initialize-thread!
    (lambda ()
      (set! cur-thread-id 0)
      (set! next-thread-id 1)
      (set! message-list
            (list (cons cur-thread-id (make-msg-list))))))


  (define assign-thread-id
    (lambda ()
      (let ((thread-id next-thread-id))
        (set! next-thread-id (+ next-thread-id 1))
        thread-id)))

  (define new-thread
    (lambda(fun)
      (let ((thid (assign-thread-id)))
        (create-message-list-by-id! thid)
        (cons thid
              fun))))
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

  (define create-message-list-by-id!
    (lambda (thid)
      (set! message-list
            (cons (cons thid (make-msg-list))
                  message-list))))
  (define remove-message-list-by-id!
    (lambda (thid)
      (set! message-list
            (remove thid message-list
                    (lambda(id th-msg)
                      (equal? id (car th-msg)))))))
  (define thread-msg-handler
    (lambda (fun)
      (lambda (thid . args)
        (let ((msg-lst (list-member thid
                                    message-list
                                    (lambda (id lst)
                                      (equal? id (car lst))))))
          (apply fun (cdr msg-lst) args)))))
  (define send-thread-msg
    (thread-msg-handler
     (lambda (m-list msg)
       (if m-list
           ((caddr m-list) msg)
           #f))))
  (define thread-msg-list-is-empty?
    (thread-msg-handler
     (lambda (m-list)
       (if m-list
           ((car m-list))
           #t))))
  (define recive-thread-msg
    (thread-msg-handler
     (lambda (m-list)
       (if m-list
           ((cadr m-list))
           #f))))

  )

