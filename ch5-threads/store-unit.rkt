#lang racket

(require eopl)
(require "data-structures-unit.rkt")

(provide store^ store@)


(define-signature store^
  (store?
   empty-store
   extend-store
   store->nextref
   store->findref
   store->setref
   answer?
   an-answer
   answer->eval
   answer->store
   storedata->new
   storedata->safe-handler
   storedata->getbyidx
   storedata->safe-getbyidx
   storedata->setbyidx
   storedata->safe-setbyidx
   ))

(define-unit store@
  (import data-structures^)
  (export store^)

  ;; Define Store -- implement by schema list
  (define-datatype store store?
    (empty-store)
    (extend-store
     (ref number?)
     (val (lambda(_)#t))
     (s store?)))
  (define store->nextref
    (lambda (s)
      (cases store s
        (empty-store () 1)
        (extend-store (ref eval _) (+ ref 1)))))
  (define store->findref
    (lambda (s s-ref)
      (cases store s
        (extend-store (ref eval nexts)
                      (cond
                        ((equal? s-ref ref) eval)
                        ((> s-ref ref)
                         (eopl:error 'store->findref "invalid ref ~s in store ~s" s-ref s))
                        (else
                         (store->findref nexts s-ref))))
        (empty-store ()
                     (eopl:error 'store->findref "invalid ref ~s in store ~s" s-ref s)))))
  (define store->setref
    (lambda (s s-ref eval)
      (cases store s
        (empty-store ()
                     (eopl:error 'store->setref "invalid ref ~s in store ~s" s-ref s))
        (extend-store (ref old-eval nexts)
                      (cond
                        ((equal? s-ref ref)
                         (extend-store
                          ref
                          eval
                          nexts))
                        ((> s-ref ref)
                         (eopl:error 'store->setref "invalid ref ~s in store ~s" s-ref s))
                        (else
                         (let ((ns (store->setref nexts s-ref eval)))
                           (extend-store
                            ref
                            old-eval
                            ns))))))))

  ;; Define Answer -- include expval and store
  (define-datatype answer answer?
    (an-answer
     (eval expval?)
     (s store?)))
  (define answer->eval
    (lambda (aw)
      (cases answer aw
        (an-answer (eval _) eval))))
  (define answer->store
    (lambda (aw)
      (cases answer aw
        (an-answer (_ s) s))))

  ;; Define Store Data Interface
  (define storedata->new
    (lambda (def-val init-vals num store make-fun)
      (define make-store
        (lambda (init-vals num ref store)
          (if (zero? num)
              store
              (let* ((is-null (null? init-vals))
                     (val (if is-null def-val
                              (car init-vals)))
                     (next-vals (if is-null init-vals
                                    (cdr init-vals))))
                (make-store next-vals
                            (- num 1)
                            (+ ref 1)
                            (extend-store ref
                                          val
                                          store))))))
      (let ((ref (store->nextref store)))
        (an-answer
         (make-fun ref)
         (make-store init-vals
                     num
                     ref
                     store)))))

  (define storedata->safe-handler
    (lambda (fun sd idx max-num . vars)
      (if (>= idx max-num)
          (eopl:error 'storedata-check "invalid idx for storedata ~s:~s" sd idx)
          (apply fun sd idx vars))))

  (define storedata->getbyidx
    (lambda (sd idx store get-ref-fun)
      (let ((ref (get-ref-fun sd)))
        (store->findref store (+ ref idx)))))

  (define storedata->safe-getbyidx
    (lambda (sd idx max-num store get-ref-fun)
      (storedata->safe-handler
       storedata->getbyidx sd idx max-num
       store
       get-ref-fun)))

  (define storedata->setbyidx
    (lambda (sd idx val store get-ref-fun)
      (let ((ref (get-ref-fun sd)))
        (an-answer
         sd
         (store->setref
          store
          (+ ref idx)
          val)))))

  (define storedata->safe-setbyidx
    (lambda (sd idx max-num val store get-ref-fun)
      (storedata->safe-handler
       storedata->setbyidx sd idx max-num
       val
       store
       get-ref-fun)))
  )
