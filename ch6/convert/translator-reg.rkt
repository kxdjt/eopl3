#lang racket

(require "./lang-cps-out.rkt"
         "./fmt-cps-out.rkt"
         "../../common/utils.rkt")

(require eopl)
(provide run)

(define MAX-PASS-VAR-NUM 0)
(define init-pass-num
  (lambda()
    (set! MAX-PASS-VAR-NUM 0)))

(define run
  (lambda(exp)
    (reg-of-tfexp/ctx exp
                      (lambda(new-exp)
                        (cps-let-exp
                         (build-list MAX-PASS-VAR-NUM
                                     (lambda(idx)
                                       (make-pvar idx)))
                         (build-list MAX-PASS-VAR-NUM
                                     (lambda(idx)
                                       (cps-const-exp -1)))
                         new-exp)))))

(define reg-of-tfexp/ctx
  (lambda(exp builder)
    (cases tfexp exp
      (cps-let-exp (idents simps body)
                   (reg-of-simps simps
                                 (lambda(new-simps)
                                   (reg-of-tfexp/ctx body
                                                     (lambda(body)
                                                       (builder
                                                        (cps-let-exp idents
                                                                     new-simps
                                                                     body)))))))
      (cps-if-exp (simp1 tf1 tf2)
                  (reg-of-simp/ctx simp1
                                   (lambda(new-simp)
                                     (reg-of-tfexps (list tf1 tf2)
                                                    (lambda(new-tfexps)
                                                      (builder
                                                       (cps-if-exp new-simp
                                                                   (car new-tfexps)
                                                                   (cadr new-tfexps))))))))
      (simple-exp->exp (simp)
                       (reg-of-simp/ctx simp
                                        (lambda(new-simp)
                                          (builder
                                           (simple-exp->exp new-simp)))))
      (cps-call-exp (simp1 simps)
                    (reg-of-simps simps
                                  (lambda(new-simps)
                                    (builder
                                     (cps-call-exp
                                      (make-reg-call new-simps simp1)
                                      '()
                                      )))))
      (cps-letrec-exp (idents varss pbodiess body)
                      (reg-of-tfexps pbodiess
                                     (lambda(new-bodiess)
                                       (reg-of-tfexp/ctx body
                                                         (lambda(new-body)
                                                           (builder
                                                            (cps-letrec-exp
                                                             idents
                                                             (map (lambda(vars)
                                                                    '())
                                                                  varss)
                                                             (map (lambda(vars pbody)
                                                                    (make-proc-body vars pbody))
                                                                  varss new-bodiess)
                                                             new-body)))))))
      )))
(define reg-of-simp/ctx
  (lambda(simp builder)
    (builder simp)))

(define make-reg-of-exps
  (lambda(regfun)
    (lambda (exps builder)
      (let helper ((exps exps)
                   (new-exps '()))
        (if (null? exps)
            (builder (reverse new-exps))
            (regfun
             (car exps)
             (lambda(new-exp)
               (helper (cdr exps)
                       (cons new-exp new-exps)))))))
    ))
(define reg-of-simps
  (make-reg-of-exps
   reg-of-simp/ctx))
(define reg-of-tfexps
  (make-reg-of-exps
   reg-of-tfexp/ctx))

(define make-pvar
  (lambda(idx)
    (string->symbol
     (format "val%%-~s" idx))))

(define make-reg-call
  (lambda (simps simp1)
    (cps-innerop-exp
     (cps-any-op "begin")
     (append
      (foldl (lambda(val res)
               (let ((idx (length res)))
                 (cons
                  (cps-set-exp
                   (make-pvar idx)
                   val)
                  res)))
             '()
             simps)
      (list simp1)))
    ))
(define make-proc-body
  (lambda(p-vars body)
    (if (< MAX-PASS-VAR-NUM (length p-vars))
        (set! MAX-PASS-VAR-NUM (length p-vars))
        'none)
    (cps-let-exp
     p-vars
     (build-list (length p-vars)
                 (lambda(idx)
                   (cps-var-exp
                    (make-pvar idx))))
     body)))
