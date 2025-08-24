#lang racket

(require "./lang-cps-out.rkt")
(require "./fmt-cps-out.rkt")
(require "../../common/utils.rkt")


(require eopl)
(provide run)

(define run
  (lambda (tfexp)
    (debug-expfmt "out-exp"
                  (string-append ":\n" (exp->fmt tfexp) "\n"))
    (init-cont-tag)
    (dtcont-of-tfexp/ctx
     tfexp
     '()
     (lambda(new-tfexp conts)
       (cps-letrec-exp
        (list 'apply-cont%%00)
        (list (list 'cont%% 'cont-val%%))
        (list (make-apply-cont-proc-body conts))
        new-tfexp)))))


;;TFEXP * LIST-OF-CONTS(tag*proc-body) * builder -> TFEXP(with dt-conts)
(define dtcont-of-tfexp/ctx
  (lambda (exp conts builder)
    (debug-trace "dtcont-of-tfexp/ctx"
                 (string-append ":\n" (exp->fmt exp) "\n"))
    (cases tfexp exp
      (cps-let-exp (idents simps body)
                   (if (and (not (null? idents))
                            (equal? (car idents) 'cont%00))
                       (dtcont-of-cont-simp (car simps) conts
                                            (lambda(new-simp new-conts)
                                              (dtcont-of-tfexp/ctx body new-conts
                                                                   (lambda(new-body new-conts)
                                                                     (builder
                                                                      (cps-let-exp idents (list new-simp) new-body)
                                                                      new-conts)))))
                       (dtcont-of-simps simps conts
                                        (lambda (new-simps new-conts)
                                          (dtcont-of-tfexp/ctx body new-conts
                                                               (lambda(new-body n-cons)
                                                                 (builder
                                                                  (cps-let-exp idents
                                                                               new-simps
                                                                               new-body)
                                                                  n-cons)))))))
      (cps-letrec-exp (idents p-varss p-bodies body)
                      (dtcont-of-tfexps p-bodies conts
                                        (lambda(new-tfexps new-conts)
                                          (dtcont-of-tfexp/ctx body new-conts
                                                               (lambda(new-body n-cons)
                                                                 (builder
                                                                  (cps-letrec-exp idents
                                                                                  p-varss
                                                                                  new-tfexps
                                                                                  new-body)
                                                                  n-cons))))))
      (cps-if-exp (simp1 tf1 tf2)
                  (dtcont-of-simp/ctx simp1 conts
                                      (lambda (new-simp new-cons)
                                        (dtcont-of-tfexps (list tf1 tf2)
                                                          new-cons
                                                          (lambda(new-tfs new-cons)
                                                            (builder
                                                             (cps-if-exp new-simp
                                                                         (car new-tfs)
                                                                         (cadr new-tfs))
                                                             new-cons))))))
      (cps-call-exp (simp1 simps)
                    (if (is-cont-call? simp1)
                        (dtcont-of-apply-cont simp1 simps conts builder)
                        (let* ((rev-simps (reverse simps))
                               (cont-simp (car rev-simps))
                               (ex-cont-simps (cons simp1 (reverse (cdr rev-simps)))))
                          (dtcont-of-simps ex-cont-simps conts
                                           (lambda (new-simps new-conts)
                                             (dtcont-of-cont-simp cont-simp new-conts
                                                                  (lambda(new-simp new-conts)
                                                                    (builder
                                                                     (cps-call-exp (car new-simps)
                                                                                   (reverse
                                                                                    (cons new-simp
                                                                                          (reverse
                                                                                           (cdr new-simps)))))
                                                                     new-conts))))))))
      (simple-exp->exp (simp)
                       (dtcont-of-simp/ctx simp conts
                                           (lambda(new-simp new-conts)
                                             (builder
                                              (simple-exp->exp new-simp)
                                              new-conts)))))))

(define dtcont-of-simp/ctx
  (lambda (simp conts builder)
    (debug-trace "dtcont-of-simp/ctx"
                 (string-append ":\n" (exp->fmt simp) "\n"))
    (cases simpleexp simp
      (cps-proc-exp (p-vars body)
                    (dtcont-of-tfexp/ctx body conts
                                         (lambda(new-body new-conts)
                                           (builder
                                            (cps-proc-exp p-vars
                                                          new-body)
                                            new-conts))))
      (cps-innerop-exp (op simps)
                       (dtcont-of-simps simps conts
                                        (lambda (new-simps new-conts)
                                          (builder
                                           (cps-innerop-exp op new-simps)
                                           new-conts))))
      (else
       (builder simp conts)))))

(define dtcont-of-cont-simp
  (lambda (simp conts builder)
    (cases simpleexp simp
      (cps-proc-exp (p-vars body)
                    (make-cont p-vars body
                               (lambda(cont-exp new-conts)
                                 (builder cont-exp (append new-conts conts)))))
      (else
       (dtcont-of-simp/ctx simp conts builder)))))

(define make-dtcont-of-exps
  (lambda(ctx-fun)
    (lambda (exps conts builder)
      (let helper ((exps exps)
                   (conts conts)
                   (new-exps '()))
        (if (null? exps)
            (builder (reverse new-exps) conts)
            (ctx-fun (car exps) conts
                     (lambda (new-exp new-conts)
                       (helper (cdr exps)
                               new-conts
                               (cons new-exp new-exps)))))))))
(define dtcont-of-tfexps
  (make-dtcont-of-exps
   dtcont-of-tfexp/ctx))
(define dtcont-of-simps
  (make-dtcont-of-exps
   dtcont-of-simp/ctx))

(define is-cont-call?
  (lambda (simp)
    (cases simpleexp simp
      (cps-var-exp (ident)
                   (if (equal? ident 'cont%00)
                       #t
                       #f))
      (else
       #f))))
(define is-cont-proc?
  (lambda (p-vars)
    (and (equal? 1 (length p-vars))
         (string-prefix? (symbol->string (car p-vars)) "gs175771-"))))
(define dtcont-of-apply-cont
  (lambda (simp1 simps conts builder)
    (dtcont-of-simps simps conts
                     (lambda (new-simps new-conts)
                       (builder
                        (cps-call-exp
                         (cps-var-exp 'apply-cont%%00)
                         (cons simp1 new-simps))
                        new-conts)))))

(define filter-bound-vars
  (lambda (vars bound-vars)
    (define filter-one
      (lambda (var bound-vars)
        (if (null? bound-vars)
            '()
            (if (equal? var (car bound-vars))
                (filter-one var
                            (cdr bound-vars))
                (cons (car bound-vars)
                      (filter-one var
                                  (cdr bound-vars)))))))
    (let helper ((vars vars)
                 (bound-vars bound-vars))
      (if (null? vars)
          bound-vars
          (helper (cdr vars)
                  (filter-one (car vars)
                              bound-vars))))))
(define append-bound-vars
  (lambda (vars bound-vars)
    (append vars
            (filter-bound-vars vars
                               bound-vars))))
(define CONT-TAG -1)
(define new-cont-tag
  (lambda()
    (set! CONT-TAG (+ CONT-TAG 1))
    CONT-TAG))
(define init-cont-tag
  (lambda()
    (set! CONT-TAG -1)))
(define make-cont
  (lambda (p-vars body builder)
    (find-free-vars-tfexp
     body
     p-vars
     '()
     (lambda(cont-vals)
       (let ((cont-tag (new-cont-tag))
             (cont-vals cont-vals))
         (dtcont-of-tfexp/ctx
          body
          '()
          (lambda(new-body new-conts)
            (builder
             (make-cont-exp cont-tag cont-vals)
             (cons (list cont-tag
                         (append p-vars cont-vals)
                         new-body)
                   new-conts)))))))))
(define make-cont-exp
  (lambda (cont-tag cont-vals)
    (let ((cont-vals (map
                      (lambda (ident)
                        (cps-var-exp ident))
                      cont-vals)))
      (cps-innerop-exp
       (cps-any-op "list")
       (cons (cps-const-exp cont-tag)
             cont-vals)))))
(define make-get-list-member-exp
  (lambda (idx lst)
    (if (equal? idx 0)
        (cps-innerop-exp
         (cps-unary-op "car")
         (list lst))
        (make-get-list-member-exp
         (- idx 1)
         (cps-innerop-exp
          (cps-unary-op "cdr")
          (list lst))))))
(define get-cont-vars-vals
  (lambda (cont-lst cont-vals)
    (let helper ((cont-vals (cdr cont-vals))
                 (idx 1)
                 (vars (list (car cont-vals)))
                 (vals (list (cps-var-exp
                              'cont-val%%))))
      (if (null? cont-vals)
          (cons vars vals)
          (helper (cdr cont-vals)
                  (+ idx 1)
                  (cons (car cont-vals) vars)
                  (cons
                   (make-get-list-member-exp idx cont-lst)
                   vals))))))
(define make-apply-cont-proc-body
  (lambda(conts)
    (let* ((cont-var-exp (cps-var-exp 'cont%%))
           (make-cont-match-exp
            (lambda(cont)
              (cps-innerop-exp
               (cps-binary-op "equal?")
               (list
                (make-get-list-member-exp
                 0 cont-var-exp)
                (cps-const-exp (car cont)))))))
      (let helper ((conts conts))
        (if (null? conts)
            (simple-exp->exp
             (cps-const-exp -9))
            (cps-if-exp
             (make-cont-match-exp (car conts))
             (let ((vars-vals (get-cont-vars-vals
                               cont-var-exp
                               (cadar conts))))
               (cps-let-exp
                (car vars-vals)
                (cdr vars-vals)
                (caddar conts)))
             (helper (cdr conts))))))))
(define find-free-vars-tfexp
  (lambda (exp bound-vars free-vars builder)
    (cases tfexp exp
      (cps-let-exp (idents simps body)
                   (find-free-vars-simps
                    simps
                    bound-vars
                    free-vars
                    (lambda(free-vars)
                      (find-free-vars-tfexp
                       body
                       (append-bound-vars idents bound-vars)
                       free-vars
                       (lambda(free-vars)
                         (builder free-vars))))))
      (cps-letrec-exp (p-names p-varss p-bodies body)
                      (let ((new-bound-vars (append-bound-vars p-names bound-vars)))
                        (find-free-vars-procs
                         p-bodies
                         p-varss
                         new-bound-vars
                         free-vars
                         (lambda(free-vars)
                           (find-free-vars-tfexp
                            body
                            new-bound-vars
                            free-vars
                            (lambda(free-vars)
                              (builder free-vars)))))))
      (cps-if-exp (simp1 tf1 tf2)
                  (find-free-vars-simp
                   simp1 bound-vars free-vars
                   (lambda(free-vars)
                     (find-free-vars-tfexps
                      (list tf1 tf2)
                      bound-vars
                      free-vars
                      (lambda(free-vars)
                        (builder free-vars))))))
      (cps-call-exp (simp1 simps)
                    (find-free-vars-simps
                     (cons simp1 simps)
                     bound-vars free-vars
                     (lambda(free-vars)
                       (builder free-vars))))
      (simple-exp->exp (simp)
                       (find-free-vars-simp
                        simp bound-vars free-vars
                        (lambda(free-vars)
                          (builder free-vars))))
      )))
(define find-free-vars-simp
  (lambda (simp bound-vars free-vars builder)
    (cases simpleexp simp
      (cps-var-exp (ident)
                   (if (list-member? ident bound-vars)
                       (builder free-vars)
                       (builder (if (list-member? ident free-vars)
                                    free-vars
                                    (append (list ident) free-vars)))))
      (cps-proc-exp (p-vars body)
                    (find-free-vars-tfexp
                     body
                     (append-bound-vars p-vars bound-vars)
                     free-vars
                     (lambda(free-vars)
                       (builder free-vars))))
      (cps-innerop-exp (op simps)
                       (find-free-vars-simps
                        simps
                        bound-vars
                        free-vars
                        (lambda(free-vars)
                          (builder free-vars))))
      (else
       (builder free-vars)))))
(define make-find-free-vars-exps
  (lambda(find-fun)
    (lambda(exps bound-vars free-vars builder)
      (let helper((exps exps)
                  (free-vars free-vars))
        (if (null? exps)
            (builder free-vars)
            (find-fun (car exps) bound-vars free-vars
                      (lambda(free-vars)
                        (helper (cdr exps)
                                free-vars))))))))
(define find-free-vars-tfexps
  (make-find-free-vars-exps
   find-free-vars-tfexp))
(define find-free-vars-simps
  (make-find-free-vars-exps
   find-free-vars-simp))
(define find-free-vars-procs
  (lambda(p-bodies p-varss bound-vars free-vars builder)
    (let helper ((p-bodies p-bodies)
                 (p-varss p-varss)
                 (free-vars free-vars))
      (if (null? p-bodies)
          (builder free-vars)
          (find-free-vars-tfexp
           (car p-bodies)
           (append-bound-vars (car p-varss) bound-vars)
           free-vars
           (lambda(free-vars)
             (helper (cdr p-bodies)
                     (cdr p-varss)
                     free-vars)))))))
