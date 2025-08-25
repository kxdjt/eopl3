#lang racket

(require "./lang-cps-out.rkt")
(require "./fmt-cps-out.rkt")
(require "../../common/utils.rkt")

(require eopl)

(provide run)

(define run
  (lambda(tfexp)
    (init-proc-tag)
    (dtproc-of-tfexp/ctx
     tfexp '()
     (lambda (new-tfexp procs)
       (cps-letrec-exp
        (list 'apply-procedure%%00)
        (list (list 'proc%% 'proc-val%%))
        (list (make-apply-procedure-body procs))
        new-tfexp)))))

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

(define get-proc-vars-vals
  (lambda(proc-list proc-val-list proc-save-info)
    (define get-vals
      (lambda (lst-var-exp var-lst idx)
        (let helper ((var-lst var-lst)
                     (idx idx)
                     (vals '()))
          (if (null? var-lst)
              (reverse vals)
              (helper (cdr var-lst)
                      (+ idx 1)
                      (cons
                       (make-get-list-member-exp idx lst-var-exp)
                       vals))))))
    (let* ((free-vars (car proc-save-info))
           (p-vars (cadr proc-save-info))
           (free-vals
            (get-vals proc-list free-vars 1))
           (p-vals
            (get-vals proc-val-list p-vars 0))
           )
      (cons
       (append
        free-vars p-vars)
       (append
        free-vals p-vals)))))

(define make-apply-procedure-body
  (lambda(o-procs)
    (debug-trace "make-apply-procedure-body"
                 "o-procs:~s\n" o-procs)
    (let* ((proc-var-exp (cps-var-exp 'proc%%))
           (make-proc-match-exp
            (lambda(proc)
              (cps-innerop-exp
               (cps-binary-op "equal?")
               (list
                (make-get-list-member-exp
                 0 proc-var-exp)
                (cps-const-exp (car proc)))))))
      (let helper ((procs o-procs))
        (if (null? procs)
            (simple-exp->exp
             (cps-const-exp -9))
            (cps-if-exp
             (make-proc-match-exp (car procs))
             (let ((vars-vals (get-proc-vars-vals
                               proc-var-exp
                               (cps-var-exp 'proc-val%%)
                               (cdar procs))))
               (debug-trace "get-vv-res:"
                            "vars-vals:~s\n
                            proc-save-info:~s\n" vars-vals
                                                 (cdar procs))
               (cps-let-exp
                (car vars-vals)
                (cdr vars-vals)
                (let ((rec-procs
                       (if (null? (cdddr (cdar procs)))
                           '()
                           (car (cdddr (cdar procs))))))
                  (debug-trace "rec-procs"
                               "~s\n" rec-procs)
                  (if (null? rec-procs)
                      (cadddr (car procs))
                      (cps-let-exp
                       (map (lambda(ident-tag)
                              (car ident-tag))
                            rec-procs)
                       (map (lambda(ident-tag)
                              (make-proc-list
                               (cdr ident-tag)
                               (cadr (assoc (cdr ident-tag) o-procs))))
                            rec-procs)
                       (cadddr (car procs)))))))
             (helper (cdr procs))))))))

(define dtproc-of-tfexp/ctx
  (lambda(exp procs builder)
    (debug-trace "dtproc-of-tfexp/ctx"
                 (string-append "\n" (exp->fmt exp) "\n"))
    (cases tfexp exp
      (cps-let-exp (idents simps body)
                   (dtproc-of-simps simps procs
                                    (lambda(new-simps procs)
                                      (dtproc-of-tfexp/ctx body procs
                                                           (lambda(new-body procs)
                                                             (builder
                                                              (cps-let-exp idents new-simps new-body)
                                                              procs))))))
      (cps-letrec-exp (idents p-varss p-bodies body)
                      (dtproc-of-letrec-procs
                       idents p-varss p-bodies procs
                       (lambda (new-p-bodies procs)
                         (dtproc-of-tfexp/ctx body procs
                                              (lambda(new-body procs)
                                                (builder
                                                 (cps-let-exp idents
                                                              new-p-bodies
                                                              new-body)
                                                 procs))))))
      (cps-if-exp (simp1 tf1 tf2)
                  (dtproc-of-simp/ctx simp1 procs
                                      (lambda(new-simp procs)
                                        (dtproc-of-tfexps (list tf1 tf2) procs
                                                          (lambda(new-tfexps procs)
                                                            (builder
                                                             (cps-if-exp new-simp
                                                                         (car new-tfexps)
                                                                         (cadr new-tfexps))
                                                             procs))))))
      (cps-call-exp (simp1 simps)
                    (dtproc-of-simps (cons simp1 simps) procs
                                     (lambda(new-simps procs)
                                       (builder
                                        (cps-call-exp
                                         (cps-var-exp 'apply-procedure%%00)
                                         (list
                                          (car new-simps)
                                          (cps-innerop-exp
                                           (cps-any-op "list")
                                           (cdr new-simps))))
                                        procs))))
      (simple-exp->exp (simp)
                       (dtproc-of-simp/ctx simp procs
                                           (lambda(new-simp procs)
                                             (builder
                                              (simple-exp->exp new-simp)
                                              procs)))))))

(define dtproc-of-simp/ctx
  (lambda(simp procs builder)
    (debug-trace "dtproc-of-simp/ctx"
                 (string-append ":\n" (exp->fmt simp) "\n"))
    (cases simpleexp simp
      (cps-proc-exp (p-vars body)
                    (make-proc p-vars body procs builder))
      (cps-innerop-exp (op simps)
                       (dtproc-of-simps simps procs
                                        (lambda(new-simps procs)
                                          (builder
                                           (cps-innerop-exp op new-simps)
                                           procs))))
      (else
       (builder simp procs)))))

(define make-dtproc-of-exps
  (lambda(dtfun)
    (lambda (exps procs builder)
      (let helper ((exps exps)
                   (procs procs)
                   (new-exps '()))
        (if (null? exps)
            (builder (reverse new-exps) procs)
            (dtfun
             (car exps)
             procs
             (lambda(new-exp procs)
               (helper (cdr exps)
                       procs
                       (cons new-exp new-exps)))))))))
(define dtproc-of-tfexps
  (make-dtproc-of-exps
   dtproc-of-tfexp/ctx))
(define dtproc-of-simps
  (make-dtproc-of-exps
   dtproc-of-simp/ctx))

(define PROC-TAG -1)
(define new-proc-tag
  (lambda()
    (set! PROC-TAG (+ PROC-TAG 1))
    PROC-TAG))
(define init-proc-tag
  (lambda()
    (set! PROC-TAG -1)))

(define make-mult-procs
  (lambda (make-fun . argss)
    (foldr cons '()
           (apply map
                  (lambda args
                    (apply make-fun args))
                  argss))))

(define make-proc-save-info
  (lambda (tag free-vars p-vars body . rec-procs)
    (list tag free-vars p-vars body
          (if (null? rec-procs)
              '()
              (car rec-procs)))))
(define make-proc-list
  (lambda(tag free-vars)
    (let ((proc-vals (map
                      (lambda (ident)
                        (cps-var-exp ident))
                      free-vars)))
      (cps-innerop-exp
       (cps-any-op "list")
       (cons (cps-const-exp tag)
             proc-vals)))))
(define make-proc
  (lambda (p-vars body procs builder)
    (let ((tag (new-proc-tag)))
      (find-free-vars-tfexp
       body
       p-vars
       '()
       (lambda(free-vars)
         (dtproc-of-tfexp/ctx body procs
                              (lambda(new-body procs)
                                (builder
                                 (make-proc-list tag free-vars)
                                 (cons
                                  (make-proc-save-info
                                   tag free-vars p-vars new-body)
                                  procs)))))))))

(define dtproc-of-letrec-procs
  (lambda(idents p-varss p-bodies procs builder)
    (debug-trace "dtproc-of-letrec-procs"
                 "idents:~s p-vars:~s\n" idents p-varss)
    (let* ((ident-tag-map
            (map (lambda(ident)
                   (cons ident (new-proc-tag)))
                 idents))
           (ident-free-vars-map
            (map (lambda(ident p-vars p-body)
                   (cons
                    ident
                    (find-free-vars-tfexp
                     p-body
                     p-vars
                     '()
                     (lambda(_)_))))
                 idents p-varss p-bodies))
           (ident-rec-idents-map
            (map (lambda(ident)
                   (cons ident
                         (intersect
                          idents
                          (cdr (assoc ident ident-free-vars-map)))))
                 idents))
           (rec-ident-tag-map
            (map (lambda(record)
                   (map (lambda(ident)
                          (assoc ident ident-tag-map))
                        (cdr record)))
                 ident-rec-idents-map))
           (filter-free-vars-map
            (map (lambda(record)
                   (cons (car record)
                         (filter-vars idents
                                      (cdr record))))
                 ident-free-vars-map))
           (free-varss
            (map (lambda(ident)
                   (merge-free-vars ident
                                    filter-free-vars-map
                                    ident-rec-idents-map))
                 idents)))
      (debug-trace "dlp"
                   "ident-free-vars-map:~s free-varss:~s rec-ident-tag-map:~s\n"
                   ident-free-vars-map free-varss rec-ident-tag-map)
      (dtproc-of-tfexps
       p-bodies procs
       (lambda(new-p-bodies new-procs)
         (let ((tags
                (map (lambda(record)
                       (cdr record))
                     ident-tag-map)))
           (builder
            (make-mult-procs
             make-proc-list tags free-varss)
            (append
             (make-mult-procs
              make-proc-save-info
              tags free-varss p-varss new-p-bodies
              rec-ident-tag-map)
             new-procs))))))))

(define merge-free-vars
  (lambda (ident free-vars-map rec-idents-map)
    (merge (cdr (assoc ident free-vars-map))
           (map (lambda(ident)
                  (cdr (assoc ident free-vars-map)))
                (cdr (assoc ident rec-idents-map))))))
(define filter-vars
  (lambda (vars next-vars)
    (define filter-one
      (lambda (var next-vars)
        (if (null? next-vars)
            '()
            (if (equal? var (car next-vars))
                (filter-one var
                            (cdr next-vars))
                (cons (car next-vars)
                      (filter-one var
                                  (cdr next-vars)))))))
    (let helper ((vars vars)
                 (next-vars next-vars))
      (if (null? vars)
          next-vars
          (helper (cdr vars)
                  (filter-one (car vars)
                              next-vars))))))
(define merge
  (lambda (lst lsts)
    (if (null? lsts)
        lst
        (merge
         (append lst
                 (filter-vars lst (car lsts)))
         (cdr lsts)))))
(define intersect
  (lambda (lst1 lst2)
    (filter-vars
     (filter-vars
      lst1 lst2)
     lst2)))
(define append-bound-vars
  (lambda (vars bound-vars)
    (append vars
            (filter-vars vars
                         bound-vars))))
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
