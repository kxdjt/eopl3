#lang racket

(require "./lang-cps-in.rkt")
(require "./lang-cps-out.rkt")
(require "./tail-form.rkt")
(require "../../common/utils.rkt")
(require "./fmt-cps-in.rkt")

(require eopl)

(provide run)

(define run
  (lambda(exp)
    (init-gident)
    (cps-of-exp exp
                (cps-proc-exp (list 'var)
                              (simple-exp->exp (cps-var-exp 'var))))))

(define make-proc-cont-call-by-let
  (lambda(idents body simple-exp)
    (cps-let-exp idents (list simple-exp)
                 body)))

(define make-proc-cont-call-by-replace
  (lambda(idents body simple-exp)
    (debug-trace "replace" "idents:~s\n body:~s\n simple-exp:~s\n" idents body simple-exp)
    (let ((var (car idents)))
      (define obscured?
        (lambda(vars)
          (ormap (lambda(s-var)
                   (equal? s-var var))
                 vars)))
      (define replace-simple
        (lambda (simp)
          (cases simpleexp simp
            (cps-var-exp (ident)
                         (if (equal? ident var)
                             simple-exp
                             simp))
            (cps-proc-exp (p-vars p-body)
                          (if (obscured? p-vars)
                              simp
                              (cps-proc-exp p-vars
                                            (replace-tfexp p-body))))
            (cps-innerop-exp (op simpls)
                             (cps-innerop-exp op
                                              (map replace-simple
                                                   simpls)))
            (else
             simp))))
      (define replace-tfexp
        (lambda (tfe)
          (cases tfexp tfe
            (simple-exp->exp (simp)
                             (simple-exp->exp
                              (replace-simple simp)))
            (cps-let-exp (let-vars let-simpls let-body)
                         (cps-let-exp let-vars
                                      (map replace-simple
                                           let-simpls)
                                      (if (obscured? let-vars)
                                          let-body
                                          (replace-tfexp let-body))))
            (cps-letrec-exp (letr-idents letr-varss letc-pbodys letc-body)
                            (if (obscured? letr-idents)
                                tfe
                                (cps-letrec-exp letr-idents letr-varss
                                                (map (lambda(vars pbody)
                                                       (if (obscured? vars)
                                                           pbody
                                                           (replace-tfexp pbody)))
                                                     letr-varss
                                                     letc-pbodys)
                                                (replace-tfexp letc-body))))
            (cps-if-exp (s tf1 tf2)
                        (cps-if-exp (replace-simple s)
                                    (replace-tfexp tf1)
                                    (replace-tfexp tf2)))
            (cps-call-exp (simp1 simps)
                          (cps-call-exp
                           (replace-simple simp1)
                           (map replace-simple simps))))))
      (replace-tfexp body))))

(define make-proc-cont-call
  make-proc-cont-call-by-replace)

;;make-send-to-cont : SimpleExp × SimpleExp → TfExp
(define make-send-to-cont
  (lambda (k-exp simple-exp)
    (debug-trace "sent-to-cont" "k-exp:~s\nsimple-exp:~s\n"
                 k-exp simple-exp)
    (cases simpleexp k-exp
      (cps-proc-exp (idents body)
                    (make-proc-cont-call idents body simple-exp))
      (else
       (cps-call-exp k-exp (list simple-exp))))))

(define NEXT-VAR-ID 0)

(define new-ident
  (lambda()
    (set! NEXT-VAR-ID (+ 1 NEXT-VAR-ID))
    (string->symbol (string-append "gs175771-"
                                   (format "~s"
                                           NEXT-VAR-ID)))))
(define init-gident
  (lambda()
    (set! NEXT-VAR-ID 0)))

(define cps-of-exps-imp
  (lambda(get-exps add-c-exps)
    (lambda (exps builder)
      (let helper ((r-exps (get-exps exps))
                   (c-exps '()))
        (if (null? r-exps)
            (builder c-exps)
            (let ((exp (car r-exps)))
              (if (not (simple-exp? exp))
                  (let* ((var-ident (new-ident))
                         (c-exps (add-c-exps c-exps
                                             (cps-var-exp var-ident))))
                    (cps-of-exp exp
                                (cps-proc-exp (list var-ident)
                                              (helper (cdr r-exps)
                                                      c-exps))))
                  (helper (cdr r-exps)
                          (add-c-exps c-exps
                                      (cps-of-simple-exp exp))))))
        ))))
(define cps-of-exps-lr
  (cps-of-exps-imp (lambda(exps) exps)
                   (lambda(c-exps new-exp)
                     (append c-exps (list new-exp)))))
(define cps-of-exps-rl
  (cps-of-exps-imp (lambda(exps) (reverse exps))
                   (lambda(c-exps new-exp)
                     (cons new-exp c-exps))))
(define cps-of-exps
  cps-of-exps-lr)

(define cps-of-exp/ctx
  (lambda (exp context)
    (if (simple-exp? exp)
        (context (cps-of-simple-exp exp))
        (let ((var-ident (new-ident)))
          (cps-of-exp exp
                      (cps-proc-exp (list var-ident)
                                    (context (cps-var-exp var-ident))))))))

;;cps-of-exp: InpExp * Cont(CPS-PROC-EXP/CPS-VAR-EXP) -> TfExp
(define cps-of-exp
  (lambda(in-exp cont)
    (debug-trace "cps-of-exp"
                 (string-append "in-exp:\n" (exp->fmt in-exp) "\n"))
    #| "in-exp:~s\n" (exp->fmt in-exp)) |#
    (cases inpexp in-exp
      (const-exp (num)
                 (make-send-to-cont cont (cps-const-exp num)))
      (var-exp (ident)
               (make-send-to-cont cont (cps-var-exp ident)))
      (if-exp (exp1 exp2 exp3)
              (cps-of-if-exp exp1 exp2 exp3 cont))
      (let-exp (idents exps body)
               (debug-trace "cps-of-let-exp" "idents:~s\n" idents)
               (cps-of-let-exp-to-proc idents exps body cont))
      (letrec-exp (p-names p-varss p-bodys body)
                  (cps-letrec-exp p-names
                                  (map (lambda(p-vars)
                                         (append p-vars (list 'cont%00)))
                                       p-varss)
                                  (map (lambda(p-body)
                                         (cps-of-exp p-body (cps-var-exp 'cont%00)))
                                       p-bodys)
                                  (cps-of-exp body cont)))
      (proc-exp (idents body)
                (make-send-to-cont cont
                                   (cps-proc-exp
                                    (append idents (list 'cont%00))
                                    (cps-of-exp body (cps-var-exp 'cont%00)))))
      (call-exp (exp1 exps)
                (cps-of-call-exp-v2 exp1 exps cont))
      (innerop-exp (op exps)
                   (cps-of-exps exps
                                (lambda(new-exps)
                                  (make-send-to-cont cont
                                                     (cps-innerop-exp
                                                      (cps-of-innerop op)
                                                      new-exps)))))
      )))

(define cps-of-simple-exp
  (lambda (exp)
    (debug-trace "cps-of-simple-exp" "in-exp:~s\n" (exp->fmt exp))
    (cases inpexp exp
      (const-exp (num) (cps-const-exp num))
      (var-exp (ident) (cps-var-exp ident))
      (proc-exp (idents body)
                (cps-proc-exp
                 (append idents (list 'cont%00))
                 (cps-of-exp body (cps-var-exp 'cont%00))))
      (innerop-exp (op exps)
                   (cps-innerop-exp (cps-of-innerop op)
                                    (map (lambda(exp)
                                           (cps-of-simple-exp exp))
                                         exps)))
      (else
       (eopl:error 'cps-of-simple-exp "Not simple exp:~s\n" exp)))))
(define cps-of-innerop
  (lambda (op)
    (cases inner-operator op
      (none-op (op) (cps-none-op op))
      (binary-op (op) (cps-binary-op op))
      (unary-op (op) (cps-unary-op op))
      (any-op (op) (cps-any-op op)))))
(define cps-of-call-exp
  (lambda (exp1 exps cont)
    (cps-of-exps (cons exp1 exps)
                 (lambda (new-exps)
                   (cps-call-exp
                    (car new-exps)
                    (append (cdr new-exps)
                            (list cont)))))))
(define cps-of-call-exp-v2
  (lambda(exp1 exps cont)
    (cps-of-exps (append exps (list exp1))
                 (lambda (new-exps)
                   (let* ((rev-exps (reverse new-exps))
                          (p (car rev-exps))
                          (rands (reverse (cdr rev-exps))))
                     (cps-call-exp
                      p
                      (append rands (list cont))))))))

(define cps-of-if-exp
  (lambda(exp1 exp2 exp3 cont)
    (cps-of-exp/ctx exp1
                    (lambda(simp1)
                      (cases simpleexp cont
                        (cps-proc-exp (idents body)
                                      (let ((var-ident (new-ident)))
                                        (cps-let-exp (list var-ident)
                                                     (list cont)
                                                     (cps-if-exp simp1
                                                                 (cps-of-exp exp2 (cps-var-exp var-ident))
                                                                 (cps-of-exp exp3 (cps-var-exp var-ident))))))
                        (else
                         (cps-if-exp simp1
                                     (cps-of-exp exp2 cont)
                                     (cps-of-exp exp3 cont))))))))
(define cps-of-let-exp
  (lambda(idents exps body cont)
    (if (null? exps)
        (cps-let-exp idents exps
                     (cps-of-exp body cont))
        (cps-of-exps exps
                     (lambda(new-exps)
                       (cps-let-exp idents
                                    new-exps
                                    (cps-of-exp body cont)))))))

(define cps-of-let-exp-to-proc
  (lambda(idents exps body cont)
    (let helper((r-exps exps)
                (r-idents idents))
      (if (null? r-exps)
          (cps-of-exp body cont)
          (let ((exp (car r-exps)))
            (cps-of-exp exp
                        (cps-proc-exp (list (car r-idents))
                                      (helper (cdr r-exps)
                                              (cdr r-idents)))))))))
