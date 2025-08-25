#lang racket

(require (prefix-in cpsin- "./inter-cps-in.rkt"))
(require (prefix-in cpsout- "./inter-cps-out.rkt"))
(require (prefix-in cpsoutrm- "./inter-cps-out-rmcont.rkt"))
(require (prefix-in cpsoutreg- "./inter-cps-out-reg.rkt"))
(require (rename-in "./tail-form.rkt"
                    (run tail-form?)))
(require "./lang-cps-in.rkt")
(require "./lang-cps-out.rkt")
(require (rename-in "./translator.rkt"
                    (run cps-of-exp)))
(require (rename-in "./translator-dtcont.rkt"
                    (run translator-dt)))
(require (rename-in "./translator-dtproc.rkt"
                    (run translator-dp)))

(require "./fmt-cps-out.rkt")

(require "./test.rkt")

(require eopl)

(define ns (variable-reference->namespace (#%variable-reference)))

(define (string->ident s)
  (define sym (string->symbol s))
  (eval sym ns))

(define out-inters
  (list "cpsout" "cpsoutrm" "cpsoutreg"))

(define test-funs
  (list "trans-test" "tail-form-test" "translator-test" "translator-dt-test"
        "translator-dp-test"))

(define trans-test
  (lambda(in-test out-test test-res)
    (let ((in-res (cpsin-expval->schemaval
                   (cpsin-run in-test))))
      (and
       (equal? in-res test-res)
       (andmap (lambda(out-inter-str)
                 (let* ((out-run (string->ident
                                  (string-append out-inter-str "-run")))
                        (out-expval->schemaval
                         (string->ident
                          (string-append out-inter-str "-expval->schemaval")))
                        (out-res (out-expval->schemaval
                                  (out-run out-test)))
                        (is-equal (equal? out-res test-res)))
                   (begin
                     (if (not is-equal)
                         (printf "out-run:~s res:~s test-res:~s\n"
                                 out-inter-str out-res test-res)
                         'none)
                     is-equal)))
               out-inters)))))

(define tail-form-test
  (lambda(in-test out-test test-res)
    (let ((cps-in-res (tail-form? in-test))
          (cps-out-res (tail-form? out-test)))
      (if (and
           (not cps-in-res)
           cps-out-res)
          #t
          (begin
            (printf "cps-in-res:~s cps-out-res:~s\n"
                    cps-in-res cps-out-res)
            #f)))))

(define print-trans-exp
  (lambda (in-test)
    (let ((cps-exp
           (cases program (scan&parse-cps-in in-test)
             (a-program (exp1)
                        (cps-of-exp exp1)))))
      (printf (string-append "trans-res:\n"
                             (exp->fmt cps-exp)
                             "\n")))))

(define translator-test
  (lambda (in-test _ test-res)
    (let* ((cps-exp
            (cases program (scan&parse-cps-in in-test)
              (a-program (exp1)
                         (cps-of-exp exp1))))
           (res (cpsout-expval->schemaval
                 (cpsout-value-of-program
                  (cps-a-program cps-exp)))))
      (if (equal? res test-res)
          #t
          (printf "trans-res:~s test-res:~s\n"
                  res test-res))
      )))

(define make-translator-print
  (lambda(translator-fun)
    (lambda (in-test . _)
      (let* ((cps-exp
              (cases program (scan&parse-cps-in in-test)
                (a-program (exp1)
                           (cps-of-exp exp1))))
             (trans-exp (translator-fun cps-exp)))
        (printf (string-append "trans-res:\n"
                               (exp->fmt trans-exp)
                               "\n"))))))
(define make-translator-test
  (lambda(translator-fun)
    (lambda (in-test _ test-res)
      (let* ((cps-exp
              (cases program (scan&parse-cps-in in-test)
                (a-program (exp1)
                           (cps-of-exp exp1))))
             (trans-exp (translator-fun cps-exp)))
        (let ((res (cpsout-expval->schemaval
                    (cpsout-value-of-program
                     (cps-a-program trans-exp)))))
          (if (equal? res test-res)
              #t
              (printf "trans-res:~s test-res:~s\n"
                      res test-res)))))))
(define print-translator-dp
  (make-translator-print
   translator-dp))
(define print-translator-dt
  (make-translator-print
   translator-dt))
(define translator-dp-test
  (make-translator-test
   translator-dp))
(define translator-dt-test
  (make-translator-test
   translator-dt))

(define run-one-test
  (lambda (test-case test-fun)
    (let* ((test-name (car test-case))
           (test-res (cdr test-case))
           (in-teststr (string-append test-name "-cps-in"))
           (out-teststr (string-append test-name "-cps-out"))
           (in-test (string->ident in-teststr))
           (out-test (string->ident out-teststr)))
      (if (test-fun in-test out-test test-res)
          (printf "pass: case:~s\n" test-name)
          (printf "fail: case:~s\n" test-name)))))

(define run-tests
  (lambda(test-fun)
    (define helper
      (lambda(test-cases)
        (if (null? test-cases)
            (printf "finish all cases!\n")
            (begin
              (run-one-test (car test-cases) test-fun)
              (helper (cdr test-cases))))))
    (helper cps-trans-test)))

(define run-all-test
  (lambda()
    (let helper ((test-funs test-funs))
      (if (null? test-funs)
          (printf "finish all tests!\n")
          (let* ((test-fun-str (car test-funs))
                 (test-fun (string->ident test-fun-str)))
            (printf "------start run test:~s-------\n" test-fun-str)
            (run-tests test-fun)
            (printf "------end test:~s--------\n" test-fun-str)
            (helper (cdr test-funs)))))))
