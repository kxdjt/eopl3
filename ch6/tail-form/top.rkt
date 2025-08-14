#lang racket

(require (prefix-in cpsin- "./inter-cps-in.rkt"))
(require (prefix-in cpsout- "./inter-cps-out.rkt"))
(require (prefix-in cpsoutrm- "./inter-cps-out-rmcont.rkt"))
(require (prefix-in cpsoutreg- "./inter-cps-out-reg.rkt"))
(require (rename-in "./tail-form.rkt"
                    (run tail-form?)))

(require "./test.rkt")

(define ns (variable-reference->namespace (#%variable-reference)))

(define (string->ident s)
  (define sym (string->symbol s))
  (eval sym ns))

(define out-inters
  (list "cpsout" "cpsoutrm" "cpsoutreg"))

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

(define run-test
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

#| (define run-test-old |#
#|   (lambda (test-case) |#
#|     (let* ((test-name (car test-case)) |#
#|            (test-res (cdr test-case)) |#
#|            (in-teststr (string-append test-name "-cps-in")) |#
#|            (out-teststr (string-append test-name "-cps-out")) |#
#|            (in-res (cpsin-expval->schemaval |#
#|                     (cpsin-run (string->ident in-teststr)))) |#
#|            (out-res (cpsout-expval->schemaval |#
#|                      (cpsout-run (string->ident out-teststr)))) |#
#|            (outrm-res (cpsoutrm-expval->schemaval |#
#|                        (cpsoutrm-run (string->ident out-teststr)))) |#
#|            (outreg-res (cpsoutreg-expval->schemaval |#
#|                         (cpsoutreg-run (string->ident out-teststr))))) |#
#|       (if (and |#
#|            (equal? in-res test-res) |#
#|            (equal? out-res test-res) |#
#|            (equal? outrm-res test-res) |#
#|            (equal? outreg-res test-res)) |#
#|           (printf "pass: case:~s\n" test-name) |#
#|           (printf "fail: case:~s in-res:~s out-res:~s outrm-res:~s outreg-res:~s test-res:~s\n" |#
#|                   test-name in-res out-res outrm-res outreg-res test-res))))) |#

(define run-all-test
  (lambda(test-fun)
    (define helper
      (lambda(test-cases)
        (if (null? test-cases)
            (printf "finish all cases!\n")
            (begin
              (run-test (car test-cases) test-fun)
              (helper (cdr test-cases))))))
    (helper cps-trans-test)))
