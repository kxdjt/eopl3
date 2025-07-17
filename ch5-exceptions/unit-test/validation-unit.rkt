#lang racket

(require "../procedure-sig.rkt")

(define-unit simple-proc-def@
  (import)
  (export proc-def^)
  (define procedure
    (lambda () 1))
  (define dynamicproc
    (lambda () 2))
  (define proc?
    (lambda (proc)
      (procedure? proc))))

(provide simple-proc-def@)
