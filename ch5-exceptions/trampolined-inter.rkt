#lang racket

(require eopl)
(require "data-structures.rkt")
(require "senv.rkt")
(require "continuation-interface.rkt")

(provide run)


#| Interpreter for the LET language |#
;; run : string -> ExpVal
(define run
  (lambda (string)
    (value-of-program (scan&parse string))))
(define init-senv
  (lambda ()
    (extend-senv 'i (num-val 1)
                 (extend-senv 'v (num-val 5)
                              (extend-senv 'x (num-val 10)
                                           (empty-senv))))))

;; Program -> FinalAnswer = ExpVal
(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (trampolined
                  (value-of/k exp1 (init-senv) (end-cont)))))))

#| Trampolining |#
;; Bounce -> Expval
(define trampolined
  (lambda (bounce)
    (if (expval? bounce)
        bounce
        (trampolined (bounce)))))
#| ;; Proc * Vals * Senv * Cont -> '()->Bounce' |#
#| (define apply-procedure/k |#
#|   (lambda (proc vals senv cont) |#
#|     (lambda () |#
#|       (proc vals senv cont)))) |#
#| (define end-cont |#
#|   (lambda () |#
#|     (lambda (aw) |#
#|       (printf "End of Computation. \n") |#
#|       (answer->eval aw)))) |#
#| (define apply-cont |#
#|   (lambda (cont aw) |#
#|     (lambda() |#
#|       (cont aw)))) |#
