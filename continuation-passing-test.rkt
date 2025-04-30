#lang racket

(require "ch5-continuation-passing.rkt")
#| (require "ch5-continuation-passing-by-dt.rkt") |#


(define makemult-test
  "let makemult = proc (maker)
                   proc (x)
                    if (zero? x) then 0
                      else (- ((maker maker) (- x 1)) (- 0 4))
  in let times4 = proc(x) ((makemult makemult) x)
     in (times4 3)")
(define factorial-test
  "let makefact = proc(maker)
                    proc(x)
                      if (zero? (- x 1)) then 1
                        else (* x ((maker maker) (- x 1)))
  in let fact = proc(x) ((makefact makefact) x)
    in (fact 3)")
(define makerec-test
  "let makerec = proc(f)
                  let d = proc(g)proc(x) ((f (g g)) x)
                  in (d d)
   in let makefact = proc(f)
                      proc(x)
                       if (zero? (- x 1)) then 1
                          else (* x (f (- x 1)))
      in let fact = (makerec makefact)
        in (fact 3)")
(define letrec-test
  "letrec p(x) = if (less? x 0)
                 then x
                 else (p (- x 1))
     in (p 1)")
(define let-test
  "let a = 1
       b = 2
    in (+ a b)")
(define let*-test
  "let* a = 1
        b = (+ a 1)
    in (+ a b)")
(define proc-test
  "let p=proc(x,y) (+ x y)
    in (p 1 2)")
(define begin-test
  "begin (+ 1 2);(+ 2 3) end")
(define fact-test
  "letrec fact(n) = if (zero? n)
                    then 1
                    else (* n (fact (- n 1)))
     in (fact 4)")
(define fact-iter-test
  "letrec fact-iter-acc(n a) =
                    if (zero? n) then a
                    else (fact-iter-acc (- n 1) (* n a))
     in let fact-iter=proc(n)
                      (fact-iter-acc n 1)
         in (fact-iter 4)")
