#lang racket

(require "ch3-let-with-sllgen.rkt")

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
(define traceproc-test
  "let makerec = traceproc(f)
                  let d = traceproc(g)traceproc(x) ((f (g g)) x)
                  in (d d)
   in let makefact = traceproc(f)
                      traceproc(x)
                       if (zero? (- x 1)) then 1
                          else (* x (f (- x 1)))
      in let fact = (makerec makefact)
        in (fact 3)")
(define dyproc-test
  "let a =3
   in let p =  dyproc (x) (- x a)
          a = 5
      in (- a (p 2))")
(define dyproc-test2
  "let a = 3
   in let p = dyproc(z) a
      in let f = dyproc (a) (p 0)
         in let a = 5
            in (f 2)")
(define letrec-test
  " letrec double(x) = if (zero? x) then 0 else (- (double (- x 1)) -2)
    in (double 6)")
(run letrec-test)
