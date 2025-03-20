#lang racket

#| (require "ch3-lexical-addressing-trimmed.rkt") |#
#| (require "ch3-lexical-addressing.rkt") |#
#| (require "ch3-translate-known-proc.rkt") |#
#| (require "ch4-store-passing-explicit-refs.rkt") |#
(require "ch4-store-passing-implicit-refs.rkt")

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
  " letrec double(x y) = if (zero? x) then 0 else (- (double (- x 1) y) y)
    in (double 6 -2)")
(define letrec-test2
  "letrec
          even(x) = if (zero? x) then 1 else (odd (- x 1))
          odd(x) = if (zero? x) then 0 else (even (- x 1))
  in (odd 13)")
(define dymanic-rec-test1
  " let fact = proc (n) (+ n 1)
    in let fact = proc (n)
                    if (zero? n)
                    then 1
                    else (* n (fact (- n 1)))
        in (fact 5)")
(define dymanic-rec-test2
  " let fact = dyproc (n) (+ n 1)
    in let fact = dyproc (n)
                    if (zero? n)
                    then 1
                    else (* n (fact (- n 1)))
        in (fact 5)")
(define multi-args-lexical-addr-test
  "let makeplus = proc(g)
                proc (x , y)
                 if (zero? y)
                 then x
                 else ((g g) (+ x 1) (- y 1))
       in let plus = (makeplus makeplus)
          in (plus 3 4)")
(define known-proc-test
  "let x = 3
    in let f = proc (y) (- y x)
              in (f 13)")
(define explicit-refs-test
  "let ref = newref(3)
   in begin
        deref(ref);
        setref(ref,5);
        (+ 1 deref(ref))
      end")
(define implicit-refs-test
  " let x = 0
    in letrec even(dummy)
                  = if (zero? x)
                    then 1
                    else begin
                            set x = (- x 1);
                            (odd 888)
                          end
              odd(dummy)
                  = if (zero? x)
                    then 0
                    else begin
                          set x = (- x 1);
                          (even 888)
                        end
      in begin set x = 13; (odd -888) end")
(define implicit-refs-test2
  "let g = let count = 0
            in proc (dummy)
                begin
                set count =(- count -1);
                count
                end
    in let a = (g 11)
        in let b = (g 11)
            in (- a b)")

