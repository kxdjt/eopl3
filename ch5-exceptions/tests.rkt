#lang racket

(provide (all-defined-out))

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
     in (fact 8)")
(define fact-iter-test
  "letrec fact-iter-acc(n a) =
                    if (zero? n) then a
                    else (fact-iter-acc (- n 1) (* n a))
     in let fact-iter=proc(n)
                      (fact-iter-acc n 1)
         in (fact-iter 8)")
(define list-test
  "(list 1 (list 2) 3 (list))")
(define implicit-refs-test
  "let g = let count = 0
            in proc (dummy)
                begin
                set count =(- count -1);
                count
                end
    in let a = (g 11)
        in let b = (g 11)
            in (- a b)")
(define exception-test
  "let index = proc (n)
                letrec inner (lst)=
                        if (null? lst)
                        then raise 99
                        else if (zero? (- (car lst) n))
                             then 0
                             else (- (inner (cdr lst)) -1)
                in proc (lst)
                      try (inner lst)
                      catch (x)-1
    in ((index 5) (list 2 3))")
(define exception-test2
  "let fun = proc (x ,y)
             (+ x y)
       in begin
          try (fun ) catch (x) x;
          try (fun 1 2) catch (x) x
          end")
(define exception-test3
  "let fun = proc(x,y)
              (/ x y)
       in begin
          try (fun 2 1) catch (x) x;
          try (fun 2 0) catch (x) x
          end")
(define exception-test4
  "let fun = proc(x, y)
              (/ x y)
         in let run=proc()
                    begin
                      raise -1;
                      (fun 2 1)
                    end
              in try (run)
                 catch (x) x")

(define exception-test5
  "let fun = proc(x, y)
              (/ x y)
         in let run=proc()
                    begin
                      raise -1 curcont endraise;
                      (fun 2 1)
                    end
              in try (run)
                 catch (x) x")
(define exception-test6
  "let fun = proc(x, y)
              (/ x y)
         in let run=proc()
                    begin
                      raise -1 endraise;
                      (fun 2 1)
                    end
              in try (run)
                 catch (x) x")
(define exception-test7
  "let g = proc(x)
           letcc cont
            in (x cont)
       h = proc(y)
           throw 2
           to y
    in (+ (g h) 3)")
(define exception-test8
  "let g = proc(x)
           letcc* cont
            in (x cont)
       h = proc(y)
           (y 2)
    in (+ (g h) 3)")
