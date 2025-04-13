#lang racket

(require "ch4-call-by-ref.rkt")

(define call-by-ref-test1
  " letmutable b = 3
      in let p = proc (&x, &y)
                    begin
                      set x = 4;
                      y
                    end
          in (p b b)")
(define call-by-ref-test2
  " letmutable b = 3
      in let p = proc (&x, &y, z)
                      set x = (+ (+ x y) z)
          in begin (p b 3 2); b end")
(define call-by-ref-test3
  " letmutable a=3 b=4
      in let p = proc (&x, &y)
                  let z = x
                    in begin
                      set x = y;
                      set y = z
                      end
          in begin (p a b); (- a b) end")

