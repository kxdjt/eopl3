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
(define array-test
  " let a = (newarray 2 -99)
        p = proc (x)
              let v = (arrayref x 1)
                  in (arrayset x 1 (- v -1))
      in begin (arrayset a 1 0); (p a); (p a); (arrayref a 1) end")
(define array-test2
  "let a = (newarray 3 -99)
       p = proc (x)
           let* len = (arraylength x 1)
               v = (arrayref x (- len 1))
               in (arrayset x (- len 1) (- v -1))
      in begin (arrayset a 2 0); (p a); (p a); (arrayref a 2) end")
(define call-array-ref-test
  "let a = (newarray 3 0)
       swap = proc(x,y)
              let tmp = x
               in begin
                set x = y;
                set y = tmp
                end
      in begin (arrayset a 1 1);
               (arrayset a 2 2);
               (swap (arrayref a 1) (arrayref a 2));
               (- (arrayref a 1) (arrayref a 2)) end")
(define call-array-ref-test1
  "let a = (newarray 3 0)
       swap = proc(&x,&y)
              let tmp = x
               in begin
                set x = y;
                set y = tmp
                end
      in begin (arrayset a 1 1);
               (arrayset a 2 2);
               (swap (arrayref a 1) (arrayref a 2));
               (- (arrayref a 1) (arrayref a 2)) end")
(define call-array-ref-test2
  "let a = (newarray 3 0)
       swap = proc(&x,&y)
              let tmp = x
               in begin
                set x = y;
                set y = tmp
                end
      in begin (arrayset a 0 1);
               (arrayset a 2 2);
               (swap (arrayref a (arrayref a 1)) (arrayref a 2));
               (- (arrayref a 0) (arrayref a 2)) end")
