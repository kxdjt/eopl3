#lang racket

(require "ch4-implicit-mutable-ref-v2.rkt")

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
(define implicit-refs-test3
  "let times4 = 0
      in begin
          set times4 = proc (x)
                        if (zero? x)
                        then 0
                        else (- (times4 (- x 1)) -4);
          (times4 3)
        end")
(define implicit-refs-test4
  "letmutable times4 = 0
      in begin
          set times4 = proc (x)
                        if (zero? x)
                        then 0
                        else (- (times4 (- x 1)) -4);
          (times4 3)
        end")
(define implicit-refs-test5
  "let x = 11
    in let p = proc (y) (- y x)
        in (- setdynamic x = 17 during (p 22)
            (p 13))")

(define mutpair-test
  "let glo = (newpair 11 22)
        in let f = proc (loc)
                      let d1 = (setright loc (left loc))
                          in let d2 = (setleft glo 99)
                              in (- (left loc) (right loc))
              in (f glo)")
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
