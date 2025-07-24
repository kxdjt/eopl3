#lang racket

(provide (all-defined-out))

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
(define thread-test1
  "let buffer= 0
    in let producer= proc(n)
                      letrec
                          wait(k)= if (zero? k)
                                   then set buffer=n
                                   else begin
                                          (print (- k -200));
                                          (wait (- k 1))
                                        end
                      in (wait  5)
          in let consumer=proc(d)
                letrec busywait(k)= if (zero? buffer)
                                    then begin
                                          (print (- k -100));
                                          (busywait (- k -1))
                                         end
                                    else buffer
                        in (busywait 0)
              in begin
                  spawn proc(d)(producer 44);
                  (print 300);
                  (consumer 86)
                end")
(define unsafe-counter-test
  " let x = 0
    in let incr_x = proc (id)
                      proc (dummy)
                        set x = (- x (- 2 (+ 1 2)))
        in begin
            (spawn(incr_x 100));
            (spawn(incr_x 200));
            (spawn(incr_x 300))
           end")
(define safe-counter-test
  "let x = 0
    in let mut = (mutex)
        in let incr_x = proc (id)
                          proc (dummy)
                            begin
                              (wait mut);
                              set x = (- x (- 2 (+ 1 2)));
                              (signal mut)
                            end
          in begin
              (spawn(incr_x 100));
              (spawn(incr_x 200));
              (spawn(incr_x 300))
             end")
(define yield-test
  "let x = 0
   in let incr_x = proc()
                    begin
                      (print x);
                      (yield);
                      set x = (- x -1);
                      (print x)
                    end
      in begin
          (spawn incr_x);
          (yield);
          (print x)
         end")

