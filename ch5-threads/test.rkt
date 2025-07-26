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
                  letrec mywait(k)= if (zero? k)
                                   then set buffer=n
                                   else begin
                                          (print (- k -200));
                                          (mywait (- k 1))
                                        end
                      in (mywait  5)
          in let consumer=proc(d)
                letrec busywait(k)= if (zero? buffer)
                                    then begin
                                          (print (- k -100));
                                          (busywait (- k -1))
                                         end
                                    else buffer
                        in (busywait 0)
              in begin
                  (spawn proc(d)(producer 44));
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

(define letrec-test
  "letrec p(x) = if (less? x 0)
                 then x
                 else (p (- x 1))
     in (p 1)")
(define safe-counter-test2
  "let x = 0
    in let mut = (mutex)
           a-mut = (mutex)
        in let incr_x = proc (id)
                          proc (dummy)
                            begin
                              (wait mut);
                              set x = (- x (- 2 (+ 1 2)));
                              if (equal? x 3) then (signal a-mut) else (print x);
                              (signal mut)
                            end
          in begin
              (wait a-mut);
              (spawn(incr_x 100));
              (spawn(incr_x 200));
              (spawn(incr_x 300));
              (wait a-mut);
              x
             end")
(define thread-test2
  "let buffer= 0
       pr-mut = (mutex)
    in let producer= proc(n)
                  letrec mywait(k)= if (zero? k)
                                   then begin set buffer=n;(signal pr-mut) end
                                   else begin
                                          (print (- k -200));
                                          (mywait (- k 1))
                                        end
                      in (mywait  5)
          in let consumer=proc(d)
                            begin
                              (wait pr-mut);
                              (print d);
                              buffer
                            end
              in begin
                  (wait pr-mut);
                  (spawn proc(d)(producer 44));
                  (print 300);
                  (consumer 86)
                end")
(define safe-counter-test3
  "let* x = 0
       a-mut = (mutex)
       mut = (mutex)
       th_proc = proc (id)
                    letrec loop(n) =
                          begin
                            (print id);
                            (print (+ n 1000));
                            if (equal? n 2)
                            then
                              begin
                                (wait mut);
                                (signal a-mut);
                                (print 9999);
                                (loop (+ n 1))
                              end
                            else
                              (loop (+ n 1))
                            end
                      in (loop 0)
    in begin
        (wait a-mut);
        let th1 = (spawn th_proc)
            th2 = (spawn th_proc)
            th3 = (spawn th_proc)
          in
            begin
              (wait a-mut);
              (kill th1);
              (kill th2);
              (kill th3);
              x
            end
        end")
(define thread-test3
  "let buffer= 0
       mut = (mutex)
    in let producer= proc(n)
                      begin
                        set buffer = (recive);
                        (signal mut)
                      end
          in let consumer=proc(id ,n)
                           (send id n)
              in begin
                 (wait mut);
                 let th1 = (spawn producer)
                    in begin
                      (spawn proc(d)(consumer th1 44));
                      (wait mut);
                      (print buffer);
                      buffer
                      end
                end")
