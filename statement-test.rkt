#lang racket

(require "ch4-statement-oriented.rkt")

(define statement-test1
  "var x,y; {x= 3;y = 4;print (+ x y)}")

(define statement-test2
  "var x,y,z;{x = 3;
              y = 4;
              z = 0;
              while (not (zero? x))
                {z= (+ z y); x= (- x 1)};
              print z}")

(define statement-test3
  "var x; {x= 3;
           print x;
           var x; {x = 4;print x};
           print x}")

(define statement-test4
  "var f,x; {f= proc(x,y) (* x y);
             x= 3;
             print (f 4 x)}")
(define statement-test5
  "var x,y,z; {x=3;y=4;z=0;
               do
                { z= (+ z y); x = (- x 1)}
               while (not (zero? x));
               print z}")

