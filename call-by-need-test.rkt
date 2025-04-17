#lang racket

(require "ch4-call-by-need.rkt")

(define test1
  "let p = lazyproc(x,y)
           (+ x y)
    in (p (+ 1 2) 3)")

(define test2
  "let p = lazyproc(x,y)
           (x y)
       p2 = proc(x)
            (+ x 3)
    in (p p2 2) ")
(define test3
  "let makerec = lazyproc (f)
                  let d = lazyproc (x) 
                            (f (x x)) 
                    in (f (d d)) 
      in let maketimes4 = lazyproc (f) 
                            lazyproc (x) 
                              if (zero? x) 
                              then 0 
                              else (- (f (- x 1)) -4) 
         in let times4 = (makerec maketimes4) 
             in (times4 3)")
(define lazy-let-test
  "letlazy* a = proc(x,y) (x x y)
            b = (a a 1)
            c = proc(x,y) y
    in (a c 2)")


