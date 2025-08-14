#lang racket

(provide (all-defined-out))

(define removeall-cps-in
  "letrec
     removeall(n s) =
        if null?(s)
        then emptylist()
        else if number?(car(s))
             then if equal?(n car(s))
                  then (removeall n cdr(s))
                  else cons(car(s)
                            (removeall n cdr(s)))
            else cons((removeall n car(s))
                      (removeall n cdr(s)))
    in
      (removeall 2
                 list(1 2 3 2 4))")
(define removeall-cps-out
  "letrec
    removeall(n s cont) =
       if null?(s)
       then (cont emptylist())
       else if number?(car(s))
            then if equal?(n car(s))
                 then (removeall n cdr(s) cont)
                 else (removeall n cdr(s)
                        proc(val)(cont cons(car(s) val)))
            else (removeall n car(s)
                    proc(val)
                      (removeall n cdr(s)
                          proc(val2)
                             (cont cons(val val2))))
    in
      (removeall 2 list(1 2 3 2 4)
                 proc(val) val)")

(define occurs-in?-cps-in
  "letrec
    occurs-in?(n s) =
          if null?(s)
          then 0
          else if number?(car(s))
               then if equal?(n car(s))
                    then 1
                    else (occurs-in? n cdr(s))
               else if (occurs-in? n car(s))
                    then 1
                    else (occurs-in? n cdr(s))
    in
      (occurs-in? 1 list(1 2 3))")
(define occurs-in?-cps-out
  "letrec
     occurs-in?(n s cont) =
         if null?(s)
         then (cont 0)
         else if number?(car(s))
              then if equal?(n car(s))
                   then (cont 1)
                   else (occurs-in? n cdr(s) cont)
              else (occurs-in? n car(s)
                      proc(val)
                         if val
                         then (cont 1)
                         else (occurs-in? n cdr(s) cont))
     in
       (occurs-in? 1 list(1 2 3)
                   proc(val) val)")

(define remfirst-cps-in
  "letrec
    occurs-in?(n s cont) =
      if null?(s)
      then (cont 0)
      else if number?(car(s))
          then if equal?(n car(s))
                then (cont 1)
                else (occurs-in? n cdr(s) cont)
          else (occurs-in? n car(s)
                  proc(val)
                      if val
                      then (cont 1)
                      else (occurs-in? n cdr(s) cont))
    remfirst(n s) =
      letrec
        loop(s) =
            if null?(s)
            then emptylist()
            else if number?(car(s))
                 then if equal?(n car(s))
                      then cdr(s)
                      else cons(car(s) (loop cdr(s)))
                 else if (occurs-in? n car(s))
                      then cons((remfirst n car(s))
                                cdr(s))
                      else cons(car(s)
                                (remfirst n cdr(s)))
      in (loop s)
    in
      (remfirst 2 list(1 2 3 2 4))")
(define remfirst-cps-out
  "letrec
    occurs-in?(n s cont) =
        if null?(s)
        then (cont 0)
        else if number?(car(s))
            then if equal?(n car(s))
                  then (cont 1)
                  else (occurs-in? n cdr(s) cont)
            else (occurs-in? n car(s)
                    proc(val)
                        if val
                        then (cont 1)
                        else (occurs-in? n cdr(s) cont))
    remfirst(n s cont) =
      letrec
        loop(s cont) =
            if null?(s)
            then (cont emptylist())
            else if number?(car(s))
                 then if equal?(n car(s))
                      then (cont cdr(s))
                      else (loop cdr(s)
                                 proc(val)
                                   (cont cons(car(s) val)))
                 else (occurs-in? n car(s)
                                  proc(val)
                                    if val
                                    then (remfirst n car(s)
                                              proc(val2)
                                                (cont cons(val2 cdr(s))))
                                    else (remfirst n cdr(s)
                                              proc(val2)
                                                (cont cons(car(s) val2)))
                      )
      in (loop s cont)
    in
      (remfirst 2 list(1 2 3 2 4)
                proc(val) val)")
(define depth-cps-in
  "letrec
      depth(s) =
          if null?(s)
          then 1
          else if number?(car(s))
               then (depth cdr(s))
               else if less?(add1((depth car(s)))
                                  (depth cdr(s)))
                    then (depth cdr(s))
                    else add1((depth car(s)))
    in
      (depth list(1 list(list(2) list(list(3)))))")
(define depth-cps-out
  "letrec
     depth(s cont) =
        if null?(s)
        then (cont 1)
        else if number?(car(s))
             then (depth cdr(s) cont)
             else (depth car(s)
                         proc(val)
                           (depth cdr(s)
                                  proc(val2)
                                    if less?(add1(val) val2)
                                    then (depth cdr(s) cont)
                                    else (depth car(s)
                                                proc(val3)
                                                  (cont add1(val3)))
                           )
                  )
    in
      (depth list(1 list(list(2) list(list(3))))
             proc(val) val)")
(define depth-with-let-cps-in
  "letrec
      depth(s) =
          if null?(s)
          then 1
          else if number?(car(s))
               then (depth cdr(s))
               else let dfirst = add1((depth car(s)))
                        drest = (depth cdr(s))
                      in if less?(dfirst drest)
                         then drest
                         else dfirst
      in
        (depth list(1 list(list(2) list(list(3)))))")
(define depth-with-let-cps-out
  "letrec
      depth(s cont) =
         if null?(s)
         then (cont 1)
         else if number?(car(s))
              then (depth cdr(s) cont)
              else (depth car(s)
                          proc(val)
                            (depth cdr(s)
                                   proc(val2)
                                    let dfirst= add1(val)
                                        drest= val2
                                        in if less?(dfirst drest)
                                           then (cont drest)
                                           else (cont dfirst))
                    )
      in
        (depth list(1 list(list(2) list(list(3))))
               proc(val) val)")
(define map-cps-in
  "letrec
      map(f l) = if null?(l)
                 then emptylist()
                 else cons((f car(l))
                           (map f cdr(l)))
      square(n) = *(n n)
    in (map square list(1 2 3 4 5))")
(define map-cps-out
  "letrec
     map(f l cont) = if null?(l)
                     then (cont emptylist())
                     else (f car(l)
                             proc(val)
                               (map f cdr(l)
                                    proc(val2)
                                     (cont cons(val val2))))
     square(n cont) = (cont *(n n))
    in (map square list(1 2 3 4 5)
            proc(val) val)")
(define fnlrgtn-cps-in
  "letrec
     fnlrgtn(l n) = if null?(l)
                    then 0
                    else if number?(car(l))
                         then if greater?(car(l) n)
                              then car(l)
                              else (fnlrgtn cdr(l) n)
                         else
                           let car-res = (fnlrgtn car(l) n)
                             in if zero?(car-res)
                                then (fnlrgtn cdr(l) n)
                                else car-res
    in
      (fnlrgtn list(1 list(3 list(2) 7 list(9))) 6)")
(define fnlrgtn-cps-out
  "letrec
     fnlrgtn(l n cont) =
       if null?(l)
       then (cont 0)
       else if number?(car(l))
            then if greater?(car(l) n)
                 then (cont car(l))
                 else (fnlrgtn cdr(l) n cont)
            else
                (fnlrgtn car(l) n
                    proc(val)
                      let car-res=val
                        in if zero?(car-res)
                           then (fnlrgtn cdr(l) n cont)
                           else (cont car-res)
                )
    in
      (fnlrgtn list(1 list(3 list(2) 7 list(9))) 6
               proc(val) val)")
(define every-cps-in
  "letrec
      every(pred l) =
          if null?(l)
          then 1
          else if (pred car(l))
               then (every pred cdr(l))
               else 0
      in (every proc(n)greater?(n 5) list(6 7 8 9))")
(define every-cps-out
  "letrec
     every(pred l cont) =
        if null?(l)
        then (cont 1)
        else (pred car(l)
                   proc(val)
                     if val
                      then (every pred cdr(l) cont)
                      else (cont 0))
    in (every proc(n,cont)
               (cont greater?(n 5))
              list(6 7 8 9)
              proc(val) val)")

(define cps-trans-test
  (list
   (cons "removeall" '(1 3 4))
   (cons "occurs-in?" 1)
   (cons "remfirst" '(1 3 2 4))
   (cons "depth" 4)
   (cons "depth-with-let" 4)
   (cons "map" '(1 4 9 16 25))
   (cons "fnlrgtn" 7)
   (cons "every" 1)
   ))


