#lang racket

(require (prefix-in dt- "remove-first/data-structure.rkt"))
(require (prefix-in il- "remove-first/inlined-procedural.rkt"))
(require (prefix-in pr- "remove-first/procedural.rkt"))
(require (prefix-in re- "remove-first/registerized.rkt"))

(require (prefix-in dt- "list-sum/data-structure.rkt"))
(require (prefix-in il- "list-sum/inlined-procedural.rkt"))
(require (prefix-in pr- "list-sum/procedural.rkt"))
(require (prefix-in re- "list-sum/registerized.rkt"))

(require (prefix-in dt- "occurs-free/data-structure.rkt"))
(require (prefix-in il- "occurs-free/inlined-procedural.rkt"))
(require (prefix-in pr- "occurs-free/procedural.rkt"))
(require (prefix-in re- "occurs-free/registerized.rkt"))

(require (prefix-in dt- "subst/data-structure.rkt"))
(require (prefix-in il- "subst/inlined-procedural.rkt"))
(require (prefix-in pr- "subst/procedural.rkt"))
(require (prefix-in re- "subst/registerized.rkt"))

(require "./occurs-free/occurs-free.rkt")
(require "./subst/subst.rkt")
(require "./list-sum/succinct-rep.rkt")
(require "test.rkt")

(define run-test
  (lambda (fun test-cases)
    (if (null? test-cases)
        'none
        (let* ((args (caar test-cases))
               (res (cdar test-cases))
               (rel-res (apply fun args)))
          (begin
            (if (equal? res rel-res)
                (printf "pass: args:~s\n" args)
                (printf "fail: args:~s res:~s rel-res:~s\n" args res rel-res))
            (run-test fun (cdr test-cases)))))))

(define ns (variable-reference->namespace (#%variable-reference)))

(define (string->proc s)
  (define sym (string->symbol s))
  (eval sym ns))

(define run-test-by-str
  (lambda (fun-str test-cases)
    (let ((fun (string->proc fun-str)))
      (run-test fun test-cases)
      (printf "finish test for ~s\n" fun-str))))

(define pre-list '("dt-" "il-" "pr-" "re-"))

(define run-all-tests
  (lambda (fun-name test-cases)
    (define helper
      (lambda (prelst)
        (if (null? prelst)
            (printf "finish all test for ~s\n" fun-name)
            (let* ((pre (car prelst))
                   (fun-str (string-append pre fun-name)))
              (run-test-by-str fun-str test-cases)
              (helper (cdr prelst))))))
    (helper pre-list)))

(define run-remove-first
  (lambda()
    (run-all-tests "remove-first" remove-first-test)))
(define run-list-sum
  (lambda()
    (run-test-by-str "list-sum" list-sum-test)
    (run-all-tests "list-sum" list-sum-test)
    ))
(define run-occurs-free
  (lambda()
    (run-all-tests "occurs-free?" occurs-free-test)))
(define run-subst
  (lambda()
    (run-all-tests "subst" subst-test)))

