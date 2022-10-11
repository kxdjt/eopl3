(define empty-env
  (lambda () `()))
(define extend-env
  (lambda (var val env)
    (cons (list var val) env)))
(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (error "no binding found" search-var)
        (if (equal? (caar env) search-var)
            (cadar env)
            (apply-env (cdr env) search-var)))))
(define empty-env?
  (lambda (env)
    (null? env)))
(define has-binding?
  (lambda (env s)
    (if (empty-env? env) #f
        (if (equal? (caar env) s) #t
            (has-binding? (cdr env) s)))))
(define extend-env*
  (lambda (vars vals env)
    (if (null? vars) env
        (if (has-binding? env (car vars))
            (error (car vars) "has binding")
            (extend-env* (cdr vars) (cdr vals) (extend-env (car vars) (car vals)
                                                           env))))))
