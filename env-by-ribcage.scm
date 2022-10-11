(define empty-env
  (lambda () `()))
(define extend-env
  (lambda (var val env)
    (if (null? env)
        (cons (list (list var) (list val)) env)
        (cons (list (cons var (caar env)) (cons val (cadar env)))
              (cdr env)))))
(define apply-env-in-ribs
  (lambda (vars vals var)
    (if (null? vars) `()
        (if (equal? (car vars) var) (car vals)
            (apply-env-in-ribs (cdr vars) (cdr vals) var)))))
(define apply-env
  (lambda (env search-var)
    (if (null? env)
        (error "no binding found" search-var)
        (if (null? (apply-env-in-ribs (caar env) (cadar env) search-var))
            (apply-env (cdr env) search-var)
            (apply-env-in-ribs (caar env) (cadar env) search-var)))))
(define empty-env?
  (lambda (env)
    (null? env)))
(define has-binding?
  (lambda (env s)
    (if (empty-env? env) #f
        (if (null? (apply-env-in-ribs (caar env) (cadar env) s))
            (has-binding? (cdr env) s)
            #t))))
(define extend-env*
  (lambda (vars vals env)
    (cons (list vars vals) env)))
