(define err_test
  (lambda (flag err_msg true_msg)
    (if (equal? flag `true) true_msg
			(error err_msg))))

