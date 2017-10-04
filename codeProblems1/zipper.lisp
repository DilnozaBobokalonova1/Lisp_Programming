(defun zipper (L1 L2)
	
	(if (null L1) 
		nil
	(cons (cons (car L1) (cons (car L2) '())) (zipper (cdr L1) (cdr L2)))))
