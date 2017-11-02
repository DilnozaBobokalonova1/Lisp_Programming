(defun sum (L) 
	(if (null L)
		0
	(+ (first L) (sum (rest L)))))
