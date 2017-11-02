(defun nth-list(N L) L
	(if (null L) 
		nil
	(if (= N 0)(= N 0)
		(first L)
	(nth-list(- N 1) (rest L)))))

