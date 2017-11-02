(defun list-reverse (L) 
	(list-reverse-acum L nil))
(defun list-reverse-acum (L A)
	(if (null L)
		A
	(list-reverse-acum (rest L) (cons (first L) A))))
