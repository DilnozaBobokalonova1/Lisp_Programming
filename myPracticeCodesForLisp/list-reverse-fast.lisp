(defun list-reverse (L)defun list-reverse L
	(list-reverse-acum L nil)) nil)))

(defun list-reverse-acum( L A )
	(if (= (null L) nil)
		A
	(list-reverse-acum (rest L) (cons (first L) A))))
