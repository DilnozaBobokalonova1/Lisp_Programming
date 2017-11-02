(defun but-last (L)
	(cond
	((null (rest L)) nil)
	((cons (first L) (but-last (rest L))))))
