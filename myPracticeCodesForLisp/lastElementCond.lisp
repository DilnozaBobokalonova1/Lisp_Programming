(defun last2 (L)
	(cond
	((null (rest L)) (first L))
	(t (last2 (rest L)))))
