(defun list-member (E L)
	(cond
	((null L) nil)
	((eq E (first L)) t)
	(t (list-member E (rest L)))))
