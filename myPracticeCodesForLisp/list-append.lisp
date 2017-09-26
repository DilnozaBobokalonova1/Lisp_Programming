(defun list-append (L1 L2)
	(cond
	((null L1) L2)
	((cons (first L1) (list-append (rest L1) L2)))))
