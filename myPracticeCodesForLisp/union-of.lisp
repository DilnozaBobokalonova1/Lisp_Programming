(defun union-of (L1 L2)
	(cond
	((null L1) L2)
	((member (first L1) L2) (union-of (rest L1) L2))
	(t (cons (first L1) (union-of(rest L1) L2)))))
