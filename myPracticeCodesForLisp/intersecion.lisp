(defun intersection-of (L1 L2)
	(cond
	((null L1) nil)
	((member (first L1) L2) (cons (first L1) (intersection-of (rest L1) L2)))
	(t (intersection-of (rest L1) L2))))
