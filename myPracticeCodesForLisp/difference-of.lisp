(defun difference-of(L1 L2) 
	(cond
	((null L1) nil)
	((member (first L1) L2) (difference-of (rest L1) L2))
	(t (cons (first L1) (difference-of (rest L1) L2)))))
