(defun recursive-length (L)
	(if (null L)
		0

	(1+ (recursive-length (rest L)))))
