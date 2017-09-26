(defun binomial(n r)
	(if (or (= r n) (zerop r)) 
		1
	(+ (binomial (- n 1) (- r 1)) (binomial (- n 1) r))))
