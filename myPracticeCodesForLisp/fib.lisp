(defun fib (x)
	(if (or (zerop x) (= x 1))
		1
	(+ (fib(- x 1)) (fib (- x 2)))))
