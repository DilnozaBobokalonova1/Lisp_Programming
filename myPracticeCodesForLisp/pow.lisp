(defun power (B E)
	(if (= E 1)
		B
	(* B (power B (- E 1)))))
