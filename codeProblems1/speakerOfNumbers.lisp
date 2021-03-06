(defun speaker (x)
	(setq L (int-to-list x))
	(if (eq (cdr L) nil)
		(setq length 1)
		(setq length (list-length L)))
	(numbers L length 0))

(defun int-to-list (int)
	(assert (>= int 0) nil)
	(if (= int 0) (list 0)
		(loop with result = nil
			while (> int 0) do
				(multiple-value-bind (divisor remainder)
					(floor int 10)
					(push remainder result)
					(setf int divisor))
					finally (return result))))
(defun numbers (L len i)
	(cond
	((eq len 10)
		(if (> (car L) 0)
			(progn
			(princ (num (car L)))
			(princ " ")
			(princ 'billion)
			(princ " ")
			(numbers (cdr L) (- len 1) (1+ i)))))
	 	
	((eq len 9)
		(if (> (car L) 0)
			(progn
			(princ (num (car L)))
			(princ " ")
			(princ 'hundred)
			(princ " ")
			(numbers (cdr L) (- len 1) (1+ i)))
		(numbers (cdr L) (- len 1) (1+ i))))
	
	((eq len 8)
		(cond
		((> (car L) 1)
			(cond
			((eq (nth 1 L) 0)
				(progn
				(princ (num1 (car L)))
				(princ 'million) 
				(princ " ")
				(numbers (cdr (cdr L)) (- len 2) (1+ i))))
			(t (progn
				(princ (num1 (car L)))
				(princ " ")
				(numbers (cdr L) (- len 1) (1+ i))))))
		((= (car L) 1)
			(progn
			(princ (num2 (nth 1 L)))
			(princ " ")
			(princ 'million)
			(princ " ")
			(numbers (cdr (cdr L)) (- len 2) (1+ i))))
		((= (car L) 0)
			(if (eq (nth 1 L) 0)
				 (numbers (cdr (cdr L)) (- len 2) (1+ i))
			(numbers (cdr L) (- len 1) (1+ i))))
		(t (numbers (cdr L) (- len 1) (1+ i))))) 
	((eq len 7)
		(if (> (car L) 0)
			(progn
			(princ (num (car L)))
			(princ " ")
			(princ 'million) 
			(princ " ")
			(numbers (cdr L) (- len 1) (1+ i)))
		(progn 
		(princ 'million)
		(princ " ")
		(numbers (cdr L) (- len 1) (1+ i)))))

	((eq len 6)
		(if (> (car L) 0)
			(progn
			(princ (num (car L)))
			(princ " ")
			(princ 'hundred)
			(princ " ")
			(numbers (cdr L) (- len 1) (1+ i)))
		(numbers (cdr L) (- len 1) (1+ i))))
		
	((eq len 5)
		(cond
		((> (car L) 1)
			(progn
			(princ (num1 (car L)))
			(princ " ")
			(numbers (cdr L) (- len 1) (1+ i))))
		((= (car L) 1)
			(progn 
			(princ (num2 (nth 1 L)))
			(princ " ")
			(princ 'thousand) 
			(princ " ")
			(numbers (cdr (cdr L)) (- len 2) (1+ i))))
		(t (if (eq (nth 1 L) 0) 
			(numbers (cdr (cdr L)) (- len 2) (1+ i)) 
		     (numbers (cdr L) (- len 1) (1+ i))))))
	((eq len 4)
		(if (> (car L) 0)
			(progn
			(princ (num (car L)))
			(princ " ")
			(princ 'thousand)
			(princ " ") 
			(numbers (cdr L) (- len 1) (1+ i)))
		(progn 
			(princ 'thousand)
			(princ " ")
			(numbers (cdr L) (- len 1) (1+ i)))))
	
	((eq len 3)
		(if (> (car L) 0)
			(progn
			(princ (num (car L)))
			(princ " ")
			(princ 'hundred)
			(princ " ") 
			(numbers (cdr L) (- len 1) (1+ i))
			)
		(numbers (cdr L) (- len 1) (1+ i))
	))
	
	((eq len 2)
		(cond
			((= (car L) 1)	
				(progn
				(princ (num2 (nth 1 L)))
				(values))
			)
			((eq (car L) 0)
				(numbers (cdr L) (- len 1) (1+ i)))
			(t
				(cond 
				  ((eq (nth 1 L) 0)
					(progn 
					(princ (num1 (car L)))
					(values)))
				  (t (progn
					(prin1 (num1 (car L)))
					(princ " ")
					(numbers (cdr L) (- len 1) (1+ i))))))))
	
	((eq len 1)
		(if (= i 0)
			(if (= (car L) 0)
				'zero
			(num (car L)))	
		(cond
			((= (car L) 0)
				(values))
			(t 
				(progn 
				(princ (num (car L)))
				(values))))))
		;;(progn
		;;	(prin1 (num (car L)))
		;;	(values)
		;;	(numbers (cdr L) (- len 1))
		;; ))
))

(defun list-append (L1 L2)
	(cond 
	((null L1) L2)
	((cons (first L1) (list-append (rest L1) L2)))))
(defun num (x)
	(cond
	((eq x 1)
		'one)
	((eq x 2)
		'two)
	((eq x 3)
		'three)
	((eq x 4)
		'four)
	((eq x 5)
		'five)
	((eq x 6)
		'six)
	((eq x 7)
		'seven)
	((eq x 8)
		'eight)
	((eq x 9)
		'nine)
	))
(defun num1 (x)
	(cond
	((eq x 1)
		'ten)
	((eq x 2)
		'twenty)
	((eq x 3)
		'thirty)
	((eq x 4)
		'forty)
	((eq x 5)
		'fifty)
	((eq x 6)
		'sixty)
	((eq x 7)
		'seventy)
	((eq x 8)
		'eighty)
	((eq x 9)
		'ninety)
	((eq x 0)
		'())
	))
(defun num2 (x)
	(cond
	((eq x 0)
		'ten)
	((eq x 2)
		'twelve)
	((eq x 3)
		'thirteen)
	((eq x 4)
		'fourteen)
	((eq x 5)
		'fifteen)
	((eq x 6)
		'sixteen)
	((eq x 7)
		'seventeen)
	((eq x 8)
		'eighteen)
	((eq x 9)
		'nineteen)
	((eq x 1)
		'eleven)
	))
