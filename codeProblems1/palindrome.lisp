(defun palindrome (L)  
;;	(lambda (len (list-length L)))
	
	(setq length (list-length L))
	(setq index1 (- (list-length L) 1))
	(setq thehalf (/ (- length (mod (list-length L) 2)) 2))
	(pal L length index1 thehalf))
;;	(lambda (index (- len 1)))
;;	(lambda (half (/ (- len (mod len 2)) 2)))
;;	(pal L (list-length L) (- (list-length L) 1) (/ (- (list-length L) (mod len 2)) 2))) 
(defun pal (L len index half)

	(if (< index half)
		t
	(if (eq  (nth (- len (+ index 1)) L) (nth index L))
		(pal L len (- index 1) half)
	nil)))

