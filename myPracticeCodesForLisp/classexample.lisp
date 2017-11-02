; pos/neg/zero check

(defun check (x)
  (cond ((> x 0)   'positive)
        ((zerop x) 'zero)
        (t         'negative)))


; as always, factorial

(defun fact (n)
  (if (zerop n)
      1
      (* n (fact (- n 1)))))


; length of a list

(defun len (L)
  (if (null L)
      0
      (+ 1 (len (cdr L)))))


; append two lists to make a single one

(defun app (a b)
  (if (null a)
      b
      (cons (car a) 
            (app (cdr a) b))))
