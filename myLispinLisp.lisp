(defun map1 (f L)
  (if (null L)
      nil
      (cons (funcall f (car L))
            (map1 f (cdr L)))))

(defun reduce3 (f z L)
  (if (null L)
      z
      (reduce3-worker f (car L) (cdr L))))

(defun reduce3-worker (f z L)
  (if (null L)
      z
      (reduce3-worker
          f
          (funcall f z (car L))
          (cdr L))))

(defun zipper (L1 L2)

        (if (null L1)
                nil
        (cons (cons (car L1) (cons (car L2) '())) (zipper (cdr L1) (cdr L2)))))

(defun ALset (AL var val)
  (cond 
     ((null AL)
           (list (list var val)))
  
     ((eq var (caar AL)) 
           (cons (list var val)
                 (cdr AL)))

     (t    (cons (car AL)
                 (ALset (cdr AL) var val)))))


(defun ALget (AL var)
  (cond 
     ((null AL)
           'undefined)
  
     ((eq var (caar AL))
           (cadar AL))

     (t    (ALget (cdr AL) var))))


(defun lisp-eval (AL S)
  (cond
     ((numberp S) 
	 S)
    
     ((eq S nil) 
	 nil)
     ((symbolp S) 
	(ALget AL S))
     ((eq (car S) 'eq)
	(cond
	((and (listp (cadr S)) (listp (caddr S)))
		(eq (lisp-eval AL (second (cadr S))) (lisp-eval AL (second (caddr S)))))
		
	((and (listp (cadr S)) (not (listp (caddr S)) ))
		(eq (lisp-eval AL (second (cadr S))) (lisp-eval AL (caddr S))))
	
        ((and (not (listp (cadr S)) ) (listp (caddr S)))
		(eq (lisp-eval AL (cadr S)) (lisp-eval AL (second (caddr S)))))
	(t
		(eq (lisp-eval AL (cadr S)) (lisp-eval AL (caddr S))))))
     ((eq (car S) 'null)
	(null (lisp-eval AL (car (cdr (second S))))))
     ((eq (car S) 'car)
	(if (equal (first (cadr S)) 'list)
		(lisp-eval AL (second (first (cdr S))))
	(car (lisp-eval AL (second (first (cdr S)))))))
     ((eq (car S) 'cdr)
	(if (equal (first (cadr S)) 'list)
		(cdr (lisp-eval AL (cdr (cadr S))))
	(cdr (lisp-eval AL (second (cadr S))))))
     ((eq (car S) 'cons)
	(cond
		((and (not (listp (second S))) (not (listp (third S))))
			(cons (lisp-eval AL (second S)) (lisp-eval AL (third S))))
		((and (not (listp (second S))) (listp (third S)))
			(cons (lisp-eval AL (second S)) (lisp-eval AL (second (second (cdr S))))))
		((and (listp (second S)) (not (listp (third S))))
			(cons (lisp-eval AL (second (cadr S))) (lisp-eval AL (third S))))
		((and (listp (second S)) (listp (third S)))
			(cons (lisp-eval AL (second (cadr S))) (lisp-eval AL (second (second (cdr S))))))))
     ((eq (car S) 'if)
	(if (lisp-eval AL (second S))
		(lisp-eval AL (third S))
	    (lisp-eval AL (fourth S))))
    ((eq (car S) 'numberp) 	
	(cond
	((listp (cadr S))
		(numberp (lisp-eval AL (second (cadr S)))))
	(t (numberp (lisp-eval AL (cadr S))))))	
    ((eq (car S) 'atom)
	(cond
	((atom (lisp-eval AL (cadr S))) 
		t)
	
	((equal (first (cadr S)) 'quote)
		(progn
		(print S)
		(if (consp (lisp-eval AL (second (cadr S))))
			nil
		t)))))
     ((eq (car S) 'symbolp)
	(symbolp (lisp-eval AL (second (cadr S)))))
     ((eq (car S) '>)
	(cond
	((and (listp (cadr S)) (listp (caddr S)))
		(> (lisp-eval AL (second (cadr S))) (lisp-eval AL (second (caddr S)))))
	((and (listp (cadr S)) (not (listp (caddr S))))
		(> (lisp-eval AL (second (cadr S))) (lisp-eval AL (cddr S))))
	((and (not (listp (cadr S))) (not (listp (caddr S))))
		(> (lisp-eval AL (cadr S)) (lisp-eval AL (caddr S))))
	(t 
		(> (lisp-eval AL (cadr S)) (lisp-eval AL (second (caddr S)))))))
     ((eq (car S) '<)
	(cond
	((and (listp (cadr S)) (listp (caddr S)))

		(< (lisp-eval AL (second (cadr S))) (lisp-eval AL (second (caddr S)))))
	((and (listp (cadr S)) (not (listp (caddr S))))
		(< (lisp-eval AL (second (cadr S))) (lisp-eval AL (cddr S))))
	((and (not (listp (cadr S))) (not (listp (caddr S))))
		(< (lisp-eval AL (cadr S)) (lisp-eval AL (caddr S))))
	(t 
		(< (lisp-eval AL (cadr S)) (lisp-eval AL (second (caddr S)))))))
     ((eq (car S) 'add)
         (reduce3 #'+ 0 (map1 (lisp-eval1 AL) (cdr S))))
     ((eq (car S) 'sub)
         (reduce3 #'- 0 (map1 (lisp-eval1 AL) (cdr S))))
     ((eq (car S) 'mul)
         (reduce3 #'* 1 (map1 (lisp-eval1 AL) (cdr S))))
     ((eq (car S) 'div)
         (reduce3 #'/ 1 (map1 (lisp-eval1 AL) (cdr S))))
     ((eq (car S) 'if)
	 (if (lisp-eval AL (second S))
		 (lisp-eval AL (third S))
	 (lisp-eval AL (fourth S))))
     ((eq (car S) 'funcall)
	 (progn
		(lisp-eval AL (cons (cadadr S) (cddr S)))))
	 
     ((listp (car S))
	(if (eq (caar S) 'lambda) 
	
	  (lisp-eval AL (third (car S)))))	
     (t  S)))

(defun setter (L AL)
	(cond
	 ((null L)
		AL)
	(t 
	    (progn
	        (setq memory (ALset AL (caar L) (lisp-eval AL (second (car L)))))
		(setter (cdr L) memory)
			
	))))

(defun lisp-eval1 (AL)
  (lambda (S) (lisp-eval AL S)))
	
(defun lisp-run ()
  (let ((memory nil)
        (form nil))
    (loop
       (princ "> ")
       (setq form (read))

       (if (listp form)
           (cond
             ((eq (car form) 'setq)
                (let ((varname (cadr form))
                      (value (caddr form)))
                  (setq memory
                        (ALset memory
                               varname
                               (lisp-eval memory value)))))

             ((eq (car form) 'defun)
                (let ((funcname (cadr form))
                      (params (caddr form))
                      (body (cadddr form)))
                  (setq memory 
                        (ALset memory
                               funcname
                               (list 'lambda params body)))))

             ((eq (car form) 'exit)
                (return 'bye))
             
	     ((listp (car form))
		(if (eq (caar form) 'lambda)
        	(progn
        	   (setq List (zipper (cadar form) (cdr form)))
		   ;;(setq memory (setter List memory))
		   ;;(setq memory (ALset memory (caar List) (lisp-eval memory (second (car List)))))
		   (setq memory (setter List memory))
		   (prin1 (lisp-eval memory form)))))
	     
	     (t
                (prin1 (lisp-eval memory form))
                (terpri)))

           (progn
             (prin1 (lisp-eval memory form))
             (terpri))))))
