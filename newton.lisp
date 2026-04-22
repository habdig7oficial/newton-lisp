
(defun derivate (f)
  (cond
    ((atom f) 0)
    ((eq (car f) 'expt) `(* ,(third f) (expt ,(second f) (- ,(third f) 1))))
    ((eq (car f) 'sin) `(cos ,(car (cdr f))))
    ((eq (car f) 'cos) `(- (sin ,(car (cdr f)))))
    ((eq (car f) 'exp) `(exp ,(car (cdr f))))
    ((eq (car f) 'log) `(/ 1 ,(car (cdr f))))
    (t f)))

(defun derivate-all* (fx)
  (print fx)
  (cond
    ((null fx) nil)
    ((atom fx) fx)
    (t (let ((res (derivate (car fx))))
	 (cons (cond
		 ((eq res 0) fx)
		 (t res))
	       (derivate-all* (cdr fx)))))))


(defun derivate-all (fx)
  (cond
    ((null fx) nil)
    (t
       (let ((rest (derivate-all (cdr fx))))
	 (cond
	   ((null rest) (derivate-all* fx))
	   (t (cons (derivate-all* fx) rest)))))))
  


(defun newton (fx p epsilon))


;(print (eval (subst 3 'x (derivate '(expt 'x 3)))))
;(print (derivate '(expt 'x 3)))
;(print (eval (subst 2 'x (derivate '(sin 'x)))))
;(print (derivate 2))

;(let ((a '(- (+ (exp x) (expt 2 (- x)) (* 2 (cos x))) 6))))

(let ((b '(+ (log (- x 1)) (cos (- x 1)))))
  (print (derivate-all '(expt (cos x) 3)))
  (print (derivate-all '(1))))





	  
