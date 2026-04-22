
(defun derivate (f)
  (cond
    ((atom f) 0)
    ((eq (car f) 'expt) `(* ,(third f) (expt ,(second f) (- ,(third f) 1))))
    ((eq (car f) 'sin) `(cos ,(car (cdr f))))
    ((eq (car f) 'cos) `(- (sin ,(car (cdr f)))))
    ((eq (car f) 'exp) `(exp ,(car (cdr f))))
    ((eq (car f) 'log) `(/ 1 ,(car (cdr f))))))
     
(defun derivate-all* (fx)
  (cond
    ((null fx) nil)
    (t
       (print (car fx))
       
       (derivate-all* (cdr fx)))))
  


(defun newton (fx p epsilon))


;(print (eval (subst 3 'x (derivate '(expt 'x 3)))))
(print (derivate '(expt 'x 3)))
(print (eval (subst 2 'x (derivate '(sin 'x)))))
(print (derivate 2))

;(let ((a '(- (+ (exp x) (expt 2 (- x)) (* 2 (cos x))) 6))))

;(let ((b '(+ (log (- x 1)) (cos (- x 1)))))
;  (derivate-all* b))





	  
