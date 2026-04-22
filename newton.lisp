
(defun derivate (f x)
  (cond
    ((null f) 0)
    ;((eq (car f) 'expt) `(* ,(third f) (expt ,x (- ,(third f) 1))))
    ((eq f 'sin) `(cos ,x))
    ((eq f 'cos) `(- (sin ,x)))
    ((eq f 'exp) `(exp ,x))
    ((eq f 'log) `(/ 1 ,x))))
     
(defun derivate-all* (fx)
  (cond
    ((null fx) nil)
    (t
       (print (car fx))
       
       (derivate-all* (cdr fx)))))
  


(defun newton (fx p epsilon))


;(print (eval (subst 3 'x (derivate '(expt nil 3)  'x))))
(print (derivate 'sin 'x))

;(let ((a '(- (+ (exp x) (expt 2 (- x)) (* 2 (cos x))) 6))))

(let ((b '(+ (log (- x 1)) (cos (- x 1)))))
  (derivate-all* b))





	  
