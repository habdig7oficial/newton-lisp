
(defun derivate (f)
  (cond
    ((atom f) f)
    ((eq (car f) 'expt) `(* ,(third f) (expt ,(second f) (- ,(third f) 1))))
    ((eq (car f) 'sin) `(cos ,(car (cdr f))))
    ((eq (car f) 'cos) `(- (sin ,(car (cdr f)))))
    ((eq (car f) 'exp) `(exp ,(car (cdr f))))
    ((eq (car f) 'log) `(/ 1 ,(car (cdr f))))
    (t f)))


(defun derivate-all (fx)
  ;(print fx)
  (cond
    ((null fx) nil)
    ((atom fx) fx)
    (t (cons (derivate (derivate-all (car fx))) (derivate-all (cdr fx))))))
  
(defun rel-err (value aprox)
  (/ (abs (- value aprox)) value))

(defun newton (fx p epsilon &optional (var 'x))
  (let* (
	 (fdx (derivate-all fx))
	 (a   (subst p var fx))
	 (b   (subst p var fdx))
	 )
    (format t "f(x)  = ~a~%f'(x) = ~a ~%" fx fdx)
    a))


(defparameter *episilon* 0.00001)

(let ((b '(+ (log (- x 1)) (cos (- x 1)))))
  (print (newton b 1.3 *episilon*))
  (print (rel-err 0.1 0.15)))














;(print (eval (subst 3 'x (derivate '(expt 'x 3)))))
;(print (derivate '(expt 'x 3)))
;(print (eval (subst 2 'x (derivate '(sin 'x)))))
;(print (derivate 2))

;(let ((a '(- (+ (exp x) (expt 2 (- x)) (* 2 (cos x))) 6))))
	  
