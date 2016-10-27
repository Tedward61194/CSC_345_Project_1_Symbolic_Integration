;;; NAME: Teddy (Edward) Segal
;;; PROJECT: integrate.lisp
;;; DATE: 10/27/16


;;;========================================================
;;; INTEGRATE

(defun integrate (F V &optional lo hi)
  "Returns integral of function"
  (def-integral (indef-integral F V) V lo hi))

(defun indef-integral(F V)
  "Returns indefinite integral of function"
  (labels ((indef-integral-aux (F V)
	     (cond ((number-p F) (make-product F V))
		   ((negative-p F) (make-negative (integrate (make-negative F) V)))
		   ((variable-p F) (integrate (make-power F 1) V))
		   ((sum-p F)(make-sum (integrate (sum-operand-1 F) V)
				       (integrate (sum-operand-2 F) V)))
		   ((difference-p F) (make-difference (integrate (difference-operand-1 F) V)
						      (integrate (difference-operand-2 F) V)))
		   ((and (power-p F)
			 (not (= (power-operand-2 F) -1))) (make-quotient (make-power V (make-sum (power-operand-2 F) 1))
									  (make-sum (power-operand-2 F) 1)))
		   ((and (power-p F)
			 (equal (power-operand-2 F) -1) (make-log (power-operand-1 F)))))))
    (cond ((not (variable-p V)) nil)
	  ((nested-negative-p F) (indef-integral-aux (make-reduced-negative F) V))
	  (t (indef-integral-aux F V)))))

(defun def-integral(F V lo hi)
  "Returns definite integral of function"
  (if (not (and (number-p lo)
		(number-p hi)))
      F
    (eval (make-difference (my-replace V hi F)
			   (my-replace V lo F)))))

(defun my-replace(e1 e2 L)
  "Replace e1 with e2 to help with def-integral"
  (labels ((my-replace-aux (e1 e2 L)
	     (cond ((endp L) nil)
		   ((equal (first L) e1) (cons e2 (my-replace e1 e2 (rest L))))
		   ((listp (first L)) (cons (my-replace e1 e2 (first L))
					    (my-replace e1 e2 (rest L))))
		   (t (cons (first L) (my-replace e1 e2 (rest L)))))))
    (cond ((variable-p L) (first (my-replace-aux e1 e2 (list L))))
	  (t (my-replace-aux e1 e2 L)))))

;;;========================================================
;;;SYMBOLS
(defconstant variable-symbols '(U V W X Y Z))
(defconstant negative-symbol '-)
(defconstant sum-symbol '+)
(defconstant difference-symbol '-)
(defconstant product-symbol '*)
(defconstant quotient-symbol '/)
(defconstant power-symbol 'expt)
(defconstant log-symbol 'log)

;;;========================================================
;;;SELECTORS -- OPERATORS
(defun negative-operator (F) (first F))
(defun sum-operator (F) (first F))
(defun difference-operator (F) (first F))
(defun product-operator (F) (first F))
(defun quotient-operator (F) (first F))
(defun power-operator (F) (first F))

;;;SELECTORS -- OPERANDS

(defun negative-operand (F) (second F))
(defun sum-operand-1 (F) (second F))
(defun sum-operand-2 (F) (third F))
(defun difference-operand-1 (F) (second F))
(defun difference-operand-2 (F) (third F))
(defun product-operand-1 (F) (second F))
(defun product-operand-2 (F) (third F))
(defun quotient-operand-1 (F) (second F))
(defun quotient-operand-2 (F) (third F))
(defun power-operand-1 (F) (second F))
(defun power-operand-2 (F) (third F))

;;;========================================================
;;;PREDICATES

(defun variable-p (F)
  "T if F is a variable"
  (member F variable-symbols))

(defun number-p (F)
  "T if F is a number"
  (numberp F))

(defun negative-p (F)
  "T if F is negative"
  (cond ((and (number-p F) (< F 0)) t)                       ;;if input is a number less than 0,
	((number-p F) nil)
	((variable-p F) nil)
	((difference-p F) nil)
	((and (equal (negative-operator F) negative-symbol) ;;if the operator is the negative-symbol
	      (not (equal (negative-operand F) negative-symbol))) t))) ;;but not nested-negative

(defun nested-negative-p (F)
  "T if F has more than 1 negative operator"
  (labels ((nested-negative-p-aux (F L)
	     (cond ((endp F) (= (length L) 1))
		   ((equal (first F) negative-symbol) (nested-negative-p-aux (rest F) L)) ;; if the First is '-' ...
		   (t (nested-negative-p-aux (rest F) (cons (first F) L))))))  ;; return t and cdr
    (cond ((number-p F) nil)
	  ((variable-p F) nil)
	  ((negative-p F) nil)
	  ((difference-p F) nil)
	  ((not (listp F)) nil)
	  (t (nested-negative-p-aux F '())))))

(defun sum-p (F)
  "T if F is a sum function"
  (cond ((number-p F) nil)
	((variable-p F) nil)
	((and (equal (sum-operator F) sum-symbol)  ;; if the operator is the sum symbol
	      (sum-operand-1 F)                    ;; the first operand will be sum-operand-1
	      (sum-operand-2 F)) t)))              ;; the second operand will be sum-operand 2

(defun difference-p (F)
  "T if F is a difference function"
  (cond ((number-p F) nil)  ;; logic similar to sum-p
	((variable-p F) nil)
	((and (equal (difference-operator F) difference-symbol)
	      (not (equal (difference-operand-1 F) difference-symbol))
	      (difference-operand-2 F)) t)))

(defun power-p (F)
  "T if F is an exponent function"
  (cond ((number-p F) nil)
	((variable-p F) nil)
	((and (equal (power-operator F) power-symbol)  ;; if operator is the power-symbol
	      (variable-p (power-operand-1 F))         ;; the next 2 args are operands
	      (number-p (power-operand-2 F))) t)))

;;;========================================================
;;;CONSTRUCTORS

(defun make-variable (V)
  "Function seems 'trivial,' but is included for the completeness fo the abstraction"
  V)

(defun make-negative (F)
  "Makes the negative of arg"
  (labels ((make-negative-aux (F)
	     (cond ((number-p F) (* -1 F))
		   ((negative-p F) (negative-operand F))
		   (t (list negative-symbol F)))))
    (cond ((nested-negative-p F) (make-negative-aux (make-negative-simplified F)))
	  (t (make-negative-aux f)))))

(defun make-negative-simplified (F)
  "Simplify negative"
  (labels ((make-negative-simplified-aux (F)
	     (cond ((equal (mod (length F) 2) 0) (list negative-symbol (first (last F))))
		   (t (first (last F))))))
    (cond ((nested-negative-p F) (make-negative-simplified-aux F))
	  ((negative-p F) F))))


(defun make-sum (F G)
  "Makes sum of args"
  (cond ((equal F 0) G)                           ;; G + 0 = G
	((equal G 0) F)                           ;; F + 0 = F
	((equal F (make-negative G)) 0)           ;; X + (-X) = 0
	((and (number-p F) (number-p G)) (+ F G)) ;; if they are numbers, add them
	(t (list sum-symbol F G))))

(defun make-difference (F G)
  "Makes difference of args"
  (cond ((equal F 0) G)
	((equal G 0) F)
	((equal F G) 0)
	((and (number-p F) (number-p G)) (- F G))
	((equal F (make-negative G)) (make-sum F (make-negative G))) ;; X - (-X) = X + (-(-X))
	(t (list difference-symbol F G))))

(defun make-product(F G)
  "Makes product of args"
  (cond ((or (equal F 0)        ;; X * 1 = X
	     (equal G 0)) 0)
	((equal F 1) G)
	((equal G 1) F)
	((equal F -1) (make-negative G)) ;; X * -1 = -X
	((equal G -1) (make-negative F))
	((and (negative-p F)
	      (negative-p G)) (make-product (make-negative F) (make-negative G))) ;; -1 * -1 = 1
	((and (number-p F) (number-p G)) (* F G))  ;; not a special case
	(t (list product-symbol F G))))

(defun make-quotient (F G)
  "Makes quotient of args"
  (cond ((equal F 0) 0)
	((equal G 0) nil)
	((equal F G) 1)
	((and (number-p F) (number-p G)) (/ F G))  ;; not a special case
	(t (list quotient-symbol F G))))

(defun make-log (V)
  "Makes log of arg"
  (cond ((variable-p V) (list log-symbol V))))


(defun make-power (V N)
  "Makes V to Nth power"
  (cond ((and (variable-p V)
	      (number-p N)) (list power-symbol V N))))

;;;========================================================
;;;TESTS

(defun t1 () (integrate '1 'x))
(defun t2 () (integrate '1 'y 1 4))
(defun t3 () (integrate 'z 'z))
(defun t4 () (integrate '(+ x 0) 'x))
(defun t5 () (integrate '(- x) 'x 1 3))
(defun t6 () (integrate '(- - x) 'x) 1 4)
(defun t7 () (integrate '(- - - x) 'x))
(defun t8 () (integrate '(+ x (- x)) 'x))
(defun t9 () (integrate '(- (+ (- - x) x)) 'x 1 4))
(defun t10 () (integrate '(+ (+ (- - x) (+ x 3)) 2) 'x 2 6))
(defun t11 () (integrate '(- x (expt x 3)) 'x))
(defun t12 () (integrate '(- x (expt x 3)) 'x 2 5))
(defun t13 () (integrate '(+ (+ x (- - - x)) (expt x 3)) 'x))
(defun t14 () (integrate '(+ (- x (-x)) (expt x 3)) 'x 2 3))
(defun t15 () (integrate '(expt x -1) 'x))
(defun t16 () (integrate '(expt x -1) 'x 3 45))
(defun t17 () (integrate '(+ (+ x (-5 x)) (expt x -1)) 'x))
(defun t18 () (integrate '(+ (+ x (-5 x)) (expt x -1)) 'x 5 217))
