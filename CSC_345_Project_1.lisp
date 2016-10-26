;;;; Name: Edward (Ted) Segal
;;;; Date: 10/24/2016
;;;; Project: CSC_345_Project_1 : Symbolic_Integration

;;; INTEGRATION FUNCTIONS
(defun integrate (F V &optional lo hi)
  "Returns integral of an argument Function.
      Attempts find def-integral, otherwise finds indef-integral"
  (def-integral (indef-integral F V) lo hi))

(defun indef-integral (F V)
  "Returns the indefinite integral of an argument Function"
  (labels ((indef-integral-helper (F V)
				  (cond ((number-p F) (make-product F V))
					((variable-p F)(make-product 1/2 (make-power F 2)))
					((variable-p F) (integrate (make-power F 1) V))
					((sum-p)(make-sum (integrate (sum-operand-1 F) V)
							  (integrate (sum-operand-2 F) V)))
					((difference-p F) (make-difference (integrate (difference-operand-1 F) V)
									   (integrate (difference-operand-2 F) V)))
					((and (power-p F)
					      (not (= (power-operand-2 F) -1 )))
					 (make-quotient (make-power V (make-sum (power-operand-2 F) 1 ))
							(make-sum (power-operand-2 F) 1)))
					((and (power-p F)
					      (equal (power-operand-2 F) -1) (make-log (power-operand-1 F)))))))
	   (cond ((not (variable-p V)) nil)
		  ((nested-negative-p F) (indef-integral-helper (make-negative-simplified F) V))
		  (t (indef-integral-helper F V)))))

(defun def-integral (F V lo hi)
  "Returns the definite integral of the function"
  (if (not (and (number-p lo)
		(number-p hi)))
      F
    (eval (make-difference (my-replace V hi F)
			   (my-replace V lo F)))))

(defun my-replace (e1 e2 L)
  "Replace e1 with e2 to help with def-integral"
  (labels ((my-replace-helper (e1 e2 L)
			      (cond ((endp L) nil)
				    ((equal (first L) e1) (cons e2 (my-replace e1 e2 (rest L))))
				    ((listp (first L)) (cons (my-replace e1 e2 (first L))
							     (my-replace e1 e2 (rest L))))
				    (t (cons (first L) (my-replace e1 e2 (rest L)))))))
    (cond ((variable-p L) (first (my-replace-helper e1 e2 (list L))))
	  (t (my-replace-helper e1 e2 L)))))

;;; SYMBOLS
(defconstant variable-symbols '(U V W X Y Z))
(defconstant negative-symbol '-)
(defconstant sum-symbol '+)
(defconstant difference-symbol '-)
(defconstant product-symbol '*)
(defconstant power-symbol 'expt)
(defconstant log-symbol 'log)

;;--------------------------------------------


;; SELECTORS -- OPERATORS
(defun negative-operator (F) (first F))
(defun sum-operator (F) (first F))
(defun difference-operator (F) (first F))
(defun product-operator (F) (first))
(defun quotient-operator (F) (first))
(defun power-operator (F) (first))

;; SELECTORS -- OPERANDS
(defun negative-operand (F) (second F))
(defun sum-operand-1 (F) (second F))
(defun sum-operand-2 (F) (third))
(defun difference-operand-1 (F) (second F))
(defun difference-operand-2 (F) (third))
(defun product-operand-1 (F) (second F))
(defun product-operand-2 (F) (third F))
(defun quotient-operand-1 (F) (second F))
(defun quotient-operand-2 (F) (third F))
(defun power-operand-1 (F) (second F))
(defun power-operand-2 (F) (third F))

;;--------------------------------------------

;; PREDICATES
(defun variable-p (F)
  "t if F is a variable"
  (member F variable-symbols))


(defun number-p (F)
  "t if F is a number"
  (numberp F))

(defun negative-p (F)
  "t if F is negative"
  (cond ((and (number-p F) (< F 0)) t)                            ;; if input is a number less then 0, t
	((number-p F) nil)                                        ;; not if anything else
	((variable-p F) nil)
	((difference-p F) nil)
	((and (equal (negative-operator F) negative-symbol)       ;; if the operator is the negative-symbol
	      (not (equal (negative-operand F) negative-symbol))) ;; but not nested-negative
	 t)))

(defun nested-negative-p (F)
  "t if F has more than 1 negative-symbol"
  (labels ((nested-negative-p-helper (F L)
				     (cond ((endp F) (= (length L) 1))
				     ((equal (first F) negative-symbol) (nested-negative-p-helper (rest F) L))    ;; if First is '-' ...
				     (t (nested-negative-p-helper (rest F) (cons (first F) L))))))                ;; return t and continue to cdr
    (cond ((number-p F) nil)
	((variable-p F) nil)
	((negative-p F) nil)
	((difference-p F) nil)
	((not (listp F)) nil)
	(t (nested-negative-p-helper F '())))))

(defun sum-p (F)
  "t if F is a sum func"
  (cond ((number-p F) nil)
	((variable-p F) nil)
	((and (equal (sum-operator F) sum-symbol) ;; if the operator is the sum symbol
	      (sum-operand-1 F)                   ;; the first operand will be sum-operand-1
	      (sum-operand-2 F))                  ;; the second operand will be sum-operand-2
	 t)))

(defun difference-p (F)
  "t if difference func"
  (cond ((number-p F) nil)    ;; logic similar to sum-p
	((variable-p F) nil)
	((and (equal (difference-operator F) difference-symbol)
	     (not (equal (difference-operand-1 F) difference-symbol))
	     (difference-operand-2 F))
	 t)))

(defun power-p (F)
  "t if expt funct"
  (cond ((number-p F) nil)
	((variable-p F) nil
	 ((and (equal (power-operator F) power-symbol)) ;; if operator is the power-symbol
	  (variable-p (poweroperand-1 F))               ;; the next 2 args are operands
	  (number-p (power-operand-2 F)))
	 t)))

;;--------------------------------------------

;; CONSTRUCTORS
(defun make-variable (V)
  "Trivial but included for completeness of the abstraction"
  V)

(defun make-sum (F G)
  (cond ((eq 0 F) G)                         ; 0+G = G
	((eq 0 G) F)                          ; F+0 = F
	((and(numberp F) (numberp G)) (+ F G)) ; if they are numbers, add them
	( t (list sum-symbol F G))))

(defun make-negative (F)
  "Constructs negatative of arg"
  (labels ((make-negative-helper (F)
				 (cond ((number-p F) (* -1 F))
				       ((negative-p F) (negative-operand F))
				       (t (list negative-symbol F)))))
    (cond ((nested-negative-p F) (make-negative-helper (make-negative-simplified F)))
	  (t (make-negative-helper)))))

(defun make-negative-simplified (F)
  "simplify negative"
  (labels ((make-negative-simplified-helper (F)
					    (cond ((equal (mod (length F) 2) 0) (list negative-symbol (first (last F))))
						  (t (first (last F))))))
    (cond ((nested-negative-p F) (make-negative-simplified-helper F))
	  ((negative-p F) F))))

(defun make-difference (F G)
  "Constructs difference of F and G"
  (cond ((equal F 0) G)   ;; if F or G is 0, then the difference is the other
	((equal G 0) F)
	((equal F G) 0)   ;; X - X = 0
	((and (number-p F) (number-p G)) (- F G))
	((equal F (make-negative G)) (make-sum F (make-negative G))) ;; X - (-X) = X + (-(-X))
	(t (list difference-symbol F G))))

(defun make-product (F G)
  "Constructs product of F and G"
  (cond ((or (equal F 0)      ;; X * 0 = 0
	     (equal G 0))
	 0)
	((equal F 1) G)  ;; X * 1 = X
	((equal G 1) F)
	((equal F -1) (make-negative G))  ;; X * -1 = -X
	((equal G -1) (make-negative F))
	((and (negative-p F) (negative-p G)) (make-product (make-negative F) (make-negative G))) ;; -1 * -1 = 1
	((and (number-p F) (number-p G)) (* F G)) ;; not a special case
	(t (list product-symbol F G))))

(defun make-quotient (F G)
  "Constructs quotient of F and G"
  (cond ((equal F 0) 0)   ;; 0 / X = 0
	((equal G 0) nil) ;; X / 0 = Undefined
	((equal F G) 1)   ;; X / X = 1
	((and (number-p F) (number-p G)) (/ F G))  ;; not a special case
	(t (list quotient-symbol F G))))

(defun make-log (V)
  "Constructs log of V"
  (cond ((variable-p V) (list log-symbol V))))

(defun make-pwer (V N)
  "Constructs V to the Nth power"
  (cond ((and (variable-p V) (number-p N)) (list power-symbol V N))))

;;--------------------------------------------

;;TESTS

(defun t1 () (integrate '1 'x))
(defun t2 () (integrate '1 'y 1 4))
(defun t3 () (integrate 'z 'z))
