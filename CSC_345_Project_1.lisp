(defun integrate (F V &optional lo hi)
  (def-integral (indef-integral F V) lo hi))

(defun indef-integral (F V)
  (cond ((number-p F) (make-product FV))
	((variable-p F)(make-product 1/2 (make-power F 2)))
	;90% of work
	))
(defun def-integral (F lo hi)
  (if () ; no values supplied for lo and hi
      F     ; return the indefinate integral
    ......  ; else return the definite integral
    ))

;;--------------------------------------------

;;; SYMBOLS
(defconstant variable-symbols '(U V W X Y Z))
(defconstant negative-symbol '-)

;;--------------------------------------------


;;SELECTORS -- OPERATORS
(defun negative-operator (F) (first F))
(defun sum-operator (F) (first F))

;;SELECTORS -- OPERANDS
(defun negative-operand (F) (second F))
(defun sum-operand-1 (F) (second F))
(defun sum-operand-2 (F) (third))

;;--------------------------------------------

;;PREDICATES
(defun variable-p (F)
  (member F variable-symbols))

;;--------------------------------------------

;;CONSTRUCTORS
(defun make-variable (V) V)

(defun make-sum (F G)
  (cond ((eq 0 F) G)                         ; 0+G = G
	((eq 0G) F)                          ; F+0 = F
	((and(numberp F)(numberp G))(+ F G)) ; if they are numbers, add them
	( t (list sum-symbol FG))))
