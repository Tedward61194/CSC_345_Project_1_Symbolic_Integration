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
  (cond ((number-p F) (make-product FV))
	((variable-p F)(make-product 1/2 (make-power F 2)))
	;90% of work
	))
(defun def-integral (F lo hi)
  (if () ; no values supplied for lo and hi
      F     ; return the indefinate integral
    ......  ; else return the definite integral
    ))


;;(E)BNF
;;=============================================

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
  (cond ((and (number-p F) (< F 0)) t)
	((number-p F) nil)
	((variable-p F) nil)
	((difference-p F) nil)
	((and (equal (negative-operator F) negative-symbol)
	      (not (equal (negative-operand F) negative-symbol))) t)))

;;--------------------------------------------

;; CONSTRUCTORS
(defun make-variable (V) V)

(defun make-sum (F G)
  (cond ((eq 0 F) G)                         ; 0+G = G
	((eq 0G) F)                          ; F+0 = F
	((and(numberp F)(numberp G))(+ F G)) ; if they are numbers, add them
	( t (list sum-symbol FG))))
