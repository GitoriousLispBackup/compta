;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                           ;;; 
;;; File : 1-2.lisp                                                                                           ;;; 
;;; Modify a transaction's date and sort them by chronological order                                          ;;;
;;;                                                                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package #:compta-model)

; Redefine the transaction class to make date slot accessible in write mode
(defclass transaction (name-mixin)
  ((%date :initarg :date :initform (make-instance 'date) :accessor date)
   (%creator :initarg :creator :initform *operator* :reader creator)
   (%debits :initform '() :initarg :debits :accessor debits)
   (%credits :initform '() :initarg :credits :accessor credits)))

(in-package #:compta-gui)

; If we just want to use that name when the date is editing
(define-presentation-type year () :inherit-from 'integer)
(define-presentation-type month () :inherit-from 'integer)
(define-presentation-type day () :inherit-from 'integer)
(define-presentation-type hour () :inherit-from 'integer)
(define-presentation-type minute () :inherit-from 'integer)

; the object pass to the date, when clicking on it, after his edition, it will be immediately update
(defclass date-changer ()
  ((%transaction :initarg :transaction :accessor transaction)))

; Redefinition of the function which display the transaction list in the transaction view
; A date-chager object is pass to the written date
(defun display-oneline-transaction-summary (pane transaction modifiablep)
  (let ((object (make-instance 'date-changer :transaction transaction))
	(type 'date-changer)) 
    (with-output-as-presentation (pane object type)
      (format pane "~a "
	      (iso-date-string (date transaction)))))
  (let ((object (if modifiablep
		    (make-instance 'name-changer :object transaction)
		    transaction))
	(type (if modifiablep 'name-changer 'transaction))) 
    (with-output-as-presentation (pane object type)
      (format pane "~a~%"
	      (name transaction)))))

; Sort function call by the sorting date function (return true if date1 is before date2)
(defun date-lessp (date1 date2)
  (cond
    ((< (year date1) (year date2)) T)
    ((< (month date1) (month date2)) T)
    ((< (day date1) (day date2)) T)
    ((< (hour date1) (hour date2)) T)
    ((< (minute date1) (minute date2)) T)
    (T NIL)))

;;;;;;;;;;;;;;;;;;;;;;; Command associated to this extension ;;;;;;;;;;;;;;;;;;;;;;;;;

; Command to change the date manualy
(define-compta-command (com-change-date-manualy :name t)
    ((changer 'date-changer :gesture :menu))
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (setf (date (transaction changer))
	  (make-instance 'date
			 :year (accept 'year :default year :prompt "Please enter the transaction date year")
			 :month (accept 'month :default month :prompt "Please enter the transaction date month")
			 :day (accept 'day :default day :prompt "Please enter the transaction date day")
			 :hour (accept 'hour :default hour :prompt "Please enter the transaction date hour")
			 :minute (accept 'minute :default min :prompt "Please enter the transaction date minute")))))

; Command to change the transaction order
(define-compta-command (com-order-transaction-by-date :name t)
    ()
  (setf (transactions (current-organization *application-frame*)) 
	(sort (transactions (current-organization *application-frame*)) #'date-lessp :key #'date)))