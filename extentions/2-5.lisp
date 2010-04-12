;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                           ;;; 
;;; File : 2-5.lisp                                                                                           ;;; 
;;; Show Income Statement (compte de resultat) for a given period                                             ;;;
;;;                                                                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                           ;;;
;;; Features :                                                                                                ;;;
;;;         - Default period of 1 year from the starting date suggested to the user                           ;;;
;;;         - Default values if not in the config file or the config file does not exist                      ;;;
;;;                                                                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:compta-gui)

(define-presentation-type year () :inherit-from 'integer)
(define-presentation-type month () :inherit-from 'integer)
(define-presentation-type day () :inherit-from 'integer)
(define-presentation-type hour () :inherit-from 'integer)
(define-presentation-type minute () :inherit-from 'integer)

(defclass date-changer ()
  ((%transaction :initarg :transaction :accessor transaction)))

;; Creates a view to display the Income Statement
(defclass income-statement-view (view)
  ((%date-from :initarg :date-from :reader date-from)
   (%date-to :initarg :date-to :reader date-to)))

;; Add a command "Show Income Statement"
(define-compta-command (com-show-income-statement :name t)
    ()
  (let* ((date-now (make-instance 'date))
	(date-from (make-instance 'date
			     :year (accept 'year :prompt "Begin period year " :default (- (year date-now) 1))
			     :month (accept 'month :prompt "Begin period month " :default (month date-now))
			     :day (accept 'day :prompt "Begin period day " :default (day date-now))
			     :hour 0
			     :minute 0))
	(date-to (make-instance 'date
			     :year (accept 'year :prompt "End period year " :default (+ (year date-from) 1))
			     :month (accept 'month :prompt "End period month " :default (month date-from))
			     :day (accept 'day :prompt "End period day " :default (day date-from))
			     :hour 0
			     :minute 0)))
    (setf (stream-default-view (find-pane-named *application-frame* 'main))
	  (make-instance 'income-statement-view :date-from date-from :date-to date-to))))

;; Compute what to display
(defmethod display-main-with-view (frame pane (view income-statement-view))
  (declare (ignore frame))
  (format pane "Income statement for period ~a to ~a~%~%" (iso-date-string (date-from view)) (iso-date-string (date-to view)))
  (let ((debits '())
	(credits '()))
    (loop for transaction in (transactions (current-organization *application-frame*))
       do (if (and (/= 1 (compare-date (date-from view) (date transaction)))
		   (/= -1 (compare-date (date-to view) (date transaction))))
	      (progn (setf debits (append debits (debits transaction)))
		     (setf credits (append credits (credits transaction))))))
    (display-entry-income-statement pane "Revenues"
			 (lambda (entry) (push entry credits)) credits)
    (display-entry-income-statement pane "Expenses"
			 (lambda (entry) (push entry debits)) debits)
    (format pane "Net income : ~+d" (- (apply #'+ (mapcar #'amount credits)) (apply #'+ (mapcar #'amount debits))))))

;; Same than display-entry-adder but without the adder
(defun display-entry-income-statement (pane area-name push-entry entries)
  (let ((medium (sheet-medium pane)))
    (flet ((show-entry (entry)
	     (with-text-family
		 (medium :fixed)
	       (with-output-as-presentation (pane (amount entry) 'amount)
		 (format-amount pane (amount entry) "~10d.~2,'0d        ")))
	     (with-output-as-presentation (pane (account entry) 'account)
	       (format pane "~a~%" (name (account entry))))))
      (format pane "~a :~%" area-name)
      (loop for entry in (reverse entries)
	    do (show-entry entry)))))
    
;; Compare date1 and date2, return 1 if date1 > date2, -1 if date1 < date2 and 0 if date1 = date2
(defun compare-date (date1 date2)
  (let ((encoded-date1 (encode-universal-time 0 (minute date1) (hour date1) (day date1) (month date1) (year date1) 0))
	(encoded-date2 (encode-universal-time 0 (minute date2) (hour date2) (day date2) (month date2) (year date2) 0)))
    (cond ((> encoded-date1 encoded-date2) 1)
	  ((< encoded-date1 encoded-date2) -1)
	  ((t 0)))))
