;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                           ;;; 
;;; File : 2-5.lisp                                                                                           ;;; 
;;; Show results for a given period                                                                           ;;;
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

;; Creates a view to display the Result
(defclass result-view (view)
  ((%date-from :initarg :date-from :reader date-from)
   (%date-to :initarg :date-to :reader date-to)))

;; Add a command "Show Result"
(define-compta-command (com-show-result :name t)
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
	  (make-instance 'result-view :date-from date-from :date-to date-to))))

;; Compute what to display
(defmethod display-main-with-view (frame pane (view result-view))
  (declare (ignore frame))
  (format pane "Result for period ~a to ~a~%" (iso-date-string (date-from view)) (iso-date-string (date-to view)))
  (let ((debits '())
	(credits '()))
    (loop for transaction in (transactions (current-organization *application-frame*))
       do (if (and (/= 1 (compare-date (date-from view) (date transaction)))
		   (/= -1 (compare-date (date-to view) (date transaction))))
	      (progn (setf debits (append debits (debits transaction)))
		     (setf credits (append credits (credits transaction))))))
    (display-entry-adder pane "Debits"
			 (lambda (entry) (push entry debits)) debits)
    (display-entry-adder pane "Credits"
			 (lambda (entry) (push entry credits)) credits)))
    
;; Compare date1 and date2, return 1 if date1 > date2, -1 if date1 < date2 and 0 if date1 = date2
(defun compare-date (date1 date2)
  (let ((encoded-date1 (encode-universal-time 0 (minute date1) (hour date1) (day date1) (month date1) (year date1) 0))
	(encoded-date2 (encode-universal-time 0 (minute date2) (hour date2) (day date2) (month date2) (year date2) 0)))
    (cond ((> encoded-date1 encoded-date2) 1)
	  ((< encoded-date1 encoded-date2) -1)
	  ((t 0)))))
