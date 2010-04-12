;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                           ;;; 
;;; File : 3-8.lisp                                                                                           ;;; 
;;; Calendar for easy date picking                                                                            ;;;
;;;                                                                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:compta-gui)

; define the view class to show the view in the main pane
(defclass calendar-view (view)
  ((%year :initarg :year :reader year)
   (%month :initarg :month :reader month)))

; object need to increment the calendar month
(defclass calendar-view-incrementor (calendar-view)
  ())

; object need to decrement the calendar month
(defclass calendar-view-decrementor (calendar-view)
  ())

; table of variable which define days of the week
(defvar *day-of-the-week-string* '((0 . "Mon")(1 . "Tue")
                                   (2 . "Wed")(3 . "Thu")
                                   (4 . "Fri")(5 . "Sat")
                                   (6 . "Sun")))

(defun day-of-the-week-string (day-of-week)
  (cdr (assoc day-of-week  *day-of-the-week-string*)))

(defvar *days-in-month* '((1 . 31)(2 . 28) ( 3 . 31)( 4 . 30)
                          (5 . 31)(6 . 30) ( 7 . 31)( 8 . 31)
                          (9 . 30)(10 . 31)(11 . 30)(12 . 31))
  "alist whose first element is numeric value returned by
decode-universal-time and second is the number of days in that month")

(defvar *month-of-the-year-string* '((1 . "January")(2 . "February")
				     (3 . "March")(4 . "April")
				     (5 . "May")(6 . "June")
				     (7 . "July")(8 . "August")
				     (9 . "September")(10 . "October")
				     (11 . "November")(12 . "December")))

(defun month-of-the-year-string (month-of-year)
  (cdr (assoc month-of-year *month-of-the-year-string*)))

;; In a leap year, the month-length function increments the number of days in February as required 
(defun leap-year-p (year)
  (cond ((and (integerp (/ year 100))
              (integerp (/ year 400)))
         t)
        ((and (not (integerp (/ year 100)))
              (integerp (/ year 4)))
         t)
        (t nil)))

(defun month-length (month year)
  (let ((days (cdr (assoc month *days-in-month*))))
    (when (and (eql month 2)
               (leap-year-p year))
      (incf days))
    days))

; function which build the calendar
(defun calendar-month (month year stream)
  (let ((days-in-month (month-length month year)))
    (multiple-value-bind (sec min hour date month year start-day)
        (decode-universal-time (encode-universal-time 
                                       0 0 0 1 month year))
      (setq start-day (mod (+ start-day 1) 7)); increment of 1 because of the english calendar
      (with-output-as-presentation
	  (stream (make-instance 'calendar-view-decrementor :month month :year year ) 'calendar-view-decrementor)
	(format stream "<<  "))
      (format stream " ~a ~d " (month-of-the-year-string month) year)
      (with-output-as-presentation
	  (stream (make-instance 'calendar-view-incrementor :month month :year year ) 'calendar-view-incrementor)
	(format stream "  >>~%"))
      (formatting-table (stream)
        (formatting-row (stream)
          (dotimes (d 7)
            (formatting-cell (stream :align-x :center)
              (write-string (day-of-the-week-string 
                            (mod (- d 1) 7)) stream))))
        (do ((date 1)
             (first-week t nil))
            ((> date days-in-month))
          (formatting-row (stream)
            (dotimes (d 7)
              (formatting-cell (stream :align-x :right)
                (when (and (<= date days-in-month)
                           (or (not first-week) (>= d start-day)))
		  (with-output-as-presentation
		   (stream (make-instance 'date :year year :month month :day date :hour 00 :minute 00) 'date)
                  (format stream "~a" date))
                  (incf date))))))))))

; function call to display the calendar view in the main pane
(defmethod display-main-with-view (frame pane (view calendar-view))
  (declare (ignore frame))
  (let ((year (year view))
	(month (month view)))
    (calendar-month month year pane)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; COMMAND ASSOCIATED TO THE CALENDAR EXTENTION ;;;;;;;;;;;;;;;;;;;

; The command add to edit the date since to the calendar
(define-compta-command (com-change-date :name t)
    ((changer 'date-changer :gesture :menu))
  (multiple-value-bind (sec min hour day month year)
      (decode-universal-time (get-universal-time))
    (setf (stream-default-view (find-pane-named *application-frame* 'main))
	  (make-instance 'calendar-view :year year :month month))
    (redisplay-frame-pane *application-frame* (find-pane-named *application-frame* 'main)) ;Force to display the calendar before asking for a date
    (setf (date (transaction changer))
	  (accept 'date))))

; command need to call the calendar view manually
(define-compta-command (com-calendar :name t :keystroke (#\C :meta)) ()
  (multiple-value-bind (sec min hour date month year start-day)
      (decode-universal-time (get-universal-time))
    (setf (stream-default-view (find-pane-named *application-frame* 'main))
	  (make-instance 'calendar-view :year year :month month))))

; command which increment the month of 1
; call the calendar to the next month (maybe next year if we are in december)
(define-compta-command (com-next-month :name t)
    ((incrementor 'calendar-view-incrementor :gesture :select))
  (let* ((month (if (= (+ 1 (month incrementor)) 13) 
		   1
		   (+ 1 (month incrementor))))
	(year (if (= 1 month)
		  (+ 1 (year incrementor))
		  (year incrementor))))
  (setf (stream-default-view (find-pane-named *application-frame* 'main))
	  (make-instance 'calendar-view :month month :year year))))

; command which increment the year of 1
; call the calendar to the next year in the same month
(define-compta-command (com-next-year :name t)
    ((incrementor 'calendar-view-incrementor :gesture :menu))
  (setf (stream-default-view (find-pane-named *application-frame* 'main))
	  (make-instance 'calendar-view :month (month incrementor) :year (+ 1 (year incrementor)))))

; command which decrement the month of 1
; call the calendar to the previous month (maybe previous year if we are in january)
(define-compta-command (com-previous-month :name t)
    ((decrementor 'calendar-view-decrementor :gesture :select))
  (let* ((month (if (= (- (month decrementor) 1) 0) 
		   12
		   (- (month decrementor) 1)))
	(year (if (= month 12)
		  (- (year decrementor) 1)
		  (year decrementor))))
  (setf (stream-default-view (find-pane-named *application-frame* 'main))
	  (make-instance 'calendar-view :month month :year year))))

; command which decrement the year of 1
; call the calendar to the previous year in the same month
(define-compta-command (com-previous-year :name t)
    ((decrementor 'calendar-view-decrementor :gesture :menu))
  (setf (stream-default-view (find-pane-named *application-frame* 'main))
	  (make-instance 'calendar-view :month (month decrementor) :year (- (year decrementor) 1))))