;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                           ;;; 
;;; File : 1-1.lisp                                                                                           ;;; 
;;; Display sum of Debits ans Credits for a given account                                                     ;;;
;;;                                                                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:compta-gui)

;; After displaying transactions, we compute and show the sum of debits and credits
(defmethod display-main-with-view :after (frame pane (view account-view))
  (declare (ignore frame))
  (let ((account (account view))
	(current-entry-debits)
	(current-entry-credits)
	(credits-sum 0)
	(debits-sum 0)
	(medium (sheet-medium pane)))

    (loop for transaction in (transactions (current-organization *application-frame*))
	  do (setf current-entry-debits (find account (debits transaction) :key #'account))
	  do (setf current-entry-credits (find account (credits transaction) :key #'account))
	  do (if (not (null current-entry-credits))
		 (setf credits-sum (+ credits-sum (amount current-entry-credits))))
	     
	  do (if (not (null current-entry-debits))
		 (setf debits-sum (+ debits-sum (amount current-entry-debits)))))
    
    (with-output-as-presentation
	(pane credits-sum 'amount)
      (format pane "~%Credits sum :")
      (with-text-family
	  (medium :fixed)
	(format-amount pane credits-sum "~13d.~2,'0d~50t~%"))
      (format pane "Debits sum : ")
      (with-text-family
	  (medium :fixed)
	(format-amount pane debits-sum "~13d.~2,'0d~50t~%"))
      (format pane "Balance :     ")
      (with-text-family
	  (medium :fixed)
	(format-amount pane (- credits-sum debits-sum) "~13@d.~2,'0d~50t")))))
