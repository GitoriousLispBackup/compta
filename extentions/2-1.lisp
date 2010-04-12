;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                           ;;; 
;;; File : 2-1.lisp                                                                                           ;;; 
;;; Group accounts                                                                                            ;;;
;;;                                                                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:compta-model)

;Redefine organization class
(defclass organization (name-mixin)
  ((%accounts-groups :initarg :accounts-groups :initform '() :accessor accounts-groups)
   (%accounts :initarg :accounts :initform '() :accessor accounts)
   (%transactions :initarg :transactions :initform '() :accessor transactions)))

;Define a new class to implement accounts group
(defclass accounts-group (name-mixin)
  ((%accounts :initarg :accounts :initform '() :accessor accounts)))


(in-package #:compta-gui)

; redifine the compta frame to have a group pane
(define-application-frame compta ()
  ((%current-organization :initform (make-instance 'organization :name "Home")
                          :accessor current-organization))
  (:panes (main :application
		:width 800
		:height 500
                :display-function 'display-main)
	  (accounts :application
                    :width 200
                    :height 650
                    :display-function 'display-accounts)
	  (groups :application
			   :width 200
			   :height 650
			   :display-function 'display-groups)
          (transactions :application
                        :width 300
                        :height 650
                        :display-function 'display-transactions)
	  (inter :interactor
		 :width 500
		 :height 100))
  (:layouts (default
		(horizontally
		    ()
		  (vertically () main inter)
		  transactions
		  accounts
		  groups))))

;Define the main view of the group
(defclass group-view (view)
  ((%group :initarg :group :reader group)))

(defun display-oneline-groups-summary (pane accounts-group)
  (with-output-as-presentation (pane accounts-group 'accounts-group)
    (format pane "~a~%" (name accounts-group))))

(defun display-groups (frame pane)
  (format pane "Accounts groups~%~%")
  (loop for group in (reverse (accounts-groups (current-organization frame)))
        do (display-oneline-groups-summary pane group)))

; Account adder, action realize by clicking on the add button in the view
(defclass account-adder ()
  ((%adder :initarg :adder :accessor adder)))

; Account remover, action realize by clicking on the remove button in the view
(defclass account-remover ()
  ((%remover :initarg :remover :accessor remover)))

; method detail to show the group view
(defun display-account-adder (pane area-name push-account accounts)
  (flet ((show-account (account)
	   (with-output-as-presentation (pane account 'account)
	     (format pane "~a~%" (name account)))
	   (let ((debits-sum 0)
		 (credits-sum 0))
	     (loop for transaction in (reverse (transactions (current-organization *application-frame*)))
		do (let ((dbs-entry (find account (debits transaction) :key #'account))
			(cds-entry (find account (credits transaction) :key #'account)))
		    (unless (null dbs-entry)
		      (setf debits-sum (+ debits-sum (amount dbs-entry))))
		    (unless (null cds-entry)
		      (setf credits-sum (+ credits-sum (amount cds-entry))))))
	     (format pane "Debits sum: ")
	     (format-amount pane debits-sum  "~10d.~2,'0d~50t")
	     (format pane "     Credits sum: ")
	     (format-amount pane credits-sum  "~10d.~2,'0d~50t")
	     (format pane "~%"))))
    (format pane "~a: " area-name)
    (let ((adder (make-instance 'account-adder
				:adder push-account)))
      (with-output-as-presentation (pane adder 'account-adder)
	(format pane "[add]~%")))
    (loop for account in accounts
	  do (show-account account))))

(defmethod display-main-with-view (frame pane (view group-view))
  (declare (ignore frame))
  (let ((group (group view)))  
    (display-oneline-groups-summary pane group)
    (format pane "~%")
    (display-account-adder pane "Accounts"
			   (lambda (account) (push account (accounts group))) (accounts group))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; COMMAND ASSOCIATED TO THE CALENDAR EXTENTION ;;;;;;;;;;;;;;;;;;;


; Command to add a new group
(define-compta-command (com-new-accounts-group :name t) ((name 'string))
  (push (make-instance 'accounts-group :name name)
        (accounts-groups (current-organization *application-frame*))))

; actoin to show the main view of this account
(define-compta-command (com-edit-accounts-group :name t)
    ((group 'accounts-group :gesture :select))
  (setf (stream-default-view (find-pane-named *application-frame* 'main))
        (make-instance 'group-view :group group)))

; action to add an acount in a selected group (without group view)
(define-compta-command (com-add-account-in-selected-group :name t)
    ((group 'accounts-group :gesture :menu :keboard (:control #\a)))
  (push (find (accept 'account) (accounts (current-organization *application-frame*))) (accounts group)))

; action to add an account in the current group (show in the main view)
(define-compta-command (com-add-account-in-group :name t)
    ((adder 'account-adder :gesture :select))
  (funcall (adder adder) 
           (find (accept 'account) (accounts (current-organization *application-frame*)))))