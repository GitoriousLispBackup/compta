;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                           ;;; 
;;; File : 2-3.lisp                                                                                           ;;; 
;;; Allowing to load several organizations in the same time                                                   ;;;
;;;                                                                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                           ;;;
;;;      You can use the provided test organization (test_organization file)                                  ;;;
;;;                                                                                                           ;;;
;;; Features :                                                                                                ;;;
;;;         - Loading organizations                                                                           ;;;
;;;         - Notify the user if he wants to load an organization with the same name as an already loaded one ;;;
;;;         - Display of loaded organizations                                                                 ;;;
;;;         - Fast switch between loaded organizations                                                        ;;;
;;;         - Unload organizations                                                                            ;;;
;;;         - Notify the user if he tries to unload the last organization                                     ;;;
;;;                                                                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :compta-gui)

;; @Override
;; Add a member to hold a list of loaded organizations and a panel to show them
(define-application-frame compta ()
  ((%current-organization :initform (make-instance 'organization :name "Home")
                          :accessor current-organization)
   (%organization-list :initform (cons (make-instance 'organization :name "Home") nil)
		       :accessor organization-list))
  (:panes (main :application
		:width 800
		:height 500
                :display-function 'display-main)
	  (accounts :application
                    :width 200
                    :height 650
                    :display-function 'display-accounts)
          (transactions :application
                        :width 300
                        :height 650
                        :display-function 'display-transactions)
          (organizations :application
                        :width 200
                        :height 250
                        :display-function 'display-organizations)
	  (inter :interactor
		 :width 500
		 :height 100))
  (:layouts (default
		(horizontally
		    ()
		  (vertically () main inter)
		  transactions
		  (vertically () accounts organizations)))))

;; @Override
;; Command to load an organization
(define-compta-command (com-read-organization :name t) ((filename 'pathname))
  (let ((loading-organization (read-model filename *compta-allowed-version-names*)))
    (if (not (member (name loading-organization) (organization-list *application-frame*) :test 'string= :key #'name))
	(progn 
	  (push loading-organization (organization-list *application-frame*))
	  (setf (current-organization *application-frame*) loading-organization))
	(if (string= (accept 'string :prompt "This organization seems already loaded. Do you want to overwrite it ? (y/n) " :default "y") "y")
	    (progn
	      (setf (organization-list *application-frame*) (remove (name loading-organization) (organization-list *application-frame*) :test 'string= :key #'name))
	      (push loading-organization (organization-list *application-frame*))
	      (setf (current-organization *application-frame*) loading-organization))))))

;; Command to unload an organization
(define-compta-command (com-unload-organization :name t)
    ((loaded-organization 'organization :gesture :edit))
  (if (not (null (cdr (organization-list *application-frame*))))
      (progn (setf (organization-list *application-frame*) (remove (name loaded-organization) (organization-list *application-frame*) :test 'string= :key #'name))
	     (setf (current-organization *application-frame*) (car (organization-list *application-frame*))))
      (format (find-pane-named *application-frame* 'inter) "Cannot unload the last organization !~%")))

;; @Override
;; Command to read the default organization
(define-compta-command (com-read-organization-default :name t) ()
  (com-read-organization #p"home"))

;; Command to switch between loaded organizations
(define-compta-command (com-change-organization :name t) 
    ((loaded-organization 'organization :gesture :select))
  (setf (current-organization *application-frame*) loaded-organization))

;; Display for the organization panel
(defun display-organizations (frame pane)
  (format pane "Organizations~%~%")
  (loop for organization in (organization-list *application-frame*)
     do (display-oneline-organization-summary pane organization)))

;; Display for an organization in the organization panel
(defun display-oneline-organization-summary (pane organization)
  (with-output-as-presentation (pane organization 'organization)
    (format pane "~a~%" (name organization))))

;; Display for an organization in the organization panel
(define-presentation-method present (object (type organization)
					    stream (view textual-view) &key)
  (format stream "~a" (name object)))