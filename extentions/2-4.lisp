;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                           ;;; 
;;; File : 2-4.lisp                                                                                           ;;; 
;;; Use of a configuration file                                                                               ;;;
;;;                                                                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                                                           ;;;
;;; Features :                                                                                                ;;;
;;;         - Easy configuration syntax for non-programmers                                                   ;;;
;;;         - Default values if not in the config file or the config file does not exist                      ;;;
;;;                                                                                                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(in-package :compta-gui)

;; @Override
;; Replace size values by the readed configuration 
(define-application-frame compta ()
  ((%current-organization :initform (make-instance 'organization :name "Home")
                          :accessor current-organization)
   (%organization-list :initform (cons (make-instance 'organization :name "Home") nil)
		       :accessor organization-list))

  (:panes (main :application
		:width (configuration-value *config* 400 :width :main :windows)
		:height (configuration-value *config* 200 :height :main :windows)
                :display-function 'display-main)
	  (accounts :application
                    :width (configuration-value *config* 200 :width :accounts :windows)
                    :height (configuration-value *config* 200 :height :accounts :windows)
                    :display-function 'display-accounts)
          (transactions :application
                        :width (configuration-value *config* 200 :width :transactions :windows)
                        :height (configuration-value *config* 200 :height :transactions :windows)
                        :display-function 'display-transactions)
          (organizations :application
                        :width (configuration-value *config* 200 :width :organizations :windows)
                        :height (configuration-value *config* 200 :height :organizations :windows)
                        :display-function 'display-organizations)
	  (inter :interactor
		 :width (configuration-value *config* 200 :width :inter :windows)
		 :height (configuration-value *config* 200 :height :inter :windows)))
  (:layouts (default
		(horizontally
		    ()
		  (vertically () main inter)
		  transactions
		  (vertically () accounts organizations)))))

;; @Override
;; Parse the configuration file
(defun compta ()
  (defparameter *config* (if (probe-file #P"extentions/.compta") 
			     (parse-configuration-file #P"extentions/.compta")
			     nil))
  (run-frame-top-level (make-application-frame 'compta)))