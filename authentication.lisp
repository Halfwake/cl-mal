(in-package #:cl-mal)

;;; Authentication

;; The username and password for http authentication are special
;; variables. They can be reassigned or shadowed with a new binding
;; over all code that uses them. The functions that the username and
;; special variables use them as default values in their argument
;; list. The authentication variables can be manually specified in
;; function calls.

(defparameter *username* nil
  "This USERNAME is for basic HTTP authorization.")

(defparameter *password* nil
  "This PASSWORD is for basic HTTP authorization.")

(defun verify-credentials (&key (username *username*) (password *password*))
  "Verify the credentials. Returns the account ID number and the username on success. On failure, returns nil."
  (multiple-value-bind (response code) 
      (drakma:http-request (format nil "~a/api/account/verify_credentials.xml"
				   *base-url*)
			   :method :get
			   :basic-authorization (list username password))
    (ecase code
      (200 (let* ((parsed-response (xmls:parse (map 'string #'code-char response)))
		  (id (parse-integer (third (third parsed-response))))
		  (username (third (fourth parsed-response))))
	     (values id username)))
      (204 nil))))
