(in-package #:cl-user)

(defpackage #:cl-mal
  (:use #:cl)
  (:export #:*base-url*
	   #:*port*
	   #:*username*
	   #:*password*
	   #:verify-credentials
	   #:media-search
	   #:add-entry
	   #:update-entry
	   #:delete-entry))
