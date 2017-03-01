(in-package #:cl-mal)

(defun media-search (media query &key (username *username*) (password *password*))
  "Search for MEDIA using the string QUERY."
  (check-type media (member :anime :manga))
  (check-type query string)
  (check-type username string)
  (check-type password string)
  (multiple-value-bind (response code)
      (drakma:http-request (format nil "~a/api/~a/search.xml"
				   *base-url*
				   (ecase media
				     (:anime "anime")
				     (:manga "manga")))
			   :method :post
			   :parameters `(("q" . ,query))
			   :basic-authorization (list username password))
    (ecase code
      (200 (mapcar #'xml-to-entry
		   (cdddr (xmls:parse (map 'string #'code-char response)))))
      (204 nil))))

;;; CRUD Operations

;;; The add-entry, update-entry, and delete-entry functions are for
;;; changing the status of a media entry. Use add-entry to initialize
;;; a new entry with some values, update-entry to change an existing
;;; entry, and delete-entry to remove an entry from the list.

;;; Each of these functions takes two keyword arguments for
;;; authentication. By default, the special *username* and *password*
;;; variables will be used, but different values can be specified
;;; directly to the function call.

(defun add-entry (id values &key (username *username*) (password *password*))
  (check-type id media-id);; TODO
  (check-type values media-values) ;;TODO
  (check-type username string)
  (check-type password string)
  (multiple-value-bind (response code)
      (drakma:http-request (format nil "~a/api/~a/add/~a.xml"
				   *base-url*
				   (media-list-url-string id)
				   (id-number id))
			   :method :post
			   :parameters `(("data" . ,(media-values-to-xml values)))
			   :basic-authorization (list username password))
    (declare (ignore response))
    (= 201 code)))

(defun update-entry (id values &key (username *username*) (password *password*))
  (check-type id media-id) ;;TODO
  (check-type values media-values) ;;TODO
  (check-type username string)
  (check-type password string)
  (multiple-value-bind (response code)
      (drakma:http-request (format nil "~a/api/~a/update/~a.xml"
				   *base-url*
				   (media-list-url-string id)
				   (id-number id))
			   :method :post
			   :parameters `(("data" . ,(media-values-to-xml values)))
			   :basic-authorization (list username password))
    (declare (ignore code))
    (string= response "Updated")))

(defun delete-entry (id &key (username *username*) (password *password*))
  "Remove the anime with ID from the list."
  (check-type id (member media-id entry))
  (check-type username string)
  (check-type password string)
  (multiple-value-bind (response code)
      (drakma:http-request (format nil "~a/api/~a/update/~a.xml"
				   *base-url*
				   (media-list-url-string id)
				   (id-number id))
			   :method :delete
			   :basic-authorization (list username password))
    (declare (ignore code))
    (string= response "Deleted")))




;;; Utility Functions

;;; These are just some utility functions used through out the
;;; system. Most of them are used for converting individual values
;;; from a client version to a API version. For instance, the cl-mal
;;; representation for tags is a list of strings, but the API expects
;;; a comma seperated list.

(defun string-to-tag-list (string)
  (loop for string in (cl-ppcre:split "," string)
     collect (string-trim " \t\n\r" string)))

(defun tag-list-to-string (tag-list)
  (string-right-trim (list #\,)
		     (format nil "~{~a,~}" tag-list)))

