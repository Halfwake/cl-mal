(in-package #:cl-mal)

;;; Serialization & Deserialization

;;; Several defintions for converting data to XML are below. The
;;; function xml-to-entry converts an XML object describing a media
;;; entry into an object. The media-values-to-xml methods convert user
;;; defined values of a media entry into XML that can be sent to the
;;; API server.

(defun xml-to-entry (xml)
  (check-type xml list)
  (flet ((get-field (item)
	   (first (last (assoc item (cddr xml) :test #'string=)))))
    (make-instance 'entry
		   :id (make-anime-id (parse-integer (get-field "id")))
		   :title (get-field "title")
		   :english (or (get-field "english") "")
		   :synonyms (loop for string in
				  (cl-ppcre:split ";" (or (get-field "synonyms") ""))
				collect (string-trim " \t\n\r" string))
		   :episodes (or (parse-integer (get-field "episodes")) 0)
		   :type (or (get-field "type") "")
		   :status (or (get-field "status") "")
		   :start-date (or (get-field "start-date") "")
		   :end-date (or (get-field "end-date") "")
		   :synonpsis (or (get-field "synonpsis") "")
		   :image (or (get-field "image") ""))))

(defgeneric media-values-to-xml (media-values)
  (:documentation "Convert MEDIA-VALUES to XML for the MAL API."))

(defmethod media-values-to-xml ((anime-values anime-values))
  (with-slots (episode status score storage-type storage-value times-rewatched
		       rewatch-value date-start date-finish priority
		       enable-discussion enable-rewatching comments
		       fansub-group tags)
      anime-values
    (with-output-to-string (stream)
      (xmls:write-prologue '(("version" . "1.0")
			     ("encoding" . "UTF-8"))
			   nil
			   stream)
      (xmls:write-xml
       `("entry" nil
		 ("episode" nil ,episode)
		 ("status" nil ,status)
		 ("score" nil ,score)
		 ("storage_type" nil ,storage-type)
		 ("storage_value" nil ,storage-value)
		 ("times_rewatched" nil ,times-rewatched)
		 ("rewatch_value" nil ,rewatch-value)
		 ("date_start" nil ,date-start)
		 ("date_finish" nil ,date-finish)
		 ("priority" nil ,priority)
		 ("enable_discussion" nil ,(if enable-discussion 1 0))
		 ("enable_rewatching" nil ,(if enable-rewatching 1 0))
		 ("comments" nil ,comments)
		 ("fansub_group" nil ,fansub-group)
		 ("tags" nil ,(tag-list-to-string tags)))
       stream))))

(defmethod media-values-to-xml ((manga-values manga-values))
  (with-slots (chapter volume status score times-reread reread-value
		       date-start date-finish priority enable-discussion
		       enable-rereading comments scan-group tags retail-volumes)
      manga-values
    (with-output-to-string (stream)
      (xmls:write-prolog '(("version" . "1.0")
			   ("encoding" . "UTF-8"))
			 nil
			 stream)
      (xmls:write-xml
       `("entry" nil
		 ("chapter" nil ,chapter)
		 ("volume" nil ,volume)
		 ("status" nil ,status)
		 ("times_reread" nil ,times-reread)
		 ("reread_value" nil ,reread-value)
		 ("date_start" nil ,date-start)
		 ("date_finish" nil ,date-finish)
		 ("priority" nil ,priority)
		 ("enable_discussion" nil ,enable-discussion)
		 ("enable_rereading" nil ,enable-rereading)
		 ("comments" nil ,comments)
		 ("scan_group" nil ,scan-group)
		 ("tags" nil ,(tag-list-to-string tags))
		 ("retail_volumes" nil ,retail-volumes))
       nil))))
