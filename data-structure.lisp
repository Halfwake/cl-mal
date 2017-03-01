(in-package #:cl-mal)

;;; Major Data Structures

;;; The classes entry class and media-values classes map to XML data
;;; from or for the API calls. The entry class is used to represent
;;; the results of a media search. It descibres the media. The
;;; media-values classes represent user information about a piece of
;;; media.

(defclass entry ()
  ((id :initarg :id)
   (title :initarg :title)
   (english :initarg :english)
   (synonyms :initarg :synonyms)
   (episodes :initarg :episodes)
   (type :initarg :type)
   (status :initarg :status)
   ;; TODO use date libraries
   (start-date :initarg :start-date)
   (end-date :initarg :end-date)
   (synonpsis :initarg :synonpsis)
   (image :initarg :image))
  (:documentation "Represents a media entry."))

(defclass media-values ()
  ()
  (:documentation "The values of an item from a MAL list."))

(defclass manga-values (media-values)
  ((chapter :initarg :chapter)
   (volume :initarg :volume)
   (status :initarg :status)
   (score :initarg :status)
   (times-reread :initarg :times-reread)
   (reread-value :initarg :reread-value)
   (date-start :initarg :date-start)
   (date-finish :initarg :date-finish)
   (priority :initarg :priority)
   (enable-discussion :initarg :enable-discussion)
   (enable-rereading :initarg :enable-rereading)
   (comments :initarg :enable-comments)
   (scan-group :initarg :scan-group)
   (tags :initarg :tags)
   (retail-volumes :initarg :retail-volume))
  (:documentation "User information about a manga entry."))

(defclass anime-values (media-values)
  ((episode :initarg :episode)
   (status :initarg :status)
   (score :initarg :score)
   (storage-type :initarg :storage-type)
   (storage-value :initarg :storage-value)
   (times-rewatched :initarg :times-rewatched)
   (rewatch-value :initarg :rewatch-value)
   (date-start :initarg :date-start)
   (date-finish :initarg :date-finish)
   (priority :initarg :priority)
   (enable-discussion :initarg :enable-discussion)
   (enable-rewatching :initarg :enable-rewatching)
   (comments :initarg :comments)
   (fansub-group :initarg :fansub-group)
   (tags :initarg :tags))
  (:documentation "User information about an anime entry."))
