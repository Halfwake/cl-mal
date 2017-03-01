(in-package #:cl-mal)

;;; ID

;;; The ID classes are unique identifiers for different pieces of
;;; media. The ID numbers have to be encapsulated into seperate
;;; objects because manga and anime objects can share the same ID
;;; number. Packaging the ID numbers into objects makes dispatching
;;; onto different types of media easier.

(defclass media-id ()
  ()
  (:documentation "A media ID."))

(defclass anime-id (media-id)
  ((id-number :initarg :id :reader id-number))
  (:documentation "An anime ID."))

(defclass manga-id (media-id)
  ((id-number :initarg :id :reader id-number))
  (:documentation "A manga ID."))

(defun make-anime-id (id-number)
  "Make an anime-id object with the given ID-NUMBER."
  (make-instance 'anime-id :id id-number))

(defun make-manga-id (id-number)
  "Make an manga-id object with the given ID-NUMBER."
  (make-instance 'manga-id :id id-number))
