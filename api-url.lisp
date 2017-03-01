(in-package #:cl-mal)

;; API URL Building

;; The media-list-url-string methods are used to generate the URL for
;; different API requests. Manga and anime entries share the same ID
;; numbers, so there are two different URL paths. One path is for
;; manga and the other path is for anime.

(defparameter *base-url* "https://myanimelist.net"
  "The BASE-URL is the location of the API server.")

(defgeneric media-list-url-string (id)
  (:documentation "The portion of the url that determines which list this media can be added to."))

(defmethod media-list-url-string ((id anime-id))
  "animelist")

(defmethod media-list-url-string ((id manga-id))
  "mangalist")
