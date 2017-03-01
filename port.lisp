(in-package #:cl-mal)

;;; Port

;;; The port the API calls will attempt to connect to the MAL service
;;; on. This shouldn't need to be changed except for testing or
;;; proxies.

(defparameter *port* 80
  "The port to try and find a service on.")
