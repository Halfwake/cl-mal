(in-package #:cl-mal)

;;; Print Methods

;;; These are print methods defined on the entry and values objects to make them more informative than the default text.

(defmethod print-object ((entry entry) stream)
  (with-slots (type title id) entry
    (format stream "(ENTRY ~a ~a ~a)" type title id)))
