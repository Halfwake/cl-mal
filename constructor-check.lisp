(in-package #:cl-mal)

;;; Constructors

;;; These defintions add some extra processing to the
;;; constructors. They add type checking, so that if a bad object is
;;; created then an exception will be raised when the constructor is
;;; run instead of later in the program far away from the actual
;;; mistake.

(defmethod initialize-instance :after ((entry entry) &rest args)
  (declare (ignore args))
  (with-slots (id title english synonyms episodes type status
		  start-date end-date synonpsis image)
      entry
    (check-type id fixnum)
    (check-type title string)
    (check-type english string)
    (check-type synonyms list)
    (check-type episodes fixnum)
    (check-type type string)
    (check-type status string)
    (check-type start-date string)
    (check-type end-date string)
    (check-type synonpsis string)
    (check-type image string)))

(defmethod initialize-instance :after ((anime-values anime-values) &rest args)
  (declare (ignore args))
  (with-slots (episode status score storage-type storage-value times-rewatched
		       rewatch-value date-start date-finish priority
		       enable-discussion enable-rewatching comments
		       fansub-group tags)
      anime-values
    (check-type episode fixnum)
    (check-type status (member 1 2 3 4 6))
    (check-type score fixnum)
    (check-type storage-type fixnum)
    (check-type storage-value float)
    (check-type times-rewatched fixnum)
    (check-type rewatch-value fixnum)
    (check-type date-start string)
    (check-type date-finish string)
    (check-type priority fixnum)
    (check-type enable-discussion boolean)
    (check-type enable-rewatching boolean)
    (check-type comments string)
    (check-type fansub-group string)
    (check-type tags list)))

(defmethod initialize-instance :after ((manga-values manga-values) &rest args)
  (declare (ignore args))
  (with-slots (chapter volume status score times-reread reread-value
		       date-start date-finish priority enable-discussion
		       enable-rereading comments scan-group tags retail-volumes)
      manga-values
    (check-type chapter fixnum)
    (check-type volume fixnum)
    (check-type status (member 1 2 3 4 6))
    (check-type score fixnum)
    (check-type times-reread fixnum)
    (check-type reread-value fixnum)
    (check-type date-start string)
    (check-type date-finish string)
    (check-type priority fixnum)
    (check-type enable-discussion boolean)
    (check-type enable-rereading boolean)
    (check-type comments string)
    (check-type scan-group string)
    (check-type tags list)
    (check-type retail-volumes fixnum)))
