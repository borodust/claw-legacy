(cl:in-package :claw.spec)

(declaim (special *type*
                  *cursor*
                  *parent*
                  *name*
                  *location*
                  *bit-size*
                  *bit-alignment*
                  *comment*))


(defun format-cursor-location (cursor)
  (let* ((type (claw.resect:cursor-type cursor))
         (declaration (claw.resect:resect-type-declaration type))
         (location (if (eq :unknown (claw.resect:cursor-kind declaration))
                       (claw.resect:cursor-location cursor)
                       (claw.resect:cursor-location declaration))))
    (format nil "~A:~A:~A"
            (claw.resect:cursor-location-name location)
            (claw.resect:cursor-location-line location)
            (claw.resect:cursor-location-column location))))

;;;
;;; BUILDER
;;;
(defgeneric make-entity-builder (kind)
  (:method (kind) (declare (ignore kind))))
(defgeneric build-foreign-entity (builder))
(defgeneric consume-cursor (consumer kind)
  (:method (consumer kind) (declare (ignore consumer kind))))


;;;
;;; PARSER
;;;
(defclass foreign-entity-stream-parser ()
  ((builders :initform nil)
   (cursors :initform nil)))


(defun make-foreign-entity-stream-parser ()
  (make-instance 'foreign-entity-stream-parser))


(defmethod %parse-cursor (parser kind)
  (with-slots (builders cursors) parser
    (flet ((diving-deeper-p ()
             (or (null cursors)
                 (cursor-equal-p (first cursors) *parent*)))
           (same-parent-p ()
             (let ((parent (second cursors)))
               (or (null parent)
                   (cursor-equal-p parent *parent*)))))
      (cond
        ((diving-deeper-p)
         (push *cursor* cursors)
         (push-entity-builder parser (make-entity-builder kind)))
        ((same-parent-p)
         (pop cursors)
         (pop-entity-builder parser)
         (push *cursor* cursors)
         (push-entity-builder parser (make-entity-builder kind)))
        (t (loop until (or (null cursors) (cursor-equal-p *cursor* (first cursors)))
                 do (pop cursors)
                    (pop-entity-builder parser)))))
    (consume-cursor (some #'identity builders) kind)))


(defun parse-cursor (parser cursor parent)
  (let* ((type (claw.resect:cursor-type cursor))
         (declaration (claw.resect:resect-type-declaration type))
         (comment (if (eq :unknown (claw.resect:cursor-kind declaration))
                      (claw.resect:cursor-comment cursor)
                      (claw.resect:cursor-comment declaration))))
    (let ((*cursor* cursor)
          (*parent* parent)
          (*type* type)
          (*location* (format-cursor-location cursor))
          (*bit-size* (* (claw.resect:resect-type-size type) 8))
          (*bit-alignment* (* (claw.resect:resect-type-alignment type) 8))
          (*comment* comment))
      (%parse-cursor parser (claw.resect:cursor-kind cursor)))))


(defun pop-entity-builder (parser)
  (with-slots (builders) parser
    (let ((builder (pop builders)))
      (when builder
        (build-foreign-entity builder)))))


(defun push-entity-builder (parser new-builder)
  (with-slots (builders) parser
    (push new-builder builders)))


(defun cursor-equal-p (this that)
  (let ((this-location (format-cursor-location this)))
    (or (equal (claw.resect:cursor-id this) (claw.resect:cursor-id that))
        (and (not (emptyp this-location))
             (equal this-location (format-cursor-location that))))))




(defun flush-foreign-enitity-stream-parser (parser)
  (loop while (pop-entity-builder parser)))


;;;
;;; LIBRARY SPECIFICATION
;;;
(defun print-debug-information ()
  (format t "~&~%~A ~A" (claw.resect:cursor-kind *cursor*) *location*)
  (format t "~&Size: ~A~&Alignment: ~A"
          (claw.resect:resect-type-size *type*)
          (claw.resect:resect-type-alignment *type*))
  (format t "~&Comment: ~A" *comment*)
  (format t "~&Debug: ~A" (claw.resect:cursor-debug-info *cursor*)))


(defun build-library-specification (path)
  (let ((*library-specification* (make-instance 'library-specification))
        (parser (make-foreign-entity-stream-parser)))
    (flet ((visit (current parent)
             (parse-cursor parser current parent)
             :continue))
      (claw.resect:parse path #'visit))
    (flush-foreign-enitity-stream-parser parser)
    *library-specification*))


(defmethod consume-cursor (this kind)
  (declare (ignore this kind))
  #++(print-debug-information))
