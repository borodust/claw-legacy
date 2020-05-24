(cl:in-package :claw.wrapper)

(declaim (special *path-mapper*))

(defun map-path (path)
  (funcall *path-mapper* path))

(defgeneric expand-library-definition (generator
                                       language
                                       wrapper
                                       configuration))
