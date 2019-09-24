(cl:in-package :claw.spec)


(declaim (special *inclusion-table*))


(define-constant +inclusion-status-weights+
    '(:enforced 6
      :weakly-enforced 5
      :partially-enforced 4
      :excluded 3
      :included 2
      :partially-included 1
      :weakly-excluded 0)
  :test #'equal)


(defgeneric next-inclusion-status (from to))


(defmethod next-inclusion-status (old-status new-status)
  (let ((old-weight (getf +inclusion-status-weights+ old-status))
        (new-weight (getf +inclusion-status-weights+ new-status)))
    (if (<= old-weight new-weight)
        new-status
        old-status)))


(defmethod next-inclusion-status ((old-status (eql :partially-enforced))
                                  (new-status (eql :included)))
  (declare (ignore old-status new-status))
  :weakly-enforced)


(defmethod next-inclusion-status ((old-status (eql :weakly-enforced))
                                  (new-status (eql :excluded)))
  (declare (ignore old-status new-status))
  :partially-enforced)


(defun find-inclusion-status (typespec)
  (gethash typespec *inclusion-table* :weakly-excluded))


(defun upgrade-inclusion-status (typespec new-status)
  (let ((old-status (find-inclusion-status typespec)))
    (unless (eq old-status (next-inclusion-status old-status new-status))
      (setf (gethash typespec *inclusion-table*) new-status)
      old-status)))


(defun transfer-inclusion-status (from-typespec to-typespec)
  (let ((status (find-inclusion-status from-typespec)))
    (upgrade-inclusion-status to-typespec status)))


(defun marked-included-p (typespec)
  (member (find-inclusion-status typespec)
          '(:included :partially-included
            :enforced :partially-enforced :weakly-enforced)))


(defun marked-fully-included-p (typespec)
  (member (find-inclusion-status typespec)
          '(:included :enforced :weakly-enforced)))


(defun marked-enforced-p (typespec)
  (member (find-inclusion-status typespec)
          '(:partially-enforced :enforced :weakly-enforced)))


(defun marked-fully-enforced-p (typespec)
  (eq (find-inclusion-status typespec) :enforced))


(defun marked-partially-included-p (typespec)
  (member (find-inclusion-status typespec)
          '(:partially-enforced :partially-included)))


(defun mark-included (typespec &optional enforce)
  (upgrade-inclusion-status typespec (if enforce
                                         :enforced
                                         :included)))


(defun mark-partially-included (typespec &optional enforce)
  (upgrade-inclusion-status typespec (if enforce
                                         :partially-enforced
                                         :partially-included)))


(defun mark-excluded (typespec)
  (upgrade-inclusion-status typespec :excluded))
