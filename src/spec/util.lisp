(cl:in-package :claw.spec)

;; misc

(declaim (special *include-definitions*
                  *exclude-definitions*
                  *include-sources*
                  *exclude-sources*))

;; testing

(defun included-p (thing includes)
  (when thing
    (loop for scanner in includes do
      (when (cl-ppcre:scan scanner thing)
        (return t)))))

(defun excluded-p (name location)
  (and (or (included-p name *exclude-definitions*)
           (and (included-p location *exclude-sources*)
                (not (included-p name *include-definitions*))))
       (not (or (included-p name *include-definitions*)
                (and (included-p location *include-sources*)
                     (not (included-p name *exclude-definitions*)))))))

(defun anonymous-p (form)
  (etypecase form
    (foreign-entity
     (or (null (foreign-entity-name form))
         (emptyp (foreign-entity-name form))))
    (cons
     (or (string= "" (aval :name form))
         (and (string= ":array" (aval :tag form))
              (string= "" (aval :name (aval :type form))))))))


(defgeneric primitivep (entity)
  (:method (entity) (declare (ignore entity)) nil))


;;;
;;; Inclusion rules
;;;
(defun explicitly-included-p (name location)
  (or (included-p name *include-definitions*)
      (and (included-p location *include-sources*)
           (not (included-p name *exclude-definitions*)))))


(defun explicitly-excluded-p (name location)
  (or (included-p name *exclude-definitions*)
      (and (included-p location *exclude-sources*)
           (not (included-p name *include-definitions*)))))


(defun keywordify (tag)
  (let* ((tag (if (eq #\: (aref tag 0)) (subseq tag 1) tag)))
    (make-keyword (uiop:standard-case-symbol-name tag))))
