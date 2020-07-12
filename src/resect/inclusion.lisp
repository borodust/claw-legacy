(cl:in-package :claw.resect)


(declaim (special *inclusion-table*
                  *include-definitions*
                  *exclude-definitions*
                  *include-sources*
                  *exclude-sources*))


(define-constant +inclusion-status-weights+
    '(:enforced 6        ; included unconditionally
      :weakly-enforced 5 ; included but can be downgraded to :partially-enforced
      :partially-enforced 4             ; partially included unconditionally
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


(defmethod next-inclusion-status ((old-status (eql :included))
                                  (new-status (eql :partially-enforced)))
  (declare (ignore old-status new-status))
  :weakly-enforced)


(defmethod next-inclusion-status ((old-status (eql :weakly-enforced))
                                  (new-status (eql :excluded)))
  (declare (ignore old-status new-status))
  :partially-enforced)


(defun find-inclusion-status (entity-id)
  (gethash entity-id *inclusion-table* :weakly-excluded))


(defun upgrade-inclusion-status (entity-id new-status)
  "Returns new status if successfully upgraded or old one otherwise"
  (let* ((old-status (find-inclusion-status entity-id))
         (next-status (next-inclusion-status old-status new-status)))
    (setf (gethash entity-id *inclusion-table*) next-status)))


(defun transfer-inclusion-status (from-entity-id to-entity-id)
  (let ((status (find-inclusion-status from-entity-id)))
    (upgrade-inclusion-status to-entity-id status)))


(defun marked-included-p (entity-id &optional table)
  (let ((*inclusion-table* (or table *inclusion-table*)))
    (member (find-inclusion-status entity-id)
            '(:included :partially-included
              :enforced :partially-enforced :weakly-enforced))))


(defun marked-fully-included-p (entity-id)
  (member (find-inclusion-status entity-id)
          '(:included :enforced :weakly-enforced)))


(defun marked-enforced-p (entity-id)
  (member (find-inclusion-status entity-id)
          '(:partially-enforced :enforced :weakly-enforced)))


(defun marked-fully-enforced-p (entity-id)
  (eq (find-inclusion-status entity-id) :enforced))


(defun marked-partially-included-p (entity-id)
  (member (find-inclusion-status entity-id)
          '(:partially-enforced :partially-included)))


(defun mark-included (entity-id &optional enforce)
  (upgrade-inclusion-status entity-id (if enforce
                                         :enforced
                                         :included)))


(defun mark-partially-included (entity-id &optional enforce)
  (upgrade-inclusion-status entity-id (if enforce
                                         :partially-enforced
                                         :partially-included)))


(defun mark-excluded (entity-id)
  (upgrade-inclusion-status entity-id :excluded))


;;;
;;;
;;;
(defun get-location-path (entity)
  (if (foreign-declared-p entity)
      (format-foreign-location (foreign-entity-location entity) nil)
      "::"))


(defun get-entity-name (entity)
  (if (foreign-named-p entity)
      (format-full-foreign-entity-name entity)
      ""))


(defun included-p (thing includes)
  (when thing
    (loop for scanner in includes
            thereis (and (cl-ppcre:scan scanner thing) t))))


(defun excluded-p (name location)
  (and (or (included-p name *exclude-definitions*)
           (and (included-p location *exclude-sources*)
                (not (included-p name *include-definitions*))))
       (not (or (included-p name *include-definitions*)
                (and (included-p location *include-sources*)
                     (not (included-p name *exclude-definitions*)))))))


(defun explicitly-included-p (name location)
  (or (included-p name *include-definitions*)
      (and (included-p location *include-sources*)
           (not (included-p name *exclude-definitions*)))))


(defun explicitly-excluded-p (name location)
  (or (included-p name *exclude-definitions*)
      (and (included-p location *exclude-sources*)
           (not (included-p name *include-definitions*)))))


(defun entity-explicitly-included-p (entity)
  (explicitly-included-p (get-entity-name entity)
                         (get-location-path entity)))


(defun entity-explicitly-excluded-p (entity)
  (and (not (typep entity 'foreign-primitive))
       (explicitly-excluded-p (get-entity-name entity)
                              (get-location-path entity))))


;;;
;;;
;;;
(defgeneric try-including-entity (entity))


(defmethod try-including-entity ((entity foreign-entity))
  (if (and (entity-explicitly-included-p entity)
           (not (entity-explicitly-excluded-p entity)))
      (prog1 t
        (mark-included (foreign-entity-id entity) t))
      (marked-included-p (foreign-entity-id entity))))


;;;
;;;
;;;
(defun make-inclusion-table (entities
                             include-definitions
                             include-sources
                             exclude-definitions
                             exclude-sources)
  (labels ((create-scanner (regex)
             (ppcre:create-scanner regex :single-line-mode t :extended-mode t))
           (create-scanners (regexps)
             (mapcar #'create-scanner regexps)))
    (let ((*inclusion-table* (make-hash-table :test 'equal))
          (*include-definitions* (create-scanners include-definitions))
          (*exclude-definitions* (create-scanners exclude-definitions))
          (*include-sources* (create-scanners include-sources))
          (*exclude-sources* (create-scanners exclude-sources)))
      (loop for entity in entities
            do (if (entity-explicitly-excluded-p entity)
                   (mark-excluded (foreign-entity-id entity))
                   (try-including-entity entity)))
      *inclusion-table*)))
