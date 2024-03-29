(cl:in-package :claw.spec)

(declaim (special *library-specification*))

(defvar *spec-path-override* nil)

(defmacro with-overriden-spec-path ((spec-path) &body body)
  `(let ((*spec-path-override* ,spec-path))
     ,@body))

(defgeneric optimize-entity (entity))

(defgeneric entity-constant-p (entity)
  (:method (entity) (declare (ignore entity)) nil))


(defclass library-specification ()
  ((entity-table :initform (make-hash-table :test 'equal))
   (symbol-order :initform (list))
   (constants :initform (list))))


(defgeneric parse-form (form tag)
  (:documentation "Parse FORM tagged as TAG; specialize on (eql 'symbol)"))


(defgeneric compose-form (type)
  (:documentation "Compose FORM from TYPE"))


(defun library-symbol-order (library)
  (append (sort (copy-list (slot-value library 'constants)) #'string<)
          (reverse (slot-value library 'symbol-order))))


(defmacro do-foreign-entities ((entity library) &body body)
  (with-gensyms (entity-name)
    (once-only (library)
      `(loop for ,entity-name in (library-symbol-order ,library)
             do (let ((,entity (gethash ,entity-name
                                        (slot-value ,library 'entity-table))))
                  (progn ,@body))))))


(defun read-library-specification (stream)
  (loop with *library-specification* = (make-instance 'library-specification)
        for form in (decode-json stream)
        for name = (aval :name form)
        for location = (aval :location form)
        do (parse-form form (aval :tag form))
        finally (return *library-specification*)))


(defun compose-forms ()
  (with-slots (entity-table) *library-specification*
    (loop for symbol in (library-symbol-order *library-specification*)
          collect (compose-form (gethash symbol entity-table)))))


(defun write-library-specification (specification stream)
  (let ((*library-specification* specification))
    (format stream "[~%")
    (loop for form on (compose-forms)
          collect (encode-json (first form) stream)
          when (rest form)
            do (format stream ",~%"))
    (format stream "~&]")))


(defun preserve-order (name &optional (library-specification *library-specification*)
                              constant-p)
  (with-slots (entity-table symbol-order constants) library-specification
    (multiple-value-bind (entity reserved-p) (gethash name entity-table)
      (declare (ignore entity))
      (unless reserved-p
        (if constant-p
            (push name constants)
            (push name symbol-order))
        (setf (gethash name entity-table) nil)))))


(defun register-foreign-entity (name type &optional (library-specification
                                                     *library-specification*))
  (with-slots (entity-table) library-specification
    (if-let ((entity (gethash name entity-table)))
      (values entity nil)
      (values (progn
                (preserve-order name library-specification (entity-constant-p type))
                (setf (gethash name entity-table) type))
              t))))


(defun find-foreign-entity (name
                            &optional (library-specification *library-specification*))
  (with-slots (entity-table) library-specification
    (gethash name entity-table)))


;;;
;;; LIBRARY SPECIFICATION CONTAINER
;;;
(defclass library-descriptor ()
  ((platform-table :initform (make-hash-table :test 'equal))))


(defun add-library-specification (library-container platform spec)
  (with-slots (platform-table) library-container
    (setf (gethash platform platform-table) spec)))


(defun map-library-specification (library-container consumer)
  (with-slots (platform-table) library-container
    (maphash consumer platform-table)))


(defmacro map-platforms ((platform specification library) &body body)
  `(map-library-specification ,library (lambda (,platform ,specification) ,@body)))


(defun find-specification-for-platform (library-container arch)
  (with-slots (platform-table) library-container
    (gethash arch platform-table)))


(defun find-specification-for-current-platform (library-container)
  (with-slots (platform-table) library-container
    (gethash (local-platform) platform-table)))


(defgeneric try-including-entity (entity))


(defun make-inclusion-table (spec
                             include-definitions
                             include-sources
                             exclude-definitions
                             exclude-sources)
  (let ((*library-specification* spec)
        (*inclusion-table* (make-hash-table :test 'equal))
        (*include-definitions* include-definitions)
        (*exclude-definitions* exclude-definitions)
        (*include-sources* include-sources)
        (*exclude-sources* exclude-sources))
    (do-foreign-entities (entity spec)
      (if (entity-explicitly-excluded-p entity)
          (mark-excluded (foreign-entity-type entity))
          (try-including-entity entity)))
    *inclusion-table*))


(defun optimize-specification (spec
                               include-definitions
                               include-sources
                               exclude-definitions
                               exclude-sources)
  (let* ((*library-specification* spec)
         (*inclusion-table* (make-inclusion-table *library-specification*
                                                  include-definitions
                                                  include-sources
                                                  exclude-definitions
                                                  exclude-sources))
         (optimized-spec (make-instance 'library-specification)))
    (do-foreign-entities (entity *library-specification*)
      (when-let ((optimized-entity (optimize-entity entity)))
        (let ((entity-type (foreign-entity-type optimized-entity)))
          (register-foreign-entity entity-type optimized-entity optimized-spec))))
    optimized-spec))


(defun describe-foreign-library (name headers &key spec-path
                                                arch-excludes
                                                arch-includes
                                                includes
                                                framework-includes
                                                language
                                                standard
                                                include-definitions
                                                include-sources
                                                exclude-definitions
                                                exclude-sources)
  (let ((spec-container (make-instance 'library-descriptor)))
    (labels ((read-spec (stream)
               (optimize-specification (read-library-specification stream)
                                       include-definitions
                                       include-sources
                                       exclude-definitions
                                       exclude-sources))
             (spec-path (arch)
               (let ((spec-path (or *spec-path-override* spec-path)))
                 (when spec-path
                   (concatenate 'string
                                (namestring spec-path)
                                (pathname-name name)
                                "." arch ".spec"))))
             (arch-excluded-p (arch)
               (member arch arch-excludes :test #'string=))
             (write-spec (arch spec)
               (when-let ((output-path (spec-path arch)))
                 (ensure-directories-exist output-path)
                 (with-open-file (output-stream output-path :direction :output
                                                            :if-exists :supersede)
                   (write-library-specification spec output-stream))
                 spec))
             (add-spec (arch spec-path)
               (with-open-file (stream spec-path)
                 (add-library-specification spec-container
                                            arch
                                            (read-spec stream))))
             (list-arches ()
               (if (uiop:featurep :claw-local-only)
                   (list (local-platform))
                   (remove-if #'arch-excluded-p (or arch-includes +known-platforms+))))
             (generate-and-add-missing-specs (missing-arches)
               (do-encoded-library-specifications ((stream arch) headers
                                                   :arch-includes missing-arches
                                                   :includes includes
                                                   :framework-includes framework-includes
                                                   :language language
                                                   :standard standard)
                 (let ((spec (read-spec stream)))
                   (add-library-specification spec-container arch spec)
                   (write-spec arch spec)))))
      (loop for arch in (list-arches)
            for spec-path = (probe-file (spec-path arch))
            if (and (not (uiop:featurep :claw-rebuild-spec)) spec-path)
              do (add-spec arch spec-path)
            else
              collect arch into missing-arches
            finally (generate-and-add-missing-specs missing-arches))
      (values spec-container (loop for arch in (list-arches)
                                   maximizing (file-write-date (spec-path arch)))))))
