(cl:in-package :claw.generator.common)


(define-condition unknown-entity-condition (serious-condition)
  ((entity :initarg :entity
           :initform (error ":entity missing")
           :reader unknown-entity-of)))


(defun signal-unknown-entity (entity)
  (error (make-condition 'unknown-entity-condition :entity entity)))


(defun check-entity-known (entity)
  (when (or (typep entity 'claw.spec:foreign-entity-parameter)
            (claw.spec:foreign-entity-unknown-p entity)
            (and (claw.spec:foreign-parameterizable-p entity)
                 (claw.spec:foreign-entity-parameters entity)))
    (signal-unknown-entity entity))
  (when (claw.spec:foreign-envelope-p entity)
    (check-entity-known (claw.spec:foreign-enveloped-entity entity)))
  entity)


(defgeneric list-required-systems (generator))
(defgeneric generate-binding (generator entity &key &allow-other-keys))
(defgeneric generate-forward-declaration (generator entity &key &allow-other-keys))


(defun parse-overrides (configuration)
  (loop with override-table = (make-hash-table :test 'equal)
        for (cffi-type new-type) in configuration
        do (setf (gethash cffi-type override-table) new-type)
        finally (return override-table)))


(defun find-alias-for-entity (entity)
  (when (claw.spec:foreign-identified-p entity)
    (loop with entity-id = (claw.spec:foreign-entity-id (claw.spec:unwrap-foreign-entity entity))
          for value in *entities*
          for unqualified = (claw.spec:unwrap-foreign-entity value)
          when (and (typep value 'claw.spec:foreign-alias)
                    (claw.spec:foreign-identified-p unqualified)
                    (equal entity-id (claw.spec:foreign-entity-id unqualified)))
            do (return value))))


(defun merge-hash-table (destination source)
  (loop for key being the hash-key of source
        unless (gethash key destination)
          do (setf (gethash key destination) (gethash key source))))


(defun call-shielded-from-unknown (lambda &optional on-error)
  (flet ((return-nil (condi)
           (declare (ignore condi))
           (return-from call-shielded-from-unknown (when on-error
                                                     (funcall on-error)))))
    (handler-bind ((unknown-entity-condition #'return-nil))
      ;; this mental code is to avoid polluting global tables
      ;; when bindings are only partially generated due to unrecognized entities
      (let ((local-adapted-table (copy-hash-table *adapted-function-table*))
            (local-visit-table (copy-hash-table *visit-table*))
            (local-export-table (copy-hash-table *export-table*)))
        (prog1 (let ((*visit-table* local-visit-table)
                     (*export-table* local-export-table)
                     (*adapted-function-table* local-adapted-table))
                 (funcall lambda))
          (merge-hash-table *visit-table* local-visit-table)
          (merge-hash-table *export-table* local-export-table)
          (merge-hash-table *adapted-function-table* local-adapted-table))))))



(defclass bindings ()
  ((definitions :initarg :definitions :reader claw.wrapper:bindings-definition)
   (exported-symbols :initarg :exported-symbols)
   (required-systems :initarg :required-systems :reader claw.wrapper:bindings-required-systems)
   (required-packages :initarg :required-packages :reader claw.wrapper:bindings-required-packages)))


(defmethod claw.wrapper:unexport-bindings ((this bindings))
  (with-slots (exported-symbols) this
    (loop for sym in exported-symbols
          for sym-package = (symbol-package sym)
          when (find-symbol (symbol-name sym) sym-package)
            do (unexport sym sym-package))))


(defmethod claw.wrapper:reexport-bindings ((this bindings))
  (with-slots (exported-symbols) this
    (loop for sym in exported-symbols
          for sym-package = (symbol-package sym)
          when (find-symbol (symbol-name sym) sym-package)
            do (export sym sym-package))))


(defun explode-library-definition (generator language wrapper configuration)
  (declare (ignore language))
  (let ((entities (claw.wrapper:wrapper-entities wrapper)))
    (destructuring-bind (&key in-package
                           symbolicate-names
                           trim-enum-prefix
                           with-adapter
                           override-types
                           recognize-bitfields
                           recognize-arrays
                           recognize-strings
                           (inline-functions t)
                           (ignore-entities (constantly nil))
                           use-float-features)
        configuration
      (let ((in-package (eval in-package))
            (*trim-enum-prefix-p* (eval trim-enum-prefix))
            (*adapter* (when with-adapter
                         (destructuring-bind (adapter-kind &key ((:path adapter-path))
                                                             extract-pointers)
                             (ensure-list with-adapter)
                           (let ((adapter-path (or (eval adapter-path) "adapter.c"))
                                 (extract-pointers (loop for regex in extract-pointers
                                                         collect (eval regex))))
                             (ecase (eval adapter-kind)
                               (:static (make-static-adapter wrapper
                                                             adapter-path
                                                             extract-pointers
                                                             in-package))
                               (:dynamic (make-dynamic-adapter wrapper
                                                               adapter-path
                                                               extract-pointers
                                                               in-package)))))))
            (*export-table* (make-hash-table))
            (*forward-declaration-table* (make-hash-table :test 'equal))
            (*visit-table* (make-hash-table :test 'equal))
            (*adapted-function-table* (make-hash-table :test 'equal))
            (*override-table* (parse-overrides override-types))
            (*recognize-strings-p* recognize-strings)
            (*recognize-bitfields-p* recognize-bitfields)
            (*recognize-arrays-p* recognize-arrays)
            (*inline-functions* inline-functions)
            (*use-float-features* use-float-features)
            (*float-features-requested* nil)
            (rename-symbols (eval (parse-renaming-pipeline symbolicate-names)))
            (bindings (list))
            (*entities* (remove-if (eval ignore-entities)
                                   (stable-sort entities #'string<
                                                :key #'claw.spec:foreign-entity-id))))
        (uiop:ensure-package in-package :use nil)
        (with-symbol-renaming (in-package rename-symbols)
          (loop for entity in *entities*
                do (let ((*dependency-type-list* nil)
                         (generated (call-shielded-from-unknown
                                     (lambda () (generate-binding generator entity)))))
                     (loop for bing in generated
                           do (push bing bindings))))
          (when *adapter*
            (generate-adapter-file *adapter*))
          (make-instance
           'bindings
           :definitions `(,@(nreverse bindings)
                          (eval-when (:load-toplevel :compile-toplevel :execute)
                            ,@(loop for symbol being the hash-key of *export-table*
                                    collect `(export ',symbol ,(package-name (symbol-package symbol)))))
                          ,@(when *adapter*
                              (expand-adapter-routines *adapter* wrapper)))
           :exported-symbols (hash-table-keys *export-table*)
           :required-systems (append (list-required-systems generator)
                                     (when (and *use-float-features* *float-features-requested*)
                                       (list :float-features)))
           :required-packages (list in-package)))))))


(defclass generator () ())
