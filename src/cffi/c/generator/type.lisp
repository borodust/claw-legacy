(cl:in-package :claw.cffi.c)


(defun anonymous-p (entity)
  (not (claw.spec:foreign-entity-name entity)))


(defun expand-constant (name value)
  (let ((name (format-symbol (or (package-name (symbol-package name))
                                 *package*) "+~A+" name)))
    (export-symbol name)
    `((defparameter ,name ,value))))


(defun typespec->c (typespec)
  (if (listp typespec)
      (destructuring-bind (kind type &optional count) typespec
        (ecase kind
          (:pointer (format nil "~A*" (typespec->c type)))
          (:array (if count
                      (format nil "~A[~A]" (typespec->c type) count)))
          (:enum (format nil "enum ~A" type))
          (:struct (format nil "struct ~A" type))
          (:union (format nil "union ~A" type))))
      typespec))


(defmethod generate-binding :around (entity &rest args &key)
  "Generates dependency bindings first"
  (labels ((%generate-bindings ()
             (loop for dep-typespec
                     in (claw.spec:foreign-entity-dependencies entity)
                   for dep = (claw.spec:find-foreign-entity dep-typespec *spec*)
                   if dep
                     append (generate-binding dep)
                   else
                     collect (apply #'generate-forward-declaration-from-typespec
                                    (ensure-list dep-typespec))))
           (%generate-depenencies ()
             (let* ((type (claw.spec:foreign-entity-type entity))
                    (*dependency-type-list* (list* type *dependency-type-list*)))
               (multiple-value-bind (existing-type present-p) (gethash type *visit-table*)
                 ;; to prevent redefinitions and stack overflow for recursive deps
                 (if present-p
                     ;; but if we get here through recursive definitions
                     ;; and type is still not defined we need to put forward decls
                     (when (and (not existing-type)
                              (not (gethash type *forward-declaration-table*))
                              (member type (rest *dependency-type-list*) :test #'equal))
                         (setf (gethash type *forward-declaration-table*) type)
                         (list (apply #'generate-forward-declaration entity args)))
                     (progn
                       ;; register a visit
                       (setf (gethash type *visit-table*) nil)
                       (let ((deps (%generate-bindings)))
                         (prog1 (append deps (call-next-method))
                           (setf (gethash type *visit-table*) entity)))))))))
    (if (or (typep entity 'claw.spec:foreign-constant)
            (typep entity 'claw.spec:foreign-extern))
        (call-next-method)
        (%generate-depenencies))))
