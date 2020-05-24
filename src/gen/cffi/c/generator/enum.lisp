(cl:in-package :claw.cffi.c)


(defun trim-enum-prefix (key-values)
  (let ((prefix (common-prefix (map 'vector (compose #'string #'car) key-values))))
    (flet ((trim-key (key-value)
             (let ((key (car key-value))
                   (value (cdr key-value)))
               (cons (format-symbol (symbol-package key) "~A"
                                    (subseq (string key) (length prefix)))
                     value))))
      (mapcar #'trim-key key-values))))


(defun bitfieldp (entity)
  (flet ((power-of-2-p (value)
           (let ((log (log value 2)))
             (<= (abs (- log (truncate log))) single-float-epsilon))))
    (loop for (key . value) in (claw.spec:foreign-enum-values entity)
          always (and (not (zerop value)) (power-of-2-p value)))))


(defmethod generate-binding ((entity claw.spec:foreign-enum) &key name)
  (let* ((name (or name
                   (unless (emptyp (claw.spec:foreign-entity-name entity))
                     (c-name->lisp (claw.spec:foreign-entity-name entity) :type))))
         (values (loop for (key . value) in (claw.spec:foreign-enum-values entity)
                       collect (cons (c-name->lisp key :enum) value))))
    (flet ((%generate-enum (name)
             (export-symbol name)
             `((,(if (and *recognize-bitfields-p* (bitfieldp entity))
                     'cffi:defbitfield
                     'cffi:defcenum)
                 ,name
                 ,@(loop for (key . value) in (if *trim-enum-prefix-p*
                                                  (trim-enum-prefix values)
                                                  values)
                         collect (list (make-keyword key) value))))))
      (cond
        (name (%generate-enum name))
        ((claw.spec:find-alias-for-type (claw.spec:foreign-entity-type entity) *spec*)
         (%generate-enum (entity-type->cffi entity)))
        (t `(,@(loop for (key . value) in values
                     append (expand-constant key value))))))))
