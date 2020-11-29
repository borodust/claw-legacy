(cl:in-package :claw.generator.common)


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
             (and (realp log)
                  (<= (abs (- log (truncate log))) single-float-epsilon)))))
    (loop for (nil . value) in (claw.spec:foreign-enum-values entity)
          always (and (not (zerop value)) (power-of-2-p value)))))


(defmethod generate-binding ((generator generator) (entity claw.spec:foreign-enum) &key name)
  (let* ((name (or name
                   (unless (emptyp (claw.spec:foreign-entity-name entity))
                     (c-name->lisp (claw.spec:format-full-foreign-entity-name entity) :type))))
         (values (loop for (key . value) in (claw.spec:foreign-enum-values entity)
                       collect (cons (c-name->lisp key :enum) value))))
    (flet ((%generate-enum (name)
             (let ((enum-type (claw.spec:foreign-enum-type entity)))
               (check-entity-known enum-type)
               (export-symbol name)
               `((,(if (and *recognize-bitfields-p* (bitfieldp entity))
                       'cffi:defbitfield
                       'cffi:defcenum)
                  (,name ,(entity->cffi-type enum-type))
                  ,(claw.spec:format-foreign-location (claw.spec:foreign-entity-location entity))
                  ,@(if-let (pairs (loop for (key . value) in (if *trim-enum-prefix-p*
                                                                  (trim-enum-prefix values)
                                                                  values)
                                         collect (list (make-keyword key) value)))
                      pairs
                      '(:empty)))))))
      (cond
        (name
         (%generate-enum name))
        ((find-alias-for-entity entity)
         (%generate-enum (c-name->lisp (claw.spec:foreign-entity-id entity))))
        (t `(,@(loop for (key . value) in values
                     append (expand-constant key value))))))))


(defmethod dependablep ((entity claw.spec:foreign-enum))
  (declare (ignore entity))
  t)


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-enum))
  (list (claw.spec:foreign-enum-type entity)))
