(cl:in-package :claw.cffi.c)


(defun common-prefix (strings)
  (if (> (length strings) 1)
      (let* ((sorted-strings (sort strings #'string<))
             (first (aref sorted-strings 0))
             (last (aref sorted-strings (1- (length sorted-strings))))
             (mismatch-idx (mismatch first last)))
        (if mismatch-idx
            (if-let ((hyphenated-prefix-idx (position #\- first :from-end t
                                                                :end mismatch-idx)))
              (subseq first 0 (1+ hyphenated-prefix-idx))
              "")
            ""))
      ""))


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
             (<= (abs (- log (truncate log))) long-float-epsilon))))
    (loop for (key . value) in (claw.spec:foreign-enum-values entity)
          always (and (not (zerop value)) (power-of-2-p value)))))


(defmethod generate-binding ((entity claw.spec:foreign-enum) &key name)
  (let* ((name (or name
                   (unless (emptyp (claw.spec:foreign-entity-name entity))
                     (c-name->lisp (claw.spec:foreign-entity-name entity) :type))))
         (values (loop for (key . value) in (claw.spec:foreign-enum-values entity)
                       collect (cons (c-name->lisp key :enum) value))))
    (if name
        (progn
          (export-symbol name)
          `((,(if (and *recognize-bitfields-p* (bitfieldp entity))
                  'cffi:defbitfield
                  'cffi:defcenum)
             ,name
             ,@(loop for (key . value) in (if *trim-enum-prefix-p*
                                              (trim-enum-prefix values)
                                              values)
                     collect (list (make-keyword key) value)))))
        `(,@(loop for (key . value) in values
                  append (expand-constant key value))))))
