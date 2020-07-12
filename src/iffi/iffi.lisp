(cl:in-package :claw.iffi)


(define-condition intricate-condition (serious-condition)
  (handle))


(defvar *function-table* (make-hash-table :test 'equal))

(defvar *function-pointer-extractor-table* (make-hash-table :test 'equal))

(defvar *intricate-table* (make-hash-table))

(defvar *doc-table* (make-hash-table))


(defun (setf intricate-documentation) (docstring name &rest arg-types)
  (flet ((format-docs ()
           (with-output-to-string (out)
             (let ((*print-case* :downcase))
               (loop for (types . doc) in (gethash name *doc-table*)
                     do (format out "(")
                        (prin1 name out)
                        (loop for type in types
                              do (format out "~&  '") (prin1 type out))
                        (format out ")")
                        (format out "~&~A~&~%" doc))))))
    (setf (assoc-value (gethash name *doc-table*) arg-types :test #'equal) docstring
          (documentation (symbol-function name) t) (format-docs)))
  docstring)


(defun (setf intricate-function) (value name &rest arg-types)
  (setf (gethash (list* name arg-types) *function-table*) value))


(defun intricate-function (name &rest arg-types)
  (gethash (list* name arg-types) *function-table*))


(defun (setf intricate-function-pointer-extractor) (value name &rest arg-types)
  (setf (gethash (list* name arg-types) *function-pointer-extractor-table*) value))


(defun intricate-function-pointer (name &rest arg-types)
  (when-let ((extractor (gethash (list* name arg-types) *function-pointer-extractor-table*)))
    (funcall extractor)))


(defun intricate-funcall (name &rest args)
  (loop for (type value) on args by #'cddr
        collect type into arg-types
        collect value into arg-values
        finally (return (if-let ((fu (apply #'intricate-function name arg-types)))
                          (apply fu arg-values)
                          (error "Intricate function with signature ~A ~A not found" name arg-types)))))


(define-compiler-macro intricate-funcall (&whole whole name &rest args)
  (loop for (type value) on args by #'cddr
        collect (eval type) into arg-types
        collect value into arg-values
        finally (return (if-let ((function (apply #'intricate-function (eval name) arg-types)))
                          `(,function ,@arg-values)
                          whole))))


(defun expand-intricate-function-body (name arguments)
  `(intricate-funcall ',name ,@arguments))


(defmacro defifun (name-and-options result-type &body configuration)
  (destructuring-bind (mangled name &rest opts) (ensure-list name-and-options)
    (let (doc
          arg-config
          cffi-opts
          pointer-extractor)
      (if (stringp (first configuration))
          (setf doc (first configuration)
                arg-config (rest configuration))
          (setf arg-config configuration))
      (loop for (name value) on opts by #'cddr
            do (case name
                 (:pointer-extractor (setf pointer-extractor value))
                 (t (setf cffi-opts (list* name value cffi-opts)))))
      (let* ((arg-types (loop for arg in arg-config
                              for type = (if (eq arg '&rest)
                                             '&rest
                                             (second arg))
                              append `(',type)))
             (intricately-defined (gethash name *intricate-table*))
             (cfun-name (format-symbol (symbol-package name) "~A~A$~A" 'iffi-cfun$ name mangled)))
        `(progn
           (cffi:defcfun (,mangled ,cfun-name ,@(nreverse cffi-opts)) ,result-type ,@configuration)
           (setf (intricate-function ',name ,@arg-types) ',cfun-name)
           ,@(when pointer-extractor
               (let ((extractor-cfun-name (symbolicate cfun-name '$pointer-extractor)))
                 `((cffi:defcfun (,pointer-extractor ,extractor-cfun-name) :pointer)
                   (setf (intricate-function-pointer-extractor ',name ,@arg-types) ',extractor-cfun-name))))
           ,@(when (or (not intricately-defined)
                       (equal intricately-defined arg-types))
               (setf (gethash name *intricate-table*) arg-types)
               `((defun ,name (&rest args)
                   ,@(when doc
                       `(,doc))
                   (apply #'intricate-funcall ',name args))
                 (define-compiler-macro ,name (&rest arguments)
                   (expand-intricate-function-body ',name arguments))))
           ,@(when doc
               `((setf (intricate-documentation ',name ,@arg-types) ,doc))))))))


(defmacro defiunion (name-and-opts &body fields)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (destructuring-bind (&key size) opts
      `(cffi:defcunion (,name ,@(append (when size
                                          `(:size ,size))))))))


(defmacro defistruct (name-and-opts superclasses &body fields)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    `(cffi:defcstruct (,name ,@opts))))


(defmacro deficlass (name-and-opts superclasses &body fields)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (destructuring-bind (&key size) opts
      `(cffi:defcstruct (,name ,@(append (when size
                                           `(:size ,size))))))))
