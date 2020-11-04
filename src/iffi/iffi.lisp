(cl:in-package :iffi)


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun find-quoted (value)
    (cond
      ((keywordp value) value)
      ((and (listp value) (eq 'quote (first value))) (second value))))


  (defmacro initialize-iffi ()
    (let ((alloc-name (cond
                        ((cffi:foreign-symbol-pointer "aligned_alloc") "aligned_alloc")
                        ((cffi:foreign-symbol-pointer "_aligned_malloc") "_aligned_malloc")
                        (t (error "Aligned memory allocation function not found. No C std library linked?")))))
      `(progn
         (declaim (inline iffi::aligned-alloc))
         (cffi:defcfun (,alloc-name iffi::aligned-alloc) :pointer
           (byte-alignment :size)
           (byte-size :size))))))


(define-condition intricate-condition (serious-condition)
  (handle))

(defvar *function-table* (make-hash-table :test 'equal))

(defvar *function-pointer-extractor-table* (make-hash-table :test 'equal))

(defvar *intricate-table* (make-hash-table))

(defvar *doc-table* (make-hash-table))

(defvar *record-table* (make-hash-table))


(initialize-iffi)


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
  (loop for (type . rest) on args by #'cddr
        unless rest
          do (error "Odd number of arguments: ~A" args)
        collect type into arg-types
        collect (first rest) into arg-values
        finally (return (if-let ((fu (apply #'intricate-function name arg-types)))
                          (apply fu arg-values)
                          (error "Intricate function with signature ~A ~A not found" name arg-types)))))


(define-compiler-macro intricate-funcall (&whole whole name &rest args)
  (loop for (type . rest) on args by #'cddr
        unless rest
          do (error "Odd number of arguments: ~A" args)
        collect (if-let ((actual (find-quoted type)))
                  actual
                  (progn
                    (warn "Passing argument types dynamically is discouraged. Invocation: ~A " whole)
                    (return whole)))
          into arg-types
        collect (first rest) into arg-values
        finally (return
                  (if-let ((quoted (find-quoted name)))
                    (if-let ((function (apply #'intricate-function quoted arg-types)))
                      `(,function ,@arg-values)
                      (progn
                        (warn "Function ~A is not defined for parameters ~A " quoted arg-types)
                        whole))
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
           (declaim (inline ,cfun-name))
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


(defclass intricate-field ()
  ((name :initarg :name :reader name-of)
   (getter :initarg :getter :reader getter-of)
   (setter :initarg :setter :reader setter-of)))


(defclass intricate-record ()
  ((name :initarg :name :reader name-of)
   (size-reporter :initarg :size-reporter :initform nil  :reader size-reporter-of)
   (alignment-reporter :initarg :alignment-reporter :initform nil :reader alignment-reporter-of)
   (constructor :initarg :constructor :initform nil :reader constructor-of)
   (destructor :initarg :destructor :initform nil :reader destructor-of)
   (field-map :initarg :field-map :initform (make-hash-table))))


(defun serialize-intricate-record (record)
  (with-slots (field-map) record
    (list :name (name-of record)
          :size (size-reporter-of record)
          :alignment (alignment-reporter-of record)
          :constructor (constructor-of record)
          :destructor (destructor-of record)
          :fields (loop for field being the hash-value of field-map
                        collect (list :name (name-of field)
                                      :getter (getter-of field)
                                      :setter (setter-of field))))))


(defun deserialize-intricate-record (record-data)
  (destructuring-bind (&key name size alignment constructor destructor fields)
      record-data
    (let ((field-map (loop with map = (make-hash-table)
                           for field in fields
                           do (destructuring-bind (&key name setter getter)
                                  field
                                (setf (gethash name map)
                                      (make-instance 'intricate-field :name name
                                                                      :getter getter
                                                                      :setter setter)))
                           finally (return map))))
      (make-instance 'intricate-record :name name
                                       :size-reporter size
                                       :alignment-reporter alignment
                                       :constructor constructor
                                       :destructor destructor
                                       :field-map field-map))))


(defun find-intricate-field (record field-name)
  (with-slots (field-map) record
    (gethash field-name field-map)))


(defun register-intricate-record (record-data)
  (let ((record (deserialize-intricate-record record-data)))
    (setf (gethash (name-of record) *record-table*) record)))


(defun find-intricate-record (name)
  (gethash name *record-table*))


(defun expand-record-field (record field)
  (destructuring-bind (field-name type &key
                                         setter
                                         getter
                       &allow-other-keys)
      field
    (when-let ((record-field (find-intricate-field record field-name)))
      (append
       (when setter
         `((cffi:defcfun (,setter ,(setter-of record-field)) :void
             (this :pointer)
             (value ,type))))
       (when getter
         `((cffi:defcfun (,getter ,(getter-of record-field)) ,type
             (this :pointer))))))))


(defun expand-record (record size-reporter alignment-reporter fields)
  `((cffi:defcfun (,size-reporter ,(size-reporter-of record))
        :unsigned-long-long)
    (cffi:defcfun (,alignment-reporter ,(alignment-reporter-of record))
        :unsigned-long-long)
    ,@(loop for field in fields
            append (expand-record-field record field))))


(defun make-field-map (record-name fields)
  (loop with table = (make-hash-table)
        for field in fields
        do (destructuring-bind (field-name type &key
                                                  setter
                                                  getter
                                                  documentation)
               field
             (declare (ignore type documentation))
             (let ((record-field (make-instance
                                  'intricate-field
                                  :name field-name
                                  :setter (when setter
                                            (format-symbol
                                             (symbol-package field-name)
                                             "~A$~A$~A"
                                             'iffi-set record-name field-name))
                                  :getter (when getter
                                            (format-symbol
                                             (symbol-package field-name)
                                             "~A$~A$~A"
                                             'iffi-get record-name field-name)))))
               (setf (gethash field-name table) record-field)))
        finally (return table)))


(defmacro defirecord (name-and-opts superclasses &body fields)
  (declare (ignore superclasses))
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (multiple-value-bind (doc fields)
        (if (stringp (first fields))
            (values (first fields) (rest fields))
            (values nil fields))
      (destructuring-bind (&key size-reporter alignment-reporter constructor destructor)
          opts
        (let ((record (make-instance 'intricate-record
                                     :name name
                                     :constructor constructor
                                     :destructor destructor
                                     :size-reporter (format-symbol (symbol-package name)
                                                                   "~A$~A" 'iffi-sizeof name)
                                     :alignment-reporter (format-symbol (symbol-package name)
                                                                        "~A$~A" 'iffi-alignof name)
                                     :field-map (make-field-map name fields))))
          (prog1 `(progn
                    (cffi:defctype ,name :void ,doc)
                    ,@(when (and size-reporter alignment-reporter)
                        (expand-record record size-reporter alignment-reporter fields))
                    (register-intricate-record ',(serialize-intricate-record record)))))))))


(defmacro defiunion (name-and-opts &body fields)
  `(defirecord ,name-and-opts nil ,@fields))


(defmacro defistruct (name-and-opts superclasses &body fields)
  `(defirecord ,name-and-opts ,superclasses ,@fields))


(defmacro deficlass (name-and-opts superclasses &body fields)
  `(defirecord ,name-and-opts ,superclasses ,@fields))


(defun intricate-size (name)
  (if-let ((intricate (find-intricate-record name)))
    (funcall (size-reporter-of intricate))
    (cffi:foreign-type-size name)))


(define-compiler-macro intricate-size (&whole whole name)
  (if-let (quoted (find-quoted name))
    (if-let ((intricate (find-intricate-record quoted)))
      `(,(size-reporter-of intricate))
      whole)
    whole))


(defun intricate-alignment (name)
  (if-let ((intricate (find-intricate-record name)))
    (funcall (alignment-reporter-of intricate))
    (cffi:foreign-type-size name)))


(define-compiler-macro intricate-alignment (&whole whole name)
  (if-let ((quoted (find-quoted name)))
    (if-let ((intricate (find-intricate-record quoted)))
      `(,(alignment-reporter-of intricate))
      whole)
    whole))


(defun intricate-alloc (name &optional (count 1))
  (if-let ((intricate (find-intricate-record name)))
    (aligned-alloc (intricate-alignment name) (* (intricate-size name) count))
    (cffi:foreign-alloc name :count count)))


(define-compiler-macro intricate-alloc (&whole whole name &optional (count 1))
  (if-let ((quoted (find-quoted name)))
    (if-let ((intricate (find-intricate-record quoted)))
      `(aligned-alloc (intricate-alignment ,name) (* (intricate-size ,name) ,count))
      whole)
    whole))


(declaim (inline intricate-free))
(defun intricate-free (ptr)
  (cffi-sys:foreign-free ptr))


(defmacro with-field-setter ((field-setter type-name slot-name) &body body)
  (with-gensyms (intricate field)
    `(if-let ((,intricate (find-intricate-record ,type-name)))
       (if-let ((,field (find-intricate-field ,intricate ,slot-name)))
         (if-let ((,field-setter (setter-of ,field)))
           (progn ,@body)
           (error "Writer not found for slot ~A in type ~A" ,slot-name ,type-name))
         (error "Field ~A not found in type ~A" ,slot-name ,type-name))
       (error "Metadata for intricate record ~A not found" ,type-name))))


(defun set-intricate-slot-value (instance type-name slot-name value)
  (with-field-setter (field-setter type-name slot-name)
    (funcall field-setter instance value))
  value)


(define-compiler-macro set-intricate-slot-value (&whole whole instance type-name slot-name value)
  (let ((quoted-type-name (find-quoted type-name))
        (quoted-slot-name (find-quoted slot-name)))
    (if (and quoted-type-name quoted-slot-name)
        (with-field-setter (field-setter quoted-type-name quoted-slot-name)
          (once-only (value)
            `(progn ,value (,field-setter ,instance ,value))))
        whole)))


(define-setf-expander intricate-slot-value (instance type-name slot-name &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion instance env)
    (declare (ignore setter newval))
    (with-gensyms (store)
      (let ((quoted-type-name (find-quoted type-name))
            (quoted-slot-name (find-quoted slot-name)))
        (values dummies
                vals
                `(,store)
                (if (and quoted-type-name quoted-slot-name)
                    `(set-intricate-slot-value ,instance ',quoted-type-name ',quoted-slot-name ,store)
                    `(set-intricate-slot-value ,instance ,type-name ,slot-name ,store))
                `(intricate-slot-value ,getter ,type-name ,slot-name))))))


(defun intricate-slot-value (instance type-name slot-name)
  (if-let ((intricate (find-intricate-record type-name)))
    (if-let ((field (find-intricate-field intricate slot-name)))
      (if-let ((getter (getter-of field)))
        (funcall getter instance)
        (error "Reader not found for slot ~A in type ~A" slot-name type-name))
      (error "Field ~A not found in type ~A" slot-name type-name))
    (error "Metadata for intricate record ~A not found" type-name)))


(define-compiler-macro intricate-slot-value (&whole whole instance type-name slot-name)
  (let ((quoted-type-name (find-quoted type-name))
        (quoted-slot-name (find-quoted slot-name)))
    (if (and quoted-type-name quoted-slot-name)
        (if-let ((intricate (find-intricate-record quoted-type-name)))
          (if-let ((field (find-intricate-field intricate quoted-slot-name)))
            (if-let ((getter (getter-of field)))
              `(,getter ,instance)
              (error "Reader not found for slot ~A in type ~A" slot-name type-name))
            (error "Field ~A not found in type ~A" slot-name type-name))
          (error "Metadata for intricate record ~A not found" type-name))
        whole)))


(defun intricate-signal-handler (ptr)
  (lambda (condi)
    (intricate-free ptr)
    (signal condi)))


(defun make-intricate-instance (name &rest args)
  (let* ((record (find-intricate-record name))
         (ptr (intricate-alloc name)))
    (unless record
      (error "Record with name ~A not found" name))
    (if-let ((ctor (constructor-of record)))
      (handler-case
          (apply (constructor-of record) `(:pointer ,name) ptr args)
        (serious-condition (condi) (intricate-free ptr) (signal condi)))
      (error "Constructor not found for record ~A" name))
    ptr))


(define-compiler-macro make-intricate-instance (&whole whole name &rest args)
  (let* ((quoted-name (find-quoted name))
         (record (find-intricate-record quoted-name))
         (ctor (and record (constructor-of record))))
    (when quoted-name
      (cond
        ((not record) (warn "Record with name ~A not found" quoted-name))
        ((not ctor) (warn "Constructor not found for record ~A" quoted-name))))
    (if ctor
        (with-gensyms (ptr condi)
          `(let ((,ptr (intricate-alloc ',quoted-name)))
             (handler-case
                 (,ctor '(:pointer ,quoted-name) ,ptr ,@args)
               (serious-condition (,condi) (intricate-free ,ptr) (signal ,condi)))
             ,ptr))
        whole)))


(defun destroy-intricate-instance (name instance)
  (let ((record (find-intricate-record name)))
    (unless record
      (error "Record with name ~A not found" name))
    (if-let ((dtor (destructor-of record)))
      (funcall dtor `(:pointer ,name) instance)
      (error "Destructor for record ~A not found" name)))
  (intricate-free instance))


(define-compiler-macro destroy-intricate-instance (&whole whole name instance)
  (let* ((quoted-name (find-quoted name))
         (record (find-intricate-record quoted-name))
         (dtor (and record (destructor-of record))))
    (if dtor
        (once-only (instance)
          `(progn
             (,dtor '(:pointer ,quoted-name) ,instance)
             (intricate-free ,instance)))
        whole)))


(defmacro with-intricate-instance ((var name &rest constructor-args) &body body)
  `(let ((,var (make-intricate-instance ',name ,@constructor-args)))
     (unwind-protect
          (progn ,@body)
       (destroy-intricate-instance ',name ,var))))


(defmacro with-intricate-instances ((&rest declarations) &body body)
  (labels ((expand-with-intricate-instances (declarations body)
             (if declarations
                 `((with-intricate-instance ,(first declarations)
                     ,@(expand-with-intricate-instances (rest declarations) body)))
                 `(,@body))))
    (first (expand-with-intricate-instances declarations body))))
