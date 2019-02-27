(cl:in-package :claw)

(defvar *local-only* nil)

(defvar *foreign-type-symbol-function* 'default-foreign-type-symbol)
(defvar *foreign-c-to-lisp-function* 'default-c-to-lisp)

(defvar *foreign-constant-excludes* nil)
(defvar *foreign-symbol-exceptions* nil)
(defvar *foreign-symbol-regex* nil)

(declaim (special *exported-foreign-record-list*
                  *exported-foreign-function-list*
                  *exported-foreign-extern-list*
                  *exported-foreign-constant-list*
                  *foreign-other-exports-list*
                  *foreign-alias-list*
                  *foreign-raw-constant-list*))
;; Collecting symbols

(defmacro collecting-symbols (&body body)
  `(let (*exported-foreign-record-list*
         *exported-foreign-function-list*
         *exported-foreign-extern-list*
         *exported-foreign-constant-list*
         *foreign-other-exports-list*
         *foreign-alias-list*
         *foreign-raw-constant-list*)
     ,@body))

;; Types and symbols

(defun apply-regexps (string regex-list)
  (loop for r in regex-list
        when (ppcre:scan-to-strings (car r) string)
          do (setf string (funcall (cdr r) string)))
  string)

(defun default-c-to-lisp (string)
  (let ((string (ppcre:regex-replace-all "([A-Z]+)([A-Z][a-z])" string "\\1_\\2")))
    (let ((string (ppcre:regex-replace-all "([a-z]+)([A-Z])" string "\\1_\\2")))
      (if (ppcre:all-matches "^(:_|_)" string)
          (nstring-upcase string)
          (nstring-upcase (nsubstitute #\- #\_ string))))))

(defun foreign-symbol-exception-p (string)
  (and *foreign-symbol-exceptions*
       (nth-value 1 (gethash string *foreign-symbol-exceptions*))))

(defun default-foreign-type-symbol (string type package)
  (let ((string (or (and *foreign-symbol-exceptions*
                         (gethash string *foreign-symbol-exceptions*))
                    (funcall *foreign-c-to-lisp-function*
                             (if *foreign-symbol-regex*
                                 (apply-regexps string *foreign-symbol-regex*)
                                 string)))))
    (if (eq #\: (aref string 0))
        (alexandria:make-keyword (subseq string 1))
        (cond
          ((eq type :cconst)
           (intern (format nil "+~A+" string) package))
          ((eq type :cenumfield)
           (alexandria:make-keyword string))
          ((eq type :cfield)
           (alexandria:make-keyword string))
          (t (intern string package))))))

(defun foreign-type-symbol (string type package)
  (if (string= "" string)
      (case type
        (:cparam (gensym "P"))
        (:cfield (gensym "FIELD-"))
        (otherwise (gensym "ANON-TYPE-")))
      (funcall *foreign-type-symbol-function* string type package)))

(defun make-record-ref (form)
  (alist-bind (tag name id) form
    (let (symbol-type)
      (cond
        ((string= tag ":struct")
         (setf tag :struct)
         (setf symbol-type :cstruct))
        ((string= tag ":union")
         (setf tag :union)
         (setf symbol-type :cunion))
        ((string= tag ":enum")
         (setf tag :enum)
         (setf symbol-type :cenum))
        (t (error "Unknown tag for MAKE-RECORD-REF: ~S" tag)))
      `((,tag (,(if (and (string= name "") (> id 0))
                    nil
                    (foreign-type-symbol name symbol-type *package*))
               ,@(when (and (string= name "") (> id 0))
                   `(:id ,id))))))))

(defun record-form-p (form)
  "Return whether FORM describes a struct or union"
  (let ((tag (aval :tag form)))
   (or (string= "struct" tag)
       (string= "union" tag)
       (string= ":struct" tag)
       (string= ":union" tag))))

(defun pointer*-to-record-form-p (form)
  "If `FORM` describes a type which is a record, or one or more levels
of pointer-to-record"
  (let ((tag (aval :tag form)))
    (if (string= ":pointer" tag)
        (pointer*-to-record-form-p (aval :type form))
        (record-form-p form))))

(defun pointer-alias-form-p (form)
  "If `FORM` is an alias to a pointer."
  (let ((tag (aval :tag form)))
    (cond
      ((string= tag "typedef") (pointer-alias-form-p (aval :type form)))
      ((string= tag ":pointer") t)
      (t nil))))

(defun maybe-add-constant (name location value &optional forcibly-include)
  (push (cons name value) *foreign-raw-constant-list*)
  (unless (included-p name *foreign-constant-excludes*)
    (let ((sym (foreign-type-symbol name :cconst *package*)))
      (when (or forcibly-include (finally-included-p name location))
        (pushnew sym *exported-foreign-constant-list*))
      `(defparameter ,sym ,value))))

;; Parsing

(defgeneric parse-type (form tag)
  (:documentation "Parse FORM describing a type, tagged by TAG.
Return the appropriate CFFI name."))

(defmethod parse-type (form tag)
  (list (foreign-type-symbol (aval :tag form) :ctype *package*)))

(defmethod parse-type :around (form tag)
  (etypecase tag
    (symbol (call-next-method))
    (string
     (parse-type form (if (eq #\: (aref tag 0))
                          (make-keyword (substr* (string-upcase tag) 1))
                          (intern (string-upcase tag) 'claw))))))

(defmethod parse-type (form (tag (eql :struct)))
  (make-record-ref form))

(defmethod parse-type (form (tag (eql :union)))
  (make-record-ref form))

(defmethod parse-type (form (tag (eql :enum)))
  (make-record-ref form))

(defmethod parse-type (form (tag (eql :pointer)))
  (alist-bind (type) form
    (alist-bind ((type-tag :tag)) type
      (cond
        ((or (string= ":char" type-tag)
             (string= ":unsigned-char" type-tag))
         '((:string)))
        (t `((:pointer ,@(parse-type type type-tag))))))))

(defmethod parse-type (form (tag (eql :function-pointer)))
  '((:pointer (:void))))

(defmethod parse-type (form (tag (eql :signed-char)))
  '(:char))

(defmethod parse-type (form (tage (eql :long-double)))
  '(long-double))

(defmethod parse-type (form (tag (eql :_bool)))
  (case (aval :bit-size form)
    ((8 nil) '(:unsigned-char))
    (32 '(:unsigned-int))))

(defmethod parse-type (form (tag (eql :array)))
  (alist-bind (type size) form
    `((:array ,@(parse-type type (aval :tag type)) ,size))))

(defmethod parse-type (form (tag (eql :bitfield)))
  (alist-bind (type width) form
    `(,@(parse-type type (aval :tag type))
      :bitfield-p t
      :bit-width ,width)))

(defun make-foreign-record-name (form type)
  (alist-bind (name id) form
    (let ((size (list :bit-size (aval :bit-size form)
                      :bit-alignment (aval :bit-alignment form)))
          (symbol-type (ecase type (:struct :cstruct) (:union :cunion))))
      (if (anonymous-p form)
          (list* nil :id id size)
          (let* ((sym (foreign-type-symbol name symbol-type *package*))
                 (descriptor `(,type (,sym))))
            (when (form-finally-included-p form)
              (pushnew descriptor *exported-foreign-record-list*))
            (list* sym size))))))

(defmethod parse-type (form (tag (eql 'struct)))
  (alist-bind (fields) form
    `((struct ,(make-foreign-record-name form :struct)
              ,@(parse-fields fields)))))

(defmethod parse-type (form (tag (eql 'union)))
  (alist-bind (fields) form
    `((union ,(make-foreign-record-name form :union)
             ,@(parse-fields fields)))))

(defmethod parse-type (form (tag (eql 'enum)))
  (alist-bind (name id fields) form
    `((enum ,(if (anonymous-p form)
                 (list nil :id id)
                 (foreign-type-symbol name :cenum *package*))
            ,@(parse-enum-fields fields)))))

(defgeneric parse-form (form tag &key &allow-other-keys)
  (:documentation "Parse FORM tagged as TAG; specialize on (eql 'symbol)"))

(defmethod parse-form :around (form tag &rest keys &key &allow-other-keys)
  (etypecase tag
    (symbol (call-next-method))
    (string
     (apply #'parse-form form (if (eq #\: (aref tag 0))
                                  (make-keyword (substr* (string-upcase tag) 1))
                                  (intern (string-upcase tag) 'claw))
            keys))))

(defmethod parse-form (form tag &key &allow-other-keys)
  (warn "Unhandled form: ~S for input:~%  ~S" tag form))

(defmethod parse-form (form (tag (eql 'typedef)) &key &allow-other-keys)
  (alist-bind (name type) form
    (let ((sym (foreign-type-symbol name :ctype *package*)))
      (when (form-finally-included-p form)
        (if (pointer*-to-record-form-p type)
            (pushnew sym *exported-foreign-record-list* :test #'equal)
            (if (pointer-alias-form-p type)
                (pushnew sym *foreign-alias-list* :test #'equal)
                (pushnew sym *foreign-other-exports-list*))))
      `(define-foreign-alias ',sym
         ,name
         ',@(parse-type type (aval :tag type))))))

(defun parse-fields (fields &optional (field-type :cfield))
  (loop for field in fields
        collect
        (alist-bind (name type bit-size bit-offset bit-alignment) field
          (let ((symbol (foreign-type-symbol name field-type *package*)))
            (list* symbol
                   `(,@(parse-type type (aval :tag type))
                     ,@(when (eq field-type :cfield)
                         `(:bit-size ,bit-size
                           :bit-offset ,bit-offset
                           :bit-alignment ,bit-alignment))))))))

(defun parse-enum-fields (fields)
  (let* ((type-symbol-fields
           (map 'vector
                (lambda (x)
                  (symbol-name
                   (foreign-type-symbol (aval :name x)
                                        :cenumfield
                                        *package*)))
                fields))
         (prefix-end (find-prefix type-symbol-fields)))
    (loop for field in fields
          as name = (foreign-type-symbol (aval :name field)
                                         :cenumfield *package*)
          as string = (symbol-name name)
          as truncated = (if (foreign-symbol-exception-p (aval :name field))
                             string
                             (substr* string prefix-end))
          collect (cons (intern truncated
                                (symbol-package name))
                        (aval :value field)))))

(defun parse-enum-to-const (fields location &optional included-p)
  (loop for field in fields
        as name = (aval :name field)
        collect (maybe-add-constant name location (aval :value field) included-p)
          into constants
        finally (return (remove-if #'null constants))))

(defmethod parse-form (form (tag (eql 'struct)) &key &allow-other-keys)
  (alist-bind (name fields) form
    (let ((sym (foreign-type-symbol name :cstruct *package*)))
      (let ((cstruct-fields (parse-fields fields)))
        (when (and (symbol-package sym) (form-finally-included-p form))
          (pushnew `(:struct (,sym)) *exported-foreign-record-list*
                   :test #'equal))
        `(define-foreign-record ',sym ,name :struct
           ,(aval :bit-size form)
           ,(aval :bit-alignment form)
           ',cstruct-fields)))))

(defmethod parse-form (form (tag (eql 'union)) &key &allow-other-keys)
  (alist-bind (name fields) form
    (let ((sym (foreign-type-symbol name :cunion *package*)))
      (let ((cunion-fields (parse-fields fields)))
        (when (and (symbol-package sym) (form-finally-included-p form))
          (pushnew `(:union (,sym)) *exported-foreign-record-list*
                   :test #'equal))
        `(define-foreign-record ',sym ,name :union
           ,(aval :bit-size form)
           ,(aval :bit-alignment form)
           ',cunion-fields)))))

(defmethod parse-form (form (tag (eql 'enum)) &key &allow-other-keys)
  (alist-bind (name location id fields) form
    (let* ((sym (foreign-type-symbol name :cenum *package*))
           (included-p (when (and (symbol-package sym) (form-finally-included-p form))
                         (pushnew sym *foreign-other-exports-list*))))

      `(progn
         ,@(parse-enum-to-const fields location included-p)
         (define-foreign-enum ',sym ,id ,name ',(parse-enum-fields fields))))))


(defmethod parse-form (form (tag (eql 'function)) &key &allow-other-keys)
  (alist-bind (name inline parameters return-type variadic) form
    (unless inline
      (let ((sym (foreign-type-symbol name :cfun *package*)))
        (let ((cfun-fields (parse-fields parameters :cparam)))
          (when (form-finally-included-p form)
            (pushnew sym *exported-foreign-function-list*))
          `(define-foreign-function '(,sym ,name
                                      ,@(when variadic '(:variadic-p t)))
               ',@(parse-type return-type (aval :tag return-type))
             ',cfun-fields))))))

(defmethod parse-form (form (tag (eql 'const)) &key &allow-other-keys)
  (alist-bind (name value location) form
    (maybe-add-constant name location value)))

(defmethod parse-form (form (tag (eql 'extern)) &key &allow-other-keys)
  (alist-bind (name type) form
    (let ((sym (foreign-type-symbol name :cextern *package*)))
      (when (form-finally-included-p form)
        (pushnew sym *exported-foreign-extern-list*))
      `(define-foreign-extern ',sym ,name ',@(parse-type type (aval :tag type))))))

(defun read-json (file)
  (let ((*read-default-float-format* 'double-float))
    (json:decode-json file)))

;; c-include utility

(defun make-scanners (list)
  (mapcar (lambda (x)
            (cons (ppcre:create-scanner (first x))
                  (second x)))
          list))

(defun read-parse-forms (in-spec)
  (loop for form in (read-json in-spec)
        as name = (aval :name form)
        as location = (aval :location form)
        collect (parse-form form (aval :tag form)) into forms
        finally (return (remove-if #'null forms))))

(defun make-define-list (def-symbol list package)
 (loop for x in (reverse list)
       collect `(,def-symbol ,x ,package)))

(defun make-export-list (list package &optional sym-fun)
  `(export '(,@(mapcar (or sym-fun
                        (lambda (x) (intern (symbol-name x) package)))
                list))
           ,package))

;; Exported API

(defun %find-path (system relative &optional path)
  (let ((relative (ensure-list relative)))
    (if path
        (flet ((%relative (base rel)
                 (let ((base (uiop:ensure-directory-pathname base)))
                   (uiop:merge-pathnames* (typecase rel
                                            (pathname rel)
                                            (t (string rel)))
                                          base))))
          (reduce #'%relative relative :initial-value path))
        (path-or-asdf (append (list system) relative)))))


(defun parse-sysincludes (system includes &optional path)
  (loop for include in includes
        collect (%find-path system include path)))


(defun package-functions (package-name)
  (loop for sym being the symbol in (find-package package-name)
        as fu = (find-function sym)
        when fu collect fu))


(defgeneric dump-c-wrapper (package-name wrapper-path &optional loader-function))


(defmacro %c-include (h-file &key (spec-path *default-pathname-defaults*)
                               symbol-exceptions symbol-regex
                               exclude-definitions exclude-sources exclude-arch
                               include-definitions include-sources include-arch
                               sysincludes
                               includes
                               framework-includes
                               (definition-package *package*)
                               exclude-constants
                               (trace-c2ffi *trace-c2ffi*)
                               type-symbol-function c-to-lisp-function
                               local-os local-environment
                               local-only language standard)
  (let ((*foreign-symbol-exceptions* (alist-hash-table symbol-exceptions :test 'equal))
        (*foreign-symbol-regex* (make-scanners (eval symbol-regex)))
        (*foreign-constant-excludes* (mapcar #'ppcre:create-scanner exclude-constants))
        (*foreign-raw-constant-list*)
        (*foreign-type-symbol-function* (or (and type-symbol-function
                                                 (eval type-symbol-function))
                                            *foreign-type-symbol-function*))
        (*foreign-c-to-lisp-function* (or (and c-to-lisp-function
                                               (eval c-to-lisp-function))
                                          *foreign-c-to-lisp-function*))
        (*include-definitions* include-definitions)
        (*include-sources* include-sources)
        (*exclude-definitions* (mapcar #'ppcre:create-scanner exclude-definitions))
        (*exclude-sources* (mapcar #'ppcre:create-scanner exclude-sources))
        (*package* (find-package definition-package))
        (h-file (path-or-asdf (eval h-file)))
        (spec-path (path-or-asdf (eval spec-path)))
        (*local-cpu* (when (boundp '*local-cpu*)
                       *local-cpu*))
        (*local-os* (eval local-os))
        (*local-environment* (eval local-environment))
        (definition-package (find-package definition-package)))
    (multiple-value-bind (spec-name)
        (let ((*trace-c2ffi* trace-c2ffi))
          (ensure-local-spec h-file
                             :spec-path spec-path
                             :arch-excludes exclude-arch
                             :arch-includes (if local-only
                                                (list (local-arch))
                                                include-arch)
                             :sysincludes (eval sysincludes)
                             :framework-includes (eval framework-includes)
                             :includes (eval includes)
                             :language language
                             :standard standard
                             :spec-processor #'squash-unrelated-definitions))
      (with-open-file (in-spec spec-name)
        (collecting-symbols
          `(progn
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (setf *failed-wraps* nil)
               (with-anonymous-indexing
                 ,@(read-parse-forms in-spec))
               ;; Definitions
               ,@(make-define-list 'define-cfun *exported-foreign-function-list* definition-package)
               ,@(make-define-list 'define-cextern *exported-foreign-extern-list* definition-package)
               ;; Report on anything missing
               (compile-time-report-wrap-failures)
               ;; Exports
               ,(when *exported-foreign-record-list*
                  (make-export-list *exported-foreign-record-list* *package*
                                    (lambda (x) (etypecase x (symbol x) (cons (caadr x))))))
               ,(when *exported-foreign-function-list*
                  (make-export-list *exported-foreign-function-list* definition-package))
               ,(when *exported-foreign-extern-list*
                  (make-export-list *exported-foreign-extern-list* definition-package))
               ,(when *exported-foreign-constant-list*
                  (make-export-list *exported-foreign-constant-list* definition-package))
               ,(when *foreign-other-exports-list*
                  `(export ',*foreign-other-exports-list* ,definition-package)))
             (eval-when (:load-toplevel :execute)
               (report-wrap-failures 'load-time *standard-output*)
               (clear-wrap-failures))))))))


(defmacro c-include (header system-opts &body body
                     &key in-package include-sources include-definitions
                       exclude-sources exclude-definitions
                       (spec-module :spec) rename-symbols
                       sysincludes
                       includes
                       (windows-environment "gnu")
                       language standard)
  (declare (ignore body))
  (destructuring-bind (include-name &key path system)
      (ensure-list system-opts)
    (let ((system-name (or system include-name)))
      (destructuring-bind (in-package &rest nicknames) (ensure-list in-package)
        (unless in-package
          (error ":in-package must be supplied"))
        `(progn
           (uiop:define-package ,in-package
               (:nicknames ,@nicknames)
             (:use))
           (%c-include
            (%find-path ',system-name ',header ,path)
            :spec-path (uiop:ensure-directory-pathname
                        (%find-path ',system-name ',spec-module ,path))
            :definition-package ,in-package
            :local-environment #+windows ,windows-environment #-windows "gnu"
            :local-only ,*local-only*
            :include-arch ("x86_64-pc-linux-gnu"
                           "i686-pc-linux-gnu"
                           ,(string+ "x86_64-pc-windows-" windows-environment)
                           ,(string+ "i686-pc-windows-" windows-environment)
                           "x86_64-apple-darwin-gnu"
                           "i686-apple-darwin-gnu")
            :sysincludes (append (parse-sysincludes ',system-name ',sysincludes ,path)
                                 ',(dump-all-gcc-include-paths))
            :framework-includes ',(dump-all-darwin-framework-paths)
            :includes (parse-sysincludes ',system-name ',includes ,path)
            :include-sources ,include-sources
            :include-definitions ,include-definitions
            :exclude-sources ,exclude-sources
            :exclude-definitions ,exclude-definitions
            :language ,language
            :standard ,standard
            :symbol-regex ,rename-symbols)
           (defmethod dump-c-wrapper ((package-name (eql ,in-package)) wrapper-path &optional loader)
             (declare (ignore package-name))
             (write-c-library-implementation wrapper-path
                                             (path-or-asdf ',header)
                                             (package-functions ,in-package)
                                             loader))))))  )
