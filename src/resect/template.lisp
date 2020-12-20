(cl:in-package :claw.resect)


(defvar *template-argument-table* nil)


(defun root-template (decl)
  (if (cffi:null-pointer-p (%resect:declaration-template decl))
      decl
      (root-template (%resect:declaration-template decl))))


(defun extract-type-arguments (type)
  (let ((literals (extract-template-literals type))
        (args))
    (resect:docollection (arg (%resect:type-template-arguments type))
      (push (when (eq :type (%resect:template-argument-kind arg))
              (%resect:template-argument-type arg))
            args))
    (nreversef args)

    (loop for arg in args
          for literal in literals
          collect (or arg literal))))


(defun extract-decl-arguments (decl)
  (let (args)
    (resect:docollection (arg (%resect:declaration-template-arguments decl))
      (push (ecase (%resect:template-argument-kind arg)
              ((:type :declaration :template) (%resect:template-argument-type arg))
              ((:null :null-ptr) nil)
              ((:integral :expression :pack) (%resect:template-argument-value arg)))
            args))
    (nreverse args)))


(defun extract-decl-parameters (decl)
  (let (params)
    (resect:docollection (param (%resect:declaration-template-parameters (root-template decl)))
      (push param params))
    (nreverse params)))


(defun make-template-argument-table (decl)
  (let* ((table (make-hash-table :test 'equal))
         (args (extract-type-arguments (%resect:declaration-type decl)))
         (params (extract-decl-parameters decl)))
    (loop for param in params
          for arg in args
          for name = (parse-infix (%resect:declaration-name param))
          for parsed = (if (stringp arg)
                           (parse-infix arg)
                           arg)
          do (setf (gethash name table) parsed))
    table))


(defmacro with-template-argument-table ((decl) &body body)
  `(let ((*template-argument-table* (make-template-argument-table ,decl)))
     ,@body))


(defun generate-expression-form (expression)
  (let (names)
    `(let (,@(loop for name being the hash-key of *template-argument-table* using (hash-value value)
                   collect `(,name ,value)
                   do (push name names)))
       (declare (ignorable ,@names))
       ,(parse-infix expression))))


(defun eval-template-argument (expression)
  (when *template-argument-table*
    (let ((result (handler-case
                      (eval (generate-expression-form expression))
                    (t () nil))))
      (if (symbolp result)
          (gethash result *template-argument-table*)
          (unless (listp result)
            result)))))



(defgeneric reconstruct-type (kind type))

(defun reconstruct-decl-name (decl)
  (let ((owners (loop for owner = (%resect:declaration-owner decl)
                        then (%resect:declaration-owner owner)
                      until (cffi:null-pointer-p owner)
                      collect (%resect:declaration-name decl)))
        (namespace (%resect:declaration-namespace decl)))
    (format nil "~@[~A::~]~{~A~^::~}~A"
            (unless (emptyp namespace)
              namespace)
            (nreverse owners)
            (%resect:declaration-name decl))))


(defun reconstruct-type-name (type)
  (let ((decl (%resect:type-declaration type))
        (type-name (%resect:type-name type)))
    (if (cffi:null-pointer-p decl)
        (let ((reconstructed (reconstruct-type :template-parameter type)))
          (or reconstructed type-name))
        (format nil "~@[~A ~]~A"
                (when (or (starts-with "const " type-name)
                          (%resect:type-const-qualified-p type))
                  "const")
                (reconstruct-decl-name decl)))))


(defun reconstruct-from-type (type)
  (reconstruct-type (%resect:type-kind type) type))


(defmethod reconstruct-type ((kind (eql :template-parameter)) type)
  (let* ((name (%resect:type-name type))
         (reconstructed (if (starts-with-subseq "const" name)
                            (let* ((trimmed (string-trim
                                             '(#\Space)
                                             (subseq name (length "const"))))
                                   (value (gethash trimmed *template-argument-table*)))
                              (when value
                                (format nil "const ~A" value)))
                            (gethash name *template-argument-table*))))
    (or reconstructed name)))


(defmethod reconstruct-type ((kind (eql :pointer)) type)
  (format nil "~A *" (reconstruct-from-type (%resect:pointer-pointee-type type))))


(defmethod reconstruct-type ((kind (eql :rvalue-reference)) type)
  (format nil "~A &&" (reconstruct-from-type (%resect:reference-pointee-type type))))


(defmethod reconstruct-type ((kind (eql :lvalue-reference)) type)
  (format nil "~A &" (reconstruct-from-type (%resect:reference-pointee-type type))))


(defun reconstruct-typename (type-name)
  (let ((name (ppcre:regex-replace "typename " type-name "")))
    (labels ((%reconstruct-replace (groups)
               (loop for group in groups
                     collect (if (listp group)
                                 (%reconstruct-replace group)
                                 (if-let (replacement (gethash group *template-argument-table*))
                                   replacement
                                   group)))))
      (join-groups-into-template-name
       (%reconstruct-replace
        (split-template-name-into-groups name))))))


(defmethod reconstruct-type (kind type)
  (declare (ignore kind))
  (let* ((type-name (%resect:type-name type)))
    (if (search "typename " type-name)
        (reconstruct-typename type-name)
        (let* ((literals (extract-template-literals type))
               (reconstructed (loop for literal in literals
                                    for replacement = (gethash literal *template-argument-table*)
                                    collect (if replacement
                                                replacement
                                                literal))))
          (format nil "~A~@[~A~]"
                  (reconstruct-type-name type)
                  (when literals
                    (format-template-argument-string reconstructed)))))))


(defun make-reconstruction-table (decl template-arguments)
  (flet ((%parse-param (param)
           (list (%resect:declaration-name param)
                 (not (position #\= (%resect:declaration-source param))))))
    (let* ((owner-template-params (loop for owner = (%resect:declaration-owner decl)
                                          then (%resect:declaration-owner owner)
                                        until (cffi:null-pointer-p owner)
                                        collect (extract-decl-parameters owner) into result
                                        finally (return (flatten (nreverse result)))))
           (template-params (mapcar #'%parse-param (extract-decl-parameters decl)))
           (required-param-len (or (position nil template-params :key #'second)
                                   (length template-params))))
      ;; TODO: should we use owner-template-params here?
      (when (<= required-param-len (length template-arguments))
        (loop with table = (make-hash-table :test 'equal)
              for param in (mapcar #'first template-params)
              for arg in template-arguments
              do (setf (gethash param table) arg)
              finally (return table))))))


(defun reconstruct-templated-function (decl &rest template-arguments)
  (let* ((method-p (eq :method (%resect:declaration-kind decl)))
         (owner (%resect:declaration-owner decl))
         params
         (*template-argument-table* (make-reconstruction-table decl template-arguments)))
    (when *template-argument-table*
      (resect:docollection (param-decl (if method-p
                                           (%resect:method-parameters decl)
                                           (%resect:function-parameters decl)))
        (push (reconstruct-from-type (%resect:declaration-type param-decl)) params))
      (format nil "~A ~A~A(~@[~A, ~]~{~A~^, ~});"
              (reconstruct-from-type (if method-p
                                         (%resect:method-result-type decl)
                                         (%resect:function-result-type decl)))
              +instantiation-prefix+
              (ensure-mangled decl)
              (unless (or (cffi:null-pointer-p owner)
                          (eq :static (%resect:method-storage-class decl)))
                (reconstruct-from-type (%resect:declaration-type owner)))
              (nreverse params)))))
