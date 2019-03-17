(cl:in-package :claw.sffi.c)


(defun make-cbv-function-variable (fun)
  (with-slots (c-symbol name fields) fun
    (unless (foreign-function-cbv-p fun)
      (error "Function '~S' doesn't pass structures by value" c-symbol))
    (let* ((fun-type (foreign-type fun))
           (parameters (loop for f in fields collect (%to-c-type-name f)))
           (void-p (eq :void fun-type))
           (return-type (if void-p "void" (%to-c-type-name fun-type)))
           (param-string (format nil "~{~A~^, ~}" parameters)))
      (format nil "static ~A (*~A~A)(~A);"
              return-type
              +variable-cbv-prefix+
              c-symbol
              param-string))))


(defun make-cbv-function-variable-init (fun)
  (with-slots (c-symbol) fun
    (unless (foreign-function-cbv-p fun)
      (error "Function '~S' doesn't pass structures by value" c-symbol))
    (format nil "    ~A~A = claw_get_proc_addr(\"~A\");"
            +variable-cbv-prefix+
            c-symbol
            c-symbol)))


(defun make-cbv-wrapper (fun &optional (c-prefix ""))
  (with-slots (c-symbol name fields) fun
    (unless (foreign-function-cbv-p fun)
      (error "Function '~S' doesn't pass structures by value" c-symbol))
    (let ((arg-counter 0))
      (labels ((%next-arg-name ()
                 (let ((current arg-counter))
                   (incf arg-counter)
                   (format nil "arg~A" current)))
               (%describe-type (rich-type)
                 (let* ((arg-name (string-downcase (%next-arg-name)))
                        (type-name (foreign-type-name (basic-foreign-type rich-type)))
                        (c-type-name (%to-c-type-name rich-type)))
                   (if (struct-or-union-p type-name)
                       (list (string+ c-type-name "*") arg-name t)
                       (list c-type-name arg-name nil))))
               (%to-param-pair-string (cons)
                 (format nil "~A ~A" (first cons) (second cons)))
               (%to-arg-string (type-spec)
                 (destructuring-bind (type name cbv-p) type-spec
                   (declare (ignore type))
                   (if cbv-p
                       (format nil "(*~A)" name)
                       name))))
        (let* ((fun-type (foreign-type fun))
               (parameters (loop for f in fields
                                 collect (%describe-type f)))
               (return-type (unless (eq :void fun-type)
                              (%describe-type fun-type)))
               (param-string (format nil "~{~A~^, ~}" (mapcar #'%to-param-pair-string
                                                              (append (when (third return-type)
                                                                        (list return-type))
                                                                      parameters))))
               (arg-string (format nil "~{~A~^, ~}" (mapcar #'%to-arg-string parameters)))
               (invocation (string+ c-prefix c-symbol "(" arg-string ");"))
               (return-arg (when (third return-type)
                             (second return-type)))
               (void-p (or (null return-type) (third return-type))))
          (with-output-to-string (output)
            (format output "~A ~A~A(~A) {~%"
                    (if void-p "void" (first return-type))
                    +cbv-prefix+
                    c-symbol
                    param-string)
            (if return-type
                (format output "  ~A result = "
                        (%to-c-type-name fun-type))
                (format output "  "))
            (format output "~A" invocation)
            (when return-arg
              (format output "~&  (*~A) = result;" return-arg))
            (unless void-p
              (format output "~&  return result;"))
            (format output "~&}")))))))


(defun preprocess-template (source &rest args)
  (labels ((%replace (name value source)
             (ppcre:regex-replace-all (format nil "{{\\s*~A\\s*}}" name) source value))
           (%to-source (result arg)
             (destructuring-bind (name . value) arg
               (%replace name value result))))
    (reduce #'%to-source (alexandria:plist-alist args) :initial-value source)))


(defun %preprocess-wrapper-source (header-file function-definitions)
  (preprocess-template *source-template*
                       "header-file" header-file
                       "function-definitions" function-definitions))

(defun prepare-wrapper-source (header-file functions)
  (let ((function-defs (with-output-to-string (out)
                         (loop for fu in functions
                               do (format out "~&~%__CLAW_API ~A" (make-cbv-wrapper fu))))))
    (%preprocess-wrapper-source header-file function-defs)))


(defun prepare-dynamic-source (header-file loader-name functions)
  (let* ((function-ptrs (with-output-to-string (out)
                          (loop for fu in functions
                                do (format out "~&~A"
                                           (make-cbv-function-variable fu)))))
         (function-ptrs-init (with-output-to-string (out)
                               (loop for fu in functions
                                     do (format out "~&~A"
                                                (make-cbv-function-variable-init fu)))))
         (function-defs (with-output-to-string (out)
                          (loop for fu in functions
                                do (format out "~&~%__CLAW_API ~A"
                                           (make-cbv-wrapper fu +variable-cbv-prefix+)))))
         (dynamic-defs (preprocess-template *dynamic-wrapping-template*
                                            "loader-name" loader-name
                                            "function-pointers" function-ptrs
                                            "function-pointers-init" function-ptrs-init
                                            "function-definitions" function-defs)))
    (%preprocess-wrapper-source header-file dynamic-defs)))


(defun write-c-library-implementation (library-path h-path functions &optional loader-name)
  (ensure-directories-exist library-path)
  (let ((cbv-functions (sort (remove-if (complement #'foreign-function-cbv-p) functions)
                             #'string< :key #'foreign-type-id)))
    (alexandria:with-output-to-file (out library-path :if-exists :supersede)
      (if loader-name
          (format out (prepare-dynamic-source h-path loader-name cbv-functions))
          (format out (prepare-wrapper-source h-path cbv-functions)))))
  (values))
