(in-package :claw)


(defun make-cbv-wrapper (fun)
  (with-slots (c-symbol name fields) fun
    (unless (foreign-function-cbv-p fun)
      (error "Function '~S' doesn't pass structures by value" c-symbol))
    (let ((arg-counter 0))
      (labels ((%next-arg-name ()
                 (let ((current arg-counter))
                   (incf arg-counter)
                   (format nil "arg~A" current)))
               (%to-c-type-name (rich-type)
                 (let* ((type (basic-foreign-type rich-type))
                        (name (foreign-type-name type)))
                   (cond
                     ((struct-or-union-p name)
                      (let ((kind (cond
                                    ((find-type `(:struct (,name))) "struct")
                                    ((find-type `(:union (,name))) "union"))))
                        (format nil "~A ~A" kind (foreign-record-id type))))
                     ((eq :pointer name) "void*")
                     (t (substitute #\- #\_ (string-downcase type))))))
               (%describe-type (rich-type)
                 (let* ((type (basic-foreign-type rich-type))
                        (arg-name (string-downcase (%next-arg-name)))
                        (type-name (foreign-type-name type))
                        (c-type-name (%to-c-type-name type)))
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
               (invocation (string+ c-symbol "(" arg-string ");"))
               (return-arg (when (third return-type)
                             (second return-type)))
               (void-p (or (null return-type) (third return-type))))
          (apply #'string+
                 (append (list (if void-p "void" (first return-type))
                               " __claw_" c-symbol "(" param-string ") {")
                         (when return-type
                           (list (%to-c-type-name (basic-foreign-type fun-type)) " result = "))
                         (list invocation)
                         (when return-arg
                           (list (format nil "(*~A) = result;" return-arg)))
                         (unless void-p
                           (list (format nil "return result;")))
                         (list "}"))))))))


(defun write-c-library-implementaion (library-path h-path functions)
  (ensure-directories-exist library-path)
  (alexandria:with-output-to-file (out library-path :if-exists :supersede)
    (format out "#include \"~A\"" h-path)
    (loop for fu in functions
          when (foreign-function-cbv-p fu)
          do (format out "~%~A" (make-cbv-wrapper fu)))))
