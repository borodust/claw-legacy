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
               (%pointer-type-name (type)
                 (typecase type
                   (foreign-field (%pointer-type-name (foreign-type type)))
                   (foreign-pointer (string+ (%pointer-type-name (foreign-type type)) "*"))
                   (foreign-alias (foreign-type-id type))
                   (t "void*")))
               (%to-c-type-name (rich-type)
                 (let* ((type (basic-foreign-type rich-type))
                        (name (foreign-type-name type)))
                   (cond
                     ((struct-or-union-p name)
                      (let ((kind (cond
                                    ((find-type `(:struct (,name))) "struct")
                                    ((find-type `(:union (,name))) "union"))))
                        (format nil "~A ~A" kind (foreign-type-id type))))
                     ((eq :pointer type)
                      #++(break "~A:~A" rich-type
                             (foreign-type-id (foreign-type rich-type)))
                      (%pointer-type-name rich-type))
                     (t (substitute #\Space #\-  (string-downcase type))))))
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
               (invocation (string+ c-symbol "(" arg-string ");"))
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


(defun write-c-library-implementation (library-path h-path functions)
  (ensure-directories-exist library-path)
  (alexandria:with-output-to-file (out library-path :if-exists :supersede)
    (format out "#include \"~A\"" h-path)
    (format out "
#ifndef __CLAW_API
  #ifdef __cplusplus
    #define __CLAW_API extern \"C\"
  #else
    #define __CLAW_API
  #endif
#endif")
    (loop for fu in (sort functions #'string< :key #'foreign-type-id)
          when (foreign-function-cbv-p fu)
            do (format out "~&~%__CLAW_API ~A" (make-cbv-wrapper fu)))))
