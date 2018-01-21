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
               (%primitive-to-c (type)
                 (substitute #\Space #\-  (string-downcase type)))
               (%to-c-type-name (type)
                 (typecase type
                   (foreign-field (%to-c-type-name (foreign-type type)))
                   (foreign-record (string+ (ecase (foreign-type type)
                                              (:struct "struct ")
                                              (:union "union "))
                                            (foreign-type-id type)))
                   (foreign-pointer (string+ (%to-c-type-name (foreign-type type)) "*"))
                   (foreign-alias (foreign-type-id type))
                   (foreign-enum (string+ "enum " (foreign-type-id type)))
                   (keyword (%primitive-to-c type))
                   (t "void")))
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
  #if defined(_WIN32)
    #define __CLAW_API __declspec(dllexport)
  #else
    #define __CLAW_API
  #endif
#endif
#if defined(__cplusplus)
extern \"C\" {
#endif")
    (loop for fu in (sort functions #'string< :key #'foreign-type-id)
          when (foreign-function-cbv-p fu)
            do (format out "~&~%__CLAW_API ~A" (make-cbv-wrapper fu)))
    (format out "
#if defined(__cplusplus)
}
#endif
")))
