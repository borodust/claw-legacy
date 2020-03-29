(cl:in-package :claw.spec)


(defgeneric parse-declaration (kind declaration)
  (:method (kind declaration)
    (warn "Ignored declaration of ~A kind: ~A" kind (%resect:declaration-name declaration))))


(defmethod parse-declaration ((kind (eql :unknown)) declaration)
  (declare (ignore kind declaration)))


(defgeneric parse-type (category kind type)
  (:method (category kind type)
    (warn "Ignored type of ~A kind from ~A category: ~A" kind category (%resect:type-name type))))


(defun write-uber-header (headers path)
  (alexandria:with-output-to-file (out path :if-exists :supersede)
    (loop for header in headers
          do (format out "#include \"~A\"~%" header))))


(defun build-library-specification (headers &key
                                              includes
                                              frameworks
                                              language
                                              standard
                                              target)
  (flet ((%stringify (value)
           (if (stringp value)
               value
               (string-downcase value))))
    (uiop:with-temporary-file (:pathname path :type "h")
      (write-uber-header headers path)
      (resect:with-translation-unit (unit (uiop:native-namestring path)
                                     :include-paths includes
                                     :framework-paths frameworks
                                     :language (%stringify language)
                                     :standard (%stringify standard)
                                     :target (%stringify target))
        (resect:docollection (decl (%resect:translation-unit-declarations unit))
          (parse-declaration (%resect:declaration-kind decl) decl))))))
