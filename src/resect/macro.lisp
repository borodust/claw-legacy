(cl:in-package :claw.resect)


(declaim (special *macros*))

(defclass macro-inspector () ())


(defmethod inspect-declaration ((this macro-inspector) kind declaration)
  (declare (ignore this kind declaration)))


(defun prepare-macros-as-constants (uber-path
                                    includes
                                    frameworks
                                    target
                                    macros)
  (uiop:with-temporary-file (:pathname macro-helper-path :type "h")
    (alexandria:with-output-to-file (out macro-helper-path :if-exists :supersede)
      (format out "#ifndef  __CLAW_MACRO~%#define __CLAW_MACRO 1~%")
      (format out "~%#include \"~A\"~%" (uiop:native-namestring uber-path))
      (loop for macro in macros
            ;; extra } to terminate macros that actually open braces
            ;; obviously, this is invalid syntax, but hopefully
            ;; libclang just ignores that
            do (format out "~&auto ~A~A = (~A); }" +macro-prefix+ macro macro))
      (format out "~&~%#endif"))
    (let (*macros*)
      (inspect-foreign-library (make-instance 'macro-inspector)
                               macro-helper-path
                               includes
                               frameworks
                               :c++
                               :c++11
                               target
                               :diagnostics nil)
      *macros*)))


(defmethod inspect-declaration ((this macro-inspector) (kind (eql :variable)) declaration)
  (let ((name (%resect:declaration-name declaration))
        (value (case (%resect:variable-kind declaration)
                 (:int (%resect:variable-to-int declaration))
                 (:float (%resect:variable-to-float declaration))
                 (:string (%resect:variable-to-string declaration))
                 (t nil))))
    (when (starts-with-subseq +macro-prefix+ name)
      (push (make-instance 'foreign-constant
                           :id name
                           :name (subseq name (length +macro-prefix+))
                           :value value)
            *macros*))))
