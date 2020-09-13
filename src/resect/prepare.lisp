(cl:in-package :claw.resect)


(declaim (special *prepared-entity-table*
                  *instantiated*
                  *macros*))


(defvar *template-argument-map-list* nil)

(define-constant +instantiation-prefix+ "__claw_instantiated"
  :test #'string=)


(defgeneric template-argument (source name)
  (:method (source name)
    (declare (ignore source name))
    nil))


(defun find-template-argument (name)
  (loop for map in *template-argument-map-list*
        do (multiple-value-bind (value exist)
               (template-argument map name)
             (when exist
               (return (values value t))))))


(defgeneric prepare-declaration (kind declaration &key &allow-other-keys)
  (:method (kind declaration &key)
    (declare (ignore kind declaration))))


(defclass preparing-inspector () ())


(defmethod inspect-declaration ((this preparing-inspector) kind declaration)
  (prepare-declaration kind declaration))


(defun prepare-macros ()
  (with-output-to-string (out)
    (loop for macro being the hash-key of *macros*
          do (format out "~&auto __claw_macro_~A = ~A;" macro macro))  ))


(defun prepare-header (uber-path prepared-path)
  (alexandria:with-output-to-file (out prepared-path :if-exists :supersede)
    (format out "#ifndef  __CLAW_PREPARED~%#define __CLAW_PREPARED 1~%")
    (format out "~%#include \"~A\"~%" (uiop:native-namestring uber-path))


    (loop for name in (sort (hash-table-values *instantiated*) #'string<)
          for counter from 0
          collect (format out "~%~A ~A~A;"
                          name
                          +instantiation-prefix+
                          counter))
    (format out "~&~%#endif")))


(defun prepare-foreign-library (uber-path
                                prepared-path
                                includes
                                frameworks
                                language
                                standard
                                target)
  (let ((*prepared-entity-table* (make-hash-table :test 'equal))
        (*instantiated* (make-hash-table :test 'equal))
        (*macros* (make-hash-table :test 'equal)))
    (inspect-foreign-library (make-instance 'preparing-inspector)
                             uber-path
                             includes
                             frameworks
                             language
                             standard
                             target)
    (prepare-macros)
    (prepare-header uber-path prepared-path)))


(defun format-template-argument-string (argument-literals)
  (format nil "<~{~A~^,~}>" argument-literals))


(defun extract-template-literals (type)
  (remove-if #'emptyp
             (split-template-argument-string-into-literals
              (extract-template-argument-string
               (%resect:type-name type)))))


(defun prepare-type (type)
  (let ((type-decl (%resect:type-declaration type)))
    (unless (cffi:null-pointer-p type-decl)
      (prepare-declaration (%resect:declaration-kind type-decl) type-decl))))


(defun prepare-new-record-template-instantiations (decl)
  (unless (or (cffi:null-pointer-p (%resect:declaration-template decl))
              (%resect:declaration-partially-specialized-p decl))
    (setf
     (gethash (%resect:declaration-id decl) *instantiated*)
     (%resect:type-name (%resect:declaration-type decl))))

  (resect:docollection (parent-decl (%resect:record-parents decl))
    (prepare-declaration (%resect:declaration-kind parent-decl) parent-decl))

  (resect:docollection (field-decl (%resect:record-fields decl))
    (prepare-type (%resect:declaration-type field-decl)))

  (resect:docollection (method-decl (%resect:record-methods decl))
    (prepare-type (%resect:method-result-type method-decl))
    (resect:docollection (param-decl (%resect:method-parameters method-decl))
      (prepare-type (%resect:declaration-type param-decl)))))


(defun prepare-record-template-instantiations (decl)
  (let* ((id (%resect:declaration-id decl))
         (prepared (gethash id *prepared-entity-table*)))
    (if prepared
        prepared
        (prog1 (setf (gethash id *prepared-entity-table*) id)
          (prepare-new-record-template-instantiations decl)))))


(defmethod prepare-declaration ((type (eql :struct)) decl &key)
  (prepare-record-template-instantiations decl))


(defmethod prepare-declaration ((type (eql :union)) decl &key)
  (prepare-record-template-instantiations decl))


(defmethod prepare-declaration ((type (eql :class)) decl &key)
  (prepare-record-template-instantiations decl))


(defmethod prepare-declaration ((kind (eql :function)) declaration &key)
  (prepare-type (%resect:function-result-type declaration))
  (resect:docollection (param-decl (%resect:function-parameters declaration))
    (prepare-type (%resect:declaration-type param-decl))))


(defmethod prepare-declaration ((kind (eql :functino-prototype)) declaration &key)
  (prepare-declaration :function declaration))

(defmethod prepare-declaration ((kind (eql :typedef)) declaration &key)
  (prepare-type (%resect:typedef-aliased-type declaration)))

(defmethod prepare-declaration ((kind (eql :variable)) declaration &key)
  (prepare-type (%resect:variable-type declaration)))

;;;
;;; MACRO
;;;
(defmethod prepare-declaration ((kind (eql :macro)) declaration &key)
  (unless (%resect:macro-function-like-p declaration)
    (setf (gethash (%resect:declaration-name declaration) *macros*) t)))
