(cl:in-package :claw.resect)


(declaim (special *inspector*))


(defun wrap-with-namespace (source namespace)
  (with-output-to-string (result)
    (let ((namespaces (ppcre:split "::" namespace)))
      (loop for namespace in namespaces
            do (format result "~&namespace ~A {" namespace))
      (format result "~&~A" source)
      (loop for nil in namespaces
            do (format result "~&}")))))


(defclass declaration-for-instantiation ()
  ((name :initarg :name :reader declaration-name)
   (namespace :initarg :namespace :reader declaration-namespace)
   (parameters :initarg :parameters :reader declaration-template-parameters)
   (owner :initarg :owner :reader declaration-owner)
   (location :initarg :location :reader declaration-location)))


(defun make-declaration-for-instantiation (decl)
  (let* ((params (mapcar #'%resect:declaration-name (extract-decl-parameters decl)))
         (location (%resect:declaration-location decl))
         (location-string (format nil "~A:~A:~A"
                                  (%resect:location-name location)
                                  (%resect:location-line location)
                                  (%resect:location-column location)))
         (owner (%resect:declaration-owner decl)))
    (make-instance 'declaration-for-instantiation
                   :name (%resect:declaration-name decl)
                   :namespace (%resect:declaration-namespace decl)
                   :parameters params
                   :location location-string
                   :owner (unless (cffi:null-pointer-p owner)
                            (make-declaration-for-instantiation owner)))))


(defun instantiatablep (decl)
  (when (and *instantiation-filter* (extract-decl-parameters decl))
    (funcall *instantiation-filter* (make-declaration-for-instantiation decl))))


(defun register-function-if-instantiable (declaration)
  (when-let ((template-args (instantiatablep declaration)))
    (when-let ((reconstructred (loop for set in template-args
                                     for rec = (apply #'reconstruct-templated-function
                                                      declaration
                                                      set)
                                     when rec
                                       collect (wrap-with-namespace
                                                rec
                                                (%resect:declaration-namespace declaration)))))
      (setf
       (gethash (format nil "~A~A"
                        (%resect:declaration-id declaration)
                        (format-template-argument-string template-args))
                *instantiated-functions*)
       (list (%resect:declaration-name declaration) reconstructred)))))


(defgeneric prepare-declaration (kind declaration &key &allow-other-keys)
  (:method (kind declaration &key)
    (declare (ignore kind declaration))))

;;;
;;; PREPARING
;;;
(defclass preparing-inspector ()
  ((prepared-entity-table :initform (make-hash-table :test 'equal))
   (instantiated-classes :initform (make-hash-table :test 'equal))
   (instantiated-functions :initform (make-hash-table :test 'equal))
   (instantiation-filter :initarg :instantiation-filter)
   (macros :initform (make-hash-table :test 'equal) :reader macros-of)))


(defgeneric prepare-header (inspector uber-path prepared-path))
(defgeneric prepare-instantiated-source (inspector decl))

(defmethod inspect-declaration ((this preparing-inspector) kind declaration)
  (with-slots (prepared-entity-table
               instantiated-classes
               instantiated-functions
               instantiation-filter
               macros)
      this
    (let ((*inspector* this)
          (*prepared-entity-table* prepared-entity-table)
          (*instantiated-classes* instantiated-classes)
          (*instantiated-functions* instantiated-functions)
          (*instantiation-filter* instantiation-filter)
          (*macros* macros))
      (prepare-declaration kind declaration))))


(defun prepare-foreign-library (uber-path
                                prepared-dir
                                includes
                                frameworks
                                language
                                standard
                                target
                                intrinsics
                                instantiation-filter)
  (let ((prepared-path (merge-pathnames "prepared_implicit.h" prepared-dir))
        (implicit (make-instance 'implicit-preparing-inspector
                                 :instantiation-filter instantiation-filter)))
    (inspect-foreign-library implicit
                             uber-path
                             includes
                             frameworks
                             language
                             standard
                             target
                             intrinsics)
    (prepare-header implicit uber-path prepared-path)
    (values (list prepared-path)
            (loop for macro being the hash-value of (macros-of implicit)
                  collect macro))))


(defun format-template-argument-string (argument-literals)
  (format nil "~@[<~{~A~^,~}>~]" argument-literals))


(defun extract-template-literals (type)
  (let ((string (extract-template-argument-string (%resect:type-name type))))
    (unless (emptyp string)
      (remove-if #'emptyp
                 (split-template-argument-string-into-literals string)))))


(defun prepare-type (type)
  (let ((type-decl (%resect:type-declaration type)))
    (unless (cffi:null-pointer-p type-decl)
      (prepare-declaration (%resect:declaration-kind type-decl) type-decl))))


(defun publicp (decl)
  (let ((access-specifier (%resect:declaration-access-specifier decl)))
    (or (eq :unknown access-specifier)
        (eq :public access-specifier))))


(defun template-arguments-public-p (decl)
  (let ((public-p t))
    (resect:docollection (arg (%resect:type-template-arguments (%resect:declaration-type decl)))
      (when (eq :type (%resect:template-argument-kind arg))
        (let ((arg-type-decl (%resect:type-declaration (%resect:template-argument-type arg))))
          (when (and (not (cffi:null-pointer-p arg-type-decl))
                     (not (publicp arg-type-decl)))
            (setf public-p nil)
            (return)))))
    public-p))


(defun prepare-new-record-template-instantiations (decl)
  (unless (or (cffi:null-pointer-p (%resect:declaration-template decl))
              (%resect:declaration-partially-specialized-p decl)
              (not (publicp decl))
              (not (template-arguments-public-p decl))
              (emptyp (%resect:declaration-name decl)))
    (setf
     (gethash (%resect:declaration-id decl) *instantiated-classes*)
     (prepare-instantiated-source *inspector* decl)))

  (resect:docollection (parent-type (%resect:record-parents decl))
    (prepare-type parent-type))

  (resect:docollection (field-decl (%resect:record-fields decl))
    (prepare-type (%resect:declaration-type field-decl)))

  (resect:docollection (method-decl (%resect:record-methods decl))
    (register-function-if-instantiable method-decl)
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
  (register-function-if-instantiable declaration)
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
    (let ((name (%resect:declaration-name declaration))
          (location (format-foreign-location (make-declaration-location declaration) nil)))
      (setf (gethash name *macros*) (cons name location)))))


;;;
;;; PREPARE IMPLICIT
;;;
(defclass implicit-preparing-inspector (preparing-inspector) ())


(defmethod prepare-header ((inspector implicit-preparing-inspector) uber-path prepared-path)
  (with-slots (instantiated-classes instantiated-functions) inspector
    (alexandria:with-output-to-file (out prepared-path :if-exists :supersede)
      (format out "#ifndef __CLAW_PREPARED_IMPLICIT~%#define __CLAW_PREPARED_IMPLICIT 1")
      (format out "~%~%#include \"~A\"~%~%" (uiop:native-namestring uber-path))

      (when-let ((keys (sort (hash-table-keys instantiated-classes) #'string<)))
        (loop for key in keys
              for counter from 0
              for source = (gethash key instantiated-classes)
              do (format out "~%~A" (funcall source counter))))

      (when-let ((functions (sort (hash-table-values instantiated-functions) #'string<
                                  :key #'first)))
        (format out "~%")
        (loop for (nil formatted) in functions
              do (loop for f in formatted
                       do (format out "~%~A" f))))

      (format out "~%~%")
      (format out "~%#endif"))))


(defmethod prepare-instantiated-source ((this implicit-preparing-inspector) decl)
  (let ((name (%resect:type-name (%resect:declaration-type decl)))
        (namespace (%resect:declaration-namespace decl)))
    (lambda (postfix)
      (wrap-with-namespace (format nil "~A ~A~A;"
                                   name
                                   +instantiation-prefix+
                                   postfix)
                           namespace))))
