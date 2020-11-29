(cl:in-package :claw.resect)


(declaim (special *inspector*))

(defun instantiatablep (decl)
  (when *instantiation-filter*
    (funcall *instantiation-filter* decl)))


(defun register-function-if-instantiable (declaration)
  (when-let ((template-args (instantiatablep declaration)))
    (when-let ((reconstructred (apply #'reconstruct-templated-function declaration template-args)))
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
            (loop for macro-name being the hash-key of (macros-of implicit)
                  collect macro-name))))


(defun format-template-argument-string (argument-literals)
  (format nil "<~{~A~^,~}>" argument-literals))


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
    (setf (gethash (%resect:declaration-name declaration) *macros*) t)))


;;;
;;; PREPARE IMPLICIT
;;;
(defclass implicit-preparing-inspector (preparing-inspector) ())


(defmethod prepare-header ((inspector implicit-preparing-inspector) uber-path prepared-path)
  (with-slots (instantiated-classes) inspector
    (alexandria:with-output-to-file (out prepared-path :if-exists :supersede)
      (format out "#ifndef __CLAW_PREPARED_IMPLICIT~%#define __CLAW_PREPARED_IMPLICIT 1")
      (format out "~%~%#include \"~A\"~%~%" (uiop:native-namestring uber-path))

      (when-let ((keys (sort (hash-table-keys instantiated-classes) #'string<)))
        (loop for key in keys
              for source = (gethash key instantiated-classes)
              for counter from 0
              collect (format out "~%~A ~A~A;" source +instantiation-prefix+ counter)))
      (format out "~%~%")
      (format out "~%#endif"))))


(defmethod prepare-instantiated-source ((this implicit-preparing-inspector) decl)
  (%resect:type-name (%resect:declaration-type decl)))
