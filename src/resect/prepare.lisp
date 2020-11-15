(cl:in-package :claw.resect)


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


(defclass preparing-inspector () ())


(defmethod inspect-declaration ((this preparing-inspector) kind declaration)
  (prepare-declaration kind declaration))


(defun prepare-header (uber-path prepared-path)
  (alexandria:with-output-to-file (out prepared-path :if-exists :supersede)
    (format out "#ifndef  __CLAW_PREPARED~%#define __CLAW_PREPARED 1")
    (format out "~%~%#include \"~A\"~%~%" (uiop:native-namestring uber-path))

    (when-let ((classes (sort (hash-table-values *instantiated-classes*) #'string<)))
      (format out "~%~%")
      (loop for name in classes
            for counter from 0
            collect (format out "~%~A ~A~A;"
                            name
                            +instantiation-prefix+
                            counter)))
    (when-let ((functions (sort (hash-table-values *instantiated-functions*) #'string<
                                :key #'first)))
      (format out "~%~%")
      (loop for (nil formatted) in functions
            collect (format out "~%~A" formatted)))

    (format out "~%~%")
    (format out "~%#endif")))


(defun prepare-foreign-library (uber-path
                                prepared-path
                                includes
                                frameworks
                                language
                                standard
                                target
                                instantiation-filter)
  (let ((*prepared-entity-table* (make-hash-table :test 'equal))
        (*instantiated-classes* (make-hash-table :test 'equal))
        (*instantiated-functions* (make-hash-table :test 'equal))
        (*instantiation-filter* instantiation-filter)
        (*macros* (make-hash-table :test 'equal)))
    (inspect-foreign-library (make-instance 'preparing-inspector)
                             uber-path
                             includes
                             frameworks
                             language
                             standard
                             target)
    (prepare-header uber-path prepared-path)
    (loop for macro-name being the hash-key of *macros*
          collect macro-name)))


(defun format-template-argument-string (argument-literals)
  (format nil "<~{~A~^,~}>" argument-literals))


(defun extract-template-literals (type)
  (let ((string (extract-template-argument-string (%resect:type-name type))))
    (unless (emptyp string)
      (remove-if #'emptyp
                 (split-template-argument-string-into-literals
                  (subseq string 1 (1- (length string))))))))


(defun reformat-template-argument-string-from-type (type)
  (reformat-template-argument-string
   (extract-template-argument-string
    (%resect:type-name type))))


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
              (not (template-arguments-public-p decl)))
    (setf
     (gethash (%resect:declaration-id decl) *instantiated-classes*)
     (%resect:type-name (%resect:declaration-type decl))))

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
