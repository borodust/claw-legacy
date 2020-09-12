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

    (loop for (namespace name) being the hash-value of *instantiated* using (hash-key id)
          for counter from 0
          do (format out "~%~A::~A~A ~A~A;"
                     namespace
                     name
                     (extract-template-argument-string id)
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


(defclass template-argument-map ()
  ((table :initform (make-hash-table :test 'equal))))


(defmethod template-argument ((this template-argument-map) name)
  (with-slots (table) this
    (gethash name table)))



(defun format-template-argument-string (argument-literals)
  (format nil "<~{~A~^,~}>" argument-literals))


(defun extract-template-literals (type)
  (remove-if #'emptyp
             (split-template-argument-string-into-literals
              (extract-template-argument-string
               (%resect:type-name type)))))


(defun fill-template-argument-map (map type)
  (with-slots (table) map
    (let (template-arguments
          template-parameter-names
          template-literals)
      ;; template-arguments
      (resect:docollection (template-arg (%resect:type-template-arguments type))
        (push template-arg template-arguments))
      ;; template-parameters
      (resect:docollection (param (%resect:declaration-template-parameters (%resect:type-declaration type)))
        (push (%resect:declaration-name param) template-parameter-names))
      ;; template-literals
      (setf template-literals (reverse (extract-template-literals type)))
      (let ((literal-length (length template-literals))
            (param-length (length template-parameter-names)))
        (when (< literal-length param-length)
          (loop repeat (- param-length literal-length)
                do (push nil template-literals))))

      (loop for arg in template-arguments
            for param-name in template-parameter-names
            for literal in template-literals
            unless (starts-with-subseq "type-parameter" literal)
              do (let ((value (if (eq (%resect:template-argument-kind arg) :type)
                                  (%resect:template-argument-type arg)
                                  (if-let ((existing (find-template-argument literal)))
                                    existing
                                    literal))))
                   (setf (gethash param-name table) value))))))


(defun make-template-argument-map (&optional type)
  (let ((map (make-instance 'template-argument-map)))
    (when type
      (fill-template-argument-map map type))
    map))


(defmacro with-next-template-argument-map ((map-name &optional source-type) &body body)
  `(let* ((,map-name (make-template-argument-map ,source-type))
          (*template-argument-map-list* (list* ,map-name *template-argument-map-list*)))
     ,@body))


(defun prepare-type (type)
  (let ((type-decl (%resect:type-declaration type)))
    (unless (cffi:null-pointer-p type-decl)
      (prepare-declaration (%resect:declaration-kind type-decl) type-decl
                           :from-type type))))


(defun prepare-new-record-template-instantiations (id decl)
  (when (find #\< id)
    (setf (gethash id *instantiated*) (list (%resect:declaration-namespace decl)
                                            (%resect:declaration-name decl))))

  (resect:docollection (parent-decl (%resect:record-parents decl))
    (prepare-declaration (%resect:declaration-kind parent-decl) parent-decl))

  (resect:docollection (field-decl (%resect:record-fields decl))
    (prepare-type (%resect:declaration-type field-decl)))

  (resect:docollection (method-decl (%resect:record-methods decl))
    (prepare-type (%resect:method-result-type method-decl))
    (resect:docollection (param-decl (%resect:method-parameters method-decl))
      (prepare-type (%resect:declaration-type param-decl)))))


(defun prepare-declaration-id (decl)
  (let (template-parameter-names)
    (resect:docollection (param-decl (%resect:declaration-template-parameters decl))
      (push (%resect:declaration-name param-decl) template-parameter-names))
    (nreversef template-parameter-names)

    (let ((prepared-template-literals (loop for param-name in template-parameter-names
                                            for arg = (find-template-argument param-name)
                                            when arg
                                              collect (if (stringp arg)
                                                          arg
                                                          (%resect:type-name arg)))))
      (if (and template-parameter-names
               (= (length prepared-template-literals)
                  (length template-parameter-names)))
          (format nil "~A~A"
                  (%resect:declaration-id decl)
                  (format-template-argument-string prepared-template-literals))
          (%resect:declaration-id decl)))))


(defun prepare-record-template-instantiations (decl &optional from-type)
  (with-next-template-argument-map (arg-map)
    (when from-type
      (fill-template-argument-map arg-map from-type))
    (let* ((id (prepare-declaration-id decl))
           (prepared (gethash id *prepared-entity-table*)))
      (if prepared
          prepared
          (prog1 (setf (gethash id *prepared-entity-table*) id)
            (prepare-new-record-template-instantiations id decl))))))


(defmethod prepare-declaration ((type (eql :struct)) decl &key from-type)
  (prepare-record-template-instantiations decl from-type))


(defmethod prepare-declaration ((type (eql :union)) decl &key from-type)
  (prepare-record-template-instantiations decl from-type))


(defmethod prepare-declaration ((type (eql :class)) decl &key from-type)
  (prepare-record-template-instantiations decl from-type))


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
