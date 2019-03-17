(cl:in-package :claw.spec)


(defclass foreign-parameter (foreign-entity) ())


(defclass foreign-function (foreign-entity)
  ((variadic-p :initarg :variadic-p
               :initform nil :reader foreign-function-variadic-p)
   (return-type :initarg :return-type :initform (error ":return-type missing")
                :reader foreign-function-return-type)
   (storage-class :initarg :storage-class
                  :initform nil :reader foreign-function-storage-class)
   (parameters :initarg :parameters :initform (error ":parameters missing")
               :reader foreign-function-parameters)))


(defun register-foreign-function (name
                                  location
                                  return-type
                                  params
                                  variadic-p
                                  storage-class)
  "=> FOREIGN-FUNCTION

Define a foreign function given a lisp symbol, C symbol (as a string),
return type and parameters.  Note this just defines to SFFI what the
foreign function looks like .. it doesn't actually DEFUN something to
call it.  "
  (let ((fun (make-instance 'foreign-function
                            :name name
                            :location location
                            :type name
                            :return-type return-type
                            :storage-class storage-class
                            :variadic-p variadic-p
                            :parameters params)))
    (register-foreign-entity name fun)
    fun))


(defun parse-parameters (params)
  (loop for param in params
        collect (alist-bind (name type) param
                  (make-instance 'foreign-parameter
                                 :name (unless (emptyp name)
                                         name)
                                 :type (parse-form type (aval :tag type))))))


(defmethod parse-form (form (tag (eql :function)))
  (alist-bind (name inline parameters return-type variadic location storage-class) form
    (unless inline
      (foreign-entity-type
       (register-foreign-function name
                                  location
                                  (parse-form return-type (aval :tag return-type))
                                  (parse-parameters parameters)
                                  variadic
                                  storage-class)))))


(defmethod parse-form (form (tag (eql :function-pointer)))
  `(:pointer "void"))


(defun compose-parameters (parameters)
  (loop for param in parameters
        collect (alist :tag "parameter"
                       :name (foreign-entity-name param)
                       :type (compose-reference (foreign-entity-type param)))))


(defmethod compose-form ((this foreign-function))
  (alist :tag "function"
         :name (foreign-entity-name this)
         :location (foreign-entity-location this)
         :inline 'false
         :variadic (if (foreign-function-variadic-p this) 'true 'false)
         :return-type (compose-reference (foreign-function-return-type this))
         :storage-class (foreign-function-storage-class this)
         :parameters (compose-parameters (foreign-function-parameters this))))


(defmethod compose-entity-reference ((this foreign-function))
  (alist :tag ":function-pointer"))
