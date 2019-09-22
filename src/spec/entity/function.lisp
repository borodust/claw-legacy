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
  (loop with variadic-p = nil
        for param in params
        for name = (aval :name param)
        for type = (aval :type param)
        for parsed-type = (parse-form type (aval :tag type))
        for basic-type = (find-basic-type parsed-type)
        if (and basic-type (equal "va_list" basic-type))
          do (setf variadic-p t)
        else
          collect (make-instance 'foreign-parameter
                                 :name (unless (emptyp name)
                                         name)
                                 :type parsed-type)
            into parsed-parameters
        finally (return (values parsed-parameters variadic-p))))


(defmethod parse-form (form (tag (eql :function)))
  (alist-bind (name inline parameters return-type variadic location storage-class) form
    (unless inline
      (multiple-value-bind (params variadic-p)
          (parse-parameters parameters)
        (foreign-entity-type
         (register-foreign-function name
                                    location
                                    (parse-form return-type (aval :tag return-type))
                                    params
                                    (or variadic-p variadic)
                                    storage-class))))))


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
  (alist :tag ":pointer"))


(defmethod foreign-entity-dependencies ((type foreign-function))
  (cleanup-dependencies
   (list* (%find-dependency (foreign-function-return-type type))
          (mapcar #'%find-entity-dependency
                  (foreign-function-parameters type)))))


(defmethod try-including-entity ((entity foreign-function))
  (when (call-next-method)
    (loop for dep-typespec in (foreign-entity-dependencies entity)
          for dep = (find-foreign-entity dep-typespec)
          unless (marked-strongly-included-p dep)
            do (if (anonymous-p dep)
                   (progn
                     (mark-included dep t)
                     (try-including-entity dep))
                   (unless (marked-strongly-partially-included-p dep)
                     (mark-partially-included dep t)
                     (try-including-entity dep))))
    t))


(defun optimize-parameters (parameters)
  (loop for parameter in parameters
        for type = (optimize-typespec (foreign-entity-type parameter))
        unless type
          do (error "~S was optimized away where shouldn't"
                    (foreign-entity-type parameter))
        collect (make-instance 'foreign-parameter
                               :name (foreign-entity-name parameter)
                               :type type)))


(defmethod optimize-entity ((this foreign-function))
  (when (marked-included-p this)
    (make-instance 'foreign-function
                   :name (foreign-entity-name this)
                   :location (foreign-entity-location this)
                   :type (foreign-entity-type this)
                   :return-type (optimize-typespec (foreign-function-return-type this))
                   :storage-class (foreign-function-storage-class this)
                   :variadic-p (foreign-function-variadic-p this)
                   :parameters (optimize-parameters (foreign-function-parameters this)))))
