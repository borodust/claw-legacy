(cl:in-package :claw.sffi.c)


(defclass c-library ()
  ((platform-table :initform (make-hash-table :test 'equal))))


(defun make-scanners (list)
  (mapcar (lambda (x)
            (cons (ppcre:create-scanner (first x))
                  (second x)))
          list))


(defmethod initialize-instance :after ((this c-library) &key library
                                                          package
                                                          output
                                                          rename-symbols)
  (declare (ignore package output scanners rename-symbols))
  (with-slots (platform-table) this
    (claw.spec:map-platforms (platform spec library)
      (let ((*type-table* (make-hash-table :test 'equal)))
        (claw.spec:do-foreign-entities (entity spec)
          (parse-c-type entity spec))
        (setf (gethash platform platform-table) *type-table*)))))


(defmethod claw.wrapper:make-library ((language (eql :c)) library
                                      &key use-package
                                        output-path
                                        rename-symbols)
  (make-instance 'c-library :library library))



(defmethod claw.wrapper:generate-bindings ((this c-library))
  (with-slots (platform-table) this
    (let ((local-platform (local-platform)))
      (if-let ((*type-table* (gethash local-platform platform-table)))
        (loop for type being the hash-value of *type-table*
              append (generate-binding type))
        (error "No specification defined for current paltform ~A" local-platform)))))


(defmethod claw.wrapper:list-adapted-functions ((library c-library)))
(defmethod claw.wrapper:library-header-file ((library c-library)))
(defmethod claw.wrapper:library-loader-name ((library c-library)))

(defclass c-library-function () ())

(defmethod claw.wrapper:library-function-name ((function c-library-function)))
#++(defmethod claw.wrapper:generate-adapted-function-definition ((function c-library-function)
                                                              name &optional output))
#++(defmethod claw.wrapper:generate-adapted-function-variable ((function c-library-function)
                                                            name &optional output))
