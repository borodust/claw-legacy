(cl:in-package :claw.iffi)


(define-condition intricate-condition (serious-condition)
  (handle))


(defvar *function-table* (make-hash-table :test 'equal))

(defvar *intricate-table* (make-hash-table))


(defun (setf intricate-function) (value name &rest arg-types)
  (setf (gethash (list* name arg-types) *function-table*) value))


(defun intricate-function (name &rest arg-types)
  (gethash (list* name arg-types) *function-table*))


(defun intricate-funcall (name &rest args)
  (loop for (type value) on args by #'cddr
        collect type into arg-types
        collect value into arg-values
        finally (return (if-let ((fu (apply #'intricate-function name arg-types)))
                          (apply fu arg-values)
                          (error "Intricate function with signature ~A ~A not found" name arg-types)))))


(define-compiler-macro intricate-funcall (&whole whole name &rest args)
  (loop for (type value) on args by #'cddr
        collect type into arg-types
        collect value into arg-values
        finally (return (if-let ((function (apply #'intricate-function (eval name) arg-types)))
                          `(funcall ,function ,@arg-values)
                          whole))))


(defun expand-intricate-function-body (name arguments)
  `(intricate-funcall ',name ,@arguments))


(defmacro defifun (name-and-options return-type &body configuration)
  (destructuring-bind (mangled name &rest opts) (ensure-list name-and-options)
    (let* ((arg-config (if (stringp (first configuration))
                           (rest configuration)
                           configuration))
           (arg-types (loop for arg in arg-config by #'cddr
                            for type = (if (eq arg '&rest)
                                           '&rest
                                           (second arg))
                            collect type))
           (intricately-defined (gethash name *intricate-table*)))
      (with-gensyms (cfun-name)
        `(progn
           (cffi:defcfun (,mangled ,cfun-name ,@opts) ,return-type ,@configuration)
           (setf (intricate-function ',name ,@arg-types) #',cfun-name)
           ,@(when (or (not intricately-defined)
                       (equal intricately-defined arg-types))
               (setf (gethash name *intricate-table*) arg-types)
               `((defun ,name (&rest args)
                   (apply #'intricate-funcall ',name args))
                 (define-compiler-macro ,name (&rest arguments)
                   (expand-intricate-function-body ',name arguments)))))))))
