(cl:in-package :claw.resect)


(defvar *template-argument-table* nil)


(defun root-template (decl)
  (if (cffi:null-pointer-p (%resect:declaration-template decl))
      decl
      (root-template (%resect:declaration-template decl))))


(defun extract-type-arguments (type)
  (let ((literals (extract-template-literals type))
        (args))
    (resect:docollection (arg (%resect:type-template-arguments type))
      (push (when (eq :type (%resect:template-argument-kind arg))
              (%resect:template-argument-type arg))
            args))
    (nreversef args)

    (loop for arg in args
          for literal in literals
          collect (or arg literal))))


(defun extract-decl-parameters (decl)
  (let (params)
    (resect:docollection (param (%resect:declaration-template-parameters (root-template decl)))
      (push param params))
    (nreverse params)))


(defun make-template-argument-table (decl)
  (let* ((table (make-hash-table :test 'equal))
         (args (extract-type-arguments (%resect:declaration-type decl)))
         (params (extract-decl-parameters decl)))
    (loop for param in params
          for arg in args
          for name = (parse-infix (%resect:declaration-name param))
          for parsed = (if (stringp arg)
                           (parse-infix arg)
                           arg)
          do (setf (gethash name table) parsed))
    table))


(defmacro with-template-argument-table ((decl) &body body)
  `(let ((*template-argument-table* (make-template-argument-table ,decl)))
     ,@body))


(defun generate-expression-form (expression)
  (let (names)
    `(let (,@(loop for name being the hash-key of *template-argument-table* using (hash-value value)
                   collect `(,name ,value)
                   do (push name names)))
       (declare (ignorable ,@names))
       ,(parse-infix expression))))


(defun eval-template-argument (expression)
  (when *template-argument-table*
    (let ((result (handler-case
                      (eval (generate-expression-form expression))
                    (t () nil))))
      (if (symbolp result)
          (gethash result *template-argument-table*)
          (unless (listp result)
            result)))))
