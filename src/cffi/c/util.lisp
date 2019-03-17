(cl:in-package :claw.cffi.c)


(declaim (special *spec*
                  *dependency-type-list*
                  *visit-table*
                  *forward-declaration-table*
                  *dependency-list*
                  *symbol-package*
                  *symbol-renaming-pipeline*
                  *trim-enum-prefix-p*
                  *adapter*
                  *export-table*))


(define-constant +adapted-variable-prefix+ "__v_claw_"
  :test #'equal)


(define-constant +adapted-function-prefix+ "___claw_"
  :test #'equal)


(defgeneric adapted-function-name (function &optional stream))
(defgeneric adapted-function-definition (function adapted-name original-name
                                         &optional stream))
(defgeneric adapted-function-original-type (function name &optional stream))


(defun make-scanners (list)
  (mapcar (lambda (x)
            (cons (ppcre:create-scanner (first x))
                  (second x)))
          list))


(defun export-symbol (symbol)
  (setf (gethash symbol *export-table*) symbol))
