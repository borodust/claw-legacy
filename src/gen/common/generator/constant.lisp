(cl:in-package :claw.generator.common)


(defmacro switch-float-infinity-value ((value) &body features)
  `(flet ((%ensure-readable-value (float-featured)
            (if *use-float-features*
                float-featured
                (make-keyword float-featured))))
     (switch (,value :test #'=)
       ,@(loop for feature in features
               collect `(,feature (%ensure-readable-value ',feature))))))


(defmacro switch-float-nan-value ((value) &body features)
  (once-only (value)
    (with-gensyms (bits)
      `(typecase ,value
         ,@(loop for feature in features
                 collect (destructuring-bind (type float->bits bits->float fallback) feature
                           `(,type
                             (if *use-float-features*
                                 (let ((,bits (,float->bits ,value)))
                                   `(,',bits->float ,,bits))
                                 ,fallback))))
         (t ,value)))))


(defun ensure-readable-float-value (value)
  (setf *float-features-requested* t)
  (if (float-features:float-nan-p value)
      (switch-float-nan-value (value)
        (single-float float-features:single-float-bits float-features:bits-single-float :single-nan)
        (double-float float-features:double-float-bits float-features:bits-double-float :double-nan)
        (short-float float-features:short-float-bits float-features:bits-short-float :short-nan)
        (long-float float-features:long-float-bits float-features:bits-long-float :long-nan))
      (switch-float-infinity-value (value)
        float-features:short-float-positive-infinity
        float-features:single-float-positive-infinity
        float-features:double-float-positive-infinity
        float-features:long-float-positive-infinity

        float-features:short-float-negative-infinity
        float-features:single-float-negative-infinity
        float-features:double-float-negative-infinity
        float-features:long-float-negative-infinity)))


(defun ensure-readable-constant-value (value)
  (cond
    ((and (typep value 'float)
          (or (float-features:float-nan-p value)
              (float-features:float-infinity-p value)))
     (ensure-readable-float-value value))
    (t value)))


(defmethod generate-binding ((generator generator) (entity claw.spec:foreign-constant) &key)
  (let* ((name (c-name->lisp (claw.spec:format-full-foreign-entity-name entity) :constant))
         (value (claw.spec:foreign-entity-value entity)))
    (expand-constant name (ensure-readable-constant-value value))))


(defmethod generate-binding ((generator generator) (entity claw.spec:foreign-variable) &key)
  (let* ((name (c-name->lisp (claw.spec:format-full-foreign-entity-name entity) :variable))
         (type (check-entity-known (claw.spec:foreing-variable-type entity)))
         (value (claw.spec:foreign-entity-value entity))
         (decorated-name (format-symbol (symbol-package name) "*~A*" name)))
    (export-symbol decorated-name)
    (if (claw.spec:foreing-variable-external-p entity)
        `((define-symbol-macro ,decorated-name
              ,(if (typep type 'claw.spec:foreign-array)
                   `(cffi:foreign-symbol-pointer ,(claw.spec:foreign-entity-name entity))
                   (let ((ptr (format-symbol (symbol-package decorated-name) "~A" 'ptr)))
                     `(let ((,ptr (cffi:foreign-symbol-pointer ,(claw.spec:foreign-entity-name entity))))
                        (when ,ptr
                          (cffi:mem-ref ,ptr ',(entity->cffi-type type))))))))
        `((defparameter ,decorated-name ,value)))))


(defmethod foreign-entity-dependencies ((entity claw.spec:foreign-variable))
  (find-foreign-dependencies (claw.spec:foreing-variable-type entity)))
