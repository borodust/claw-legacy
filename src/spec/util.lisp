(cl:in-package :claw.spec)

;; misc

(declaim (special *include-definitions*
                  *exclude-definitions*
                  *include-sources*
                  *exclude-sources*))

(define-constant +byte-size+ 8)

;; alists

(declaim (inline akey aval (setf aval)))
(defun akey (val alist &key (test 'eql)) (car (rassoc val alist :test test)))
(defun aval (key alist &key (test 'eql)) (cdr (assoc key alist :test test)))
(defun (setf aval) (value key alist &key (test 'eql))
  (setf (cdr (assoc key alist :test test)) value))

(defmacro alist-bind ((&rest vars) alist &body body)
  "Inefficient but doesn't really matter here"
  (once-only (alist)
    `(let (,@(mapcar (lambda (x)
                       (if (consp x)
                           `(,(car x) (aval ,(cadr x) ,alist))
                           `(,x (aval ,(make-keyword x) ,alist))))
                     vars))
       ,@body)))


(defun alist (&rest args &key &allow-other-keys)
  (plist-alist args))

;; testing

(defun included-p (thing includes)
  (when thing
    (loop for scanner in includes do
      (when (cl-ppcre:scan scanner thing)
        (return t)))))

(defun excluded-p (name location)
  (and (or (included-p name *exclude-definitions*)
           (and (included-p location *exclude-sources*)
                (not (included-p name *include-definitions*))))
       (not (or (included-p name *include-definitions*)
                (and (included-p location *include-sources*)
                     (not (included-p name *exclude-definitions*)))))))

(defun anonymous-p (form)
  (etypecase form
    (foreign-entity
     (null (foreign-entity-name form)))
    (cons
     (or (string= "" (aval :name form))
         (and (string= ":array" (aval :tag form))
              (string= "" (aval :name (aval :type form))))))))


(defun find-file-for-paths (file paths)
  (loop for path in paths
        as filename = (merge-pathnames file path)
        do (when (probe-file filename)
             (return filename))))

;; ASDF paths

(defun asdf-path (system &rest path)
  (asdf:component-pathname
   (or (asdf:find-component (asdf:find-system system t) path)
       (error "System ~S path not found: ~S" system path))))

(defun path-or-asdf (form)
  (etypecase form
    ((or string pathname) form)
    (list (apply #'asdf-path (car form) (cdr form)))))

;;;
;;; Inclusion rules
;;;
(defun explicitly-included-p (name location)
  (or (included-p name *include-definitions*)
      (and (included-p location *include-sources*)
           (not (included-p name *exclude-definitions*)))))


(defun explicitly-excluded-p (name location)
  (or (included-p name *exclude-definitions*)
      (and (included-p location *exclude-sources*)
           (not (included-p name *include-definitions*)))))


(defun finally-included-p (name location)
  (and (explicitly-included-p name location)
       (not (explicitly-excluded-p name location))))


(defun form-finally-included-p (form)
  (let ((name (aval :name form))
        (location (aval :location form)))
    (and (explicitly-included-p name location)
         (not (explicitly-excluded-p name location)))))


(defun keywordify (tag)
  (let* ((tag (if (eq #\: (aref tag 0)) (subseq tag 1) tag)))
    (make-keyword (uiop:standard-case-symbol-name tag))))
