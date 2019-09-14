(uiop:define-package :claw.util
  (:use :cl :alexandria)
  (:export #:+known-platforms+
           #:+byte-size+

           #:with-evaluated-variables
           #:with-evaluated-lists
           #:local-platform
           #:find-path
           #:list-all-known-include-paths
           #:list-all-known-framework-paths
           #:default-c-name-to-lisp

           #:get-timestamp

           #:with-symbol-renaming
           #:c-name->lisp
           #:make-scanners
           #:in-pipeline
           #:by-changing
           #:by-removing-prefixes
           #:by-removing-complex-prefix))
(cl:in-package :claw.util)


(declaim (special *include-definitions*
                  *exclude-definitions*
                  *include-sources*
                  *exclude-sources*
                  *symbol-package*
                  *symbol-renaming-pipeline*))


(define-constant +byte-size+ 8)

(define-constant +path-search-regex+
  "\\> search starts here:\\s*((.|\\s)*)?\\s*End of search list"
  :test #'equal)

(define-constant +stupid-darwin-framework-postfix+
  " (framework directory)"
  :test #'equal)


(define-constant +known-platforms+
    '("i686-pc-linux-gnu"
      "x86_64-pc-linux-gnu"
      "i686-pc-windows-msvc"
      "x86_64-pc-windows-msvc"
      "i686-pc-windows-gnu"
      "x86_64-pc-windows-gnu"
      "i686-apple-darwin9"
      "x86_64-apple-darwin9"
      "i686-apple-darwin-gnu"
      "x86_64-apple-darwin-gnu"
      "i386-unknown-freebsd"
      "x86_64-unknown-freebsd"
      "i386-unknown-openbsd"
      "x86_64-unknown-openbsd"
      "arm-pc-linux-gnu")
  :test #'equal)


(defun %find-asdf-component-child (component child)
  (or (asdf:find-component component child)
      (error "Component ~S child not found: ~S"
             (asdf:component-pathname component) child)))


(defun asdf-path (component &rest path)
  (if (rest path)
      (apply #'asdf-path (%find-asdf-component-child component (first path)) (rest path))
      (etypecase (first path)
        ((or string pathname)
         (merge-pathnames (first path) (asdf:component-pathname component)))
        (null (asdf:component-pathname component))
        (t (asdf-path (%find-asdf-component-child component (first path)))))))


(defun path-or-asdf (form)
  (etypecase form
    ((or string pathname) form)
    (list (apply #'asdf-path (asdf:find-system (first form) t) (rest form)))))


(defun find-path (relative &key system path)
  (let ((relative (ensure-list relative)))
    (if (or path (not system))
        (flet ((%relative (base rel)
                 (let ((base (uiop:ensure-directory-pathname base)))
                   (uiop:merge-pathnames* (typecase rel
                                            (pathname rel)
                                            (t (string rel)))
                                          base))))
          (reduce #'%relative relative :initial-value (or path
                                                          *default-pathname-defaults*)))
        (path-or-asdf (append (list system) relative)))))

;;;
;;; Filtering
;;;
(defun by-removing-prefix (prefix)
  (list (format nil "^~A\\w+$" prefix)
        (lambda (name)
          (subseq name (length prefix)))))


(defun by-removing-postfix (postfix)
  (list (format nil "^\\w+~A$" postfix)
        (lambda (name)
          (subseq name 0 (- (length name) (length postfix))))))


(defun by-removing-prefixes (&rest prefixes)
  (flet ((by-prefix-length (this-prefix that-prefix)
           (> (length this-prefix)
              (length that-prefix))))
    (mapcar #'by-removing-prefix (stable-sort prefixes #'by-prefix-length))))


(defun by-removing-postfixes (&rest prefixes)
  (flet ((by-postfix-length (this-prefix that-prefix)
           (> (length this-prefix)
              (length that-prefix))))
    (mapcar #'by-removing-postfix (stable-sort prefixes #'by-postfix-length))))


(defun by-changing (from to)
  (list (list (format nil "^~A$" from)
              (lambda (name) (declare (ignore name)) (string to)))))


(defun in-pipeline (&rest processors)
  (reduce #'append processors))


(defun by-removing-complex-prefix (regex symbols-to-cut)
  (list (list regex (lambda (name) (subseq name symbols-to-cut)))))


(defun dump-gcc-include-paths (lang)
  (handler-case
      (let* ((command (format nil "echo | gcc -x~A -E -v -" lang))
             (paths (with-output-to-string (out)
                      (uiop:run-program command
                                        :output out :error-output out)))
             (bounds (third (multiple-value-list (ppcre:scan +path-search-regex+ paths)))))
        (when bounds
          (ppcre:split "(\\r|\\n)+\\s*" (subseq paths (aref bounds 0) (aref bounds 1)))))
    (t ()
      (warn "Failed to obtain GCC search paths for language ~A" lang)
      nil)))


(defun %darwin-framework-path-p (path)
  (ends-with-subseq +stupid-darwin-framework-postfix+ path :test #'equal))


(defun list-all-known-include-paths ()
  (remove-duplicates (remove-if #'%darwin-framework-path-p
                                (append (dump-gcc-include-paths "c")
                                        (dump-gcc-include-paths "c++")))
                     :test #'equal))


(defun list-all-known-framework-paths ()
  (flet ((cut-darwin-postfix (path)
           (subseq path 0 (- (length path) (length +stupid-darwin-framework-postfix+)))))
    (remove-duplicates
     (mapcar #'cut-darwin-postfix
             (remove-if (complement #'%darwin-framework-path-p)
                        (append (dump-gcc-include-paths "c")
                                (dump-gcc-include-paths "c++"))))
     :test #'equal)))


(defun dump-gcc-version ()
  (handler-case
      (string-trim '(#\Tab #\Space #\Newline)
                   (with-output-to-string (out)
                     (uiop:run-program "gcc -dumpversion" :output out)))
    (t () "")))

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


(defmacro with-evaluated-lists ((&rest bindings) &body body)
  (let ((rebindings (loop for binding in bindings
                          collect (destructuring-bind (name &optional list)
                                      (ensure-list binding)
                                    `(,name (eval `(list ,@,(or list name))))))))
    `(let (,@rebindings)
       ,@body)))


(defmacro with-evaluated-variables ((&rest bindings) &body body)
  (let ((rebindings (loop for binding in bindings
                          collect (destructuring-bind (name &optional value)
                                      (ensure-list binding)
                                    `(,name (eval (first ,(or value name))))))))
    `(let (,@rebindings)
       ,@body)))

;;;
;;; PLATFORM
;;;
;; Arch
(defvar *local-os* nil)
(defvar *local-environment* nil)
(defvar *local-cpu* nil)


(defun local-cpu ()
  (or *local-cpu*
      #+x86-64 "x86_64"
      #+(and (not (or x86-64 freebsd)) x86) "i686"
      #+(and (not x86-64) x86 freebsd) "i386"
      #+arm "arm"))


(defun local-vendor ()
  #+(or linux windows) "-pc"
  #+darwin "-apple"
  #+(not (or linux windows darwin)) "-unknown")


(defun local-os ()
  (or (and *local-os* (format nil "-~A" *local-os*))
      #+linux "-linux"
      #+windows "-windows"
      #+darwin "-darwin"
      #+freebsd "-freebsd"
      #+openbsd "-openbsd"
      #-(or linux windows darwin freebsd openbsd) (error "Unknown operating system")))


(defun local-environment ()
  (or (and *local-environment* (format nil "-~A" *local-environment*))
      #+linux "-gnu"
      #+windows "-msvc"
      #-(or linux windows) ""))


(defun local-platform ()
  (concatenate 'string (local-cpu) (local-vendor) (local-os) (local-environment)))


(defun default-c-name-to-lisp (string &optional (package *package*))
  (let ((string (ppcre:regex-replace-all "([A-Z]+)([A-Z][a-z])" string "\\1_\\2")))
    (let ((string (ppcre:regex-replace-all "([a-z]+)([A-Z])" string "\\1_\\2")))
      (format-symbol package (uiop:standard-case-symbol-name
                              (if (ppcre:all-matches "^(:_|_)" string)
                                  string
                                  (nsubstitute #\- #\_ string)))))))


(defun get-timestamp ()
  (local-time:format-timestring nil (local-time:now) :timezone local-time:+utc-zone+))


(defun c-name->lisp (name)
  (when name
    (loop with string = (format nil "~A" name)
          for r in *symbol-renaming-pipeline*
          when (ppcre:scan-to-strings (car r) string)
            do (setf string (funcall (cdr r) string))
          finally (return (default-c-name-to-lisp string
                                                  (or *symbol-package* *package*))))))


(defun make-scanners (list)
  (mapcar (lambda (x)
            (cons (ppcre:create-scanner (first x))
                  (second x)))
          list))


(defmacro with-symbol-renaming ((package renaming-pipeline) &body body)
  `(let ((*symbol-renaming-pipeline* (make-scanners ,renaming-pipeline))
         (*symbol-package* ,package))
     ,@body))
