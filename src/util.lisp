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
           #:common-prefix

           #:parse-renaming-pipeline
           #:with-symbol-renaming
           #:c-name->lisp

           #:with-local-cpu
           #:with-local-environment))
(cl:in-package :claw.util)


(declaim (special *include-definitions*
                  *exclude-definitions*
                  *include-sources*
                  *exclude-sources*
                  *symbol-package*
                  *symbol-type*
                  *symbol-renaming-pipeline*
                  *hit-count*))


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

;;;
;;; PATH SEARCH
;;;
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
;;; INCLUDE PATHS
;;;
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
;;; EVALUATION
;;;
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
(defvar *local-os* nil)
(defvar *local-environment* nil)
(defvar *local-cpu* nil)


(defmacro with-local-cpu ((cpu) &body body)
  `(let ((*local-cpu* ,cpu))
     ,@body))


(defmacro with-local-environment ((env) &body body)
  `(let ((*local-environment* ,env))
     ,@body))


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
      "-gnu"))


(defun local-platform ()
  (concatenate 'string (local-cpu) (local-vendor) (local-os) (local-environment)))


(defun default-c-name-to-lisp (string &optional (package *package*))
  (let* ((string (ppcre:regex-replace-all "([A-Z]+)([A-Z][a-z])" string "\\1_\\2"))
         (string (ppcre:regex-replace-all "([A-Z]+)([A-Z][a-z])" string "\\1_\\2"))
         (string (ppcre:regex-replace-all "([a-z]+)([A-Z])" string "\\1_\\2"))
         (string (if (ppcre:all-matches "^(:_|_)" string)
                     (let ((position (position #\_ string :test (complement #'equal))))
                       (nsubstitute #\% #\_ string :end position))
                     string))
         (string (nsubstitute #\- #\_ string)))
    (format-symbol package (uiop:standard-case-symbol-name string))))


;;;
;;; RENAMING
;;;
(defun make-scanners (list)
  (flet ((%to-scanner (regex-action)
           (cons (ppcre:create-scanner (car regex-action)) (cdr regex-action))))
    (mapcar #'%to-scanner list)))


(defmacro with-symbol-renaming ((in-package renaming-pipeline) &body body)
  `(let ((*symbol-renaming-pipeline* (make-scanners ,renaming-pipeline))
         (*symbol-package* ,in-package))
     ,@body))


(defun pipeline-rename (name)
  (loop with *hit-count* = 0
        with string = (format nil "~A" name)
        for scanner-action in *symbol-renaming-pipeline*
        when (ppcre:scan-to-strings (car scanner-action) string)
          do (setf string (funcall (cdr scanner-action) string)
                   *hit-count* (1+ *hit-count*))
        finally (return string)))


(defun c-name->lisp (name &optional type)
  (when name
    (let* ((*symbol-package* (or *symbol-package* *package*))
           (*symbol-type* type)
           (name (pipeline-rename name)))
      (default-c-name-to-lisp name (or *symbol-package* *package*)))))


(defun %%by-removing-prefix (prefix)
  (cons (format nil "^~A\\w+$" prefix)
        (lambda (name)
          (subseq name (length prefix)))))


(defun %%by-removing-postfix (postfix)
  (cons (format nil "^\\w+~A$" postfix)
        (lambda (name)
          (subseq name 0 (- (length name) (length postfix))))))


(defun %by-removing-prefixes (&rest prefixes)
  (flet ((by-prefix-length (this-prefix that-prefix)
            (> (length this-prefix)
               (length that-prefix))))
    (mapcar #'%%by-removing-prefix (stable-sort prefixes #'by-prefix-length))))


(defun by-removing-prefixes (configuration)
  `(%by-removing-prefixes ,@configuration))


(defun %by-removing-postfixes (&rest prefixes)
  (flet ((by-postfix-length (this-prefix that-prefix)
           (> (length this-prefix)
              (length that-prefix))))
    (mapcar #'%%by-removing-postfix (stable-sort prefixes #'by-postfix-length))))


(defun by-removing-postfixes (configuration)
  `(%by-removing-postfixes ,@configuration))


(defun %by-changing (from to)
  (list (cons (format nil "^~A$" from)
              (lambda (name) (declare (ignore name)) (string to)))))


(defun by-changing (configuration)
  `(%by-changing ,@configuration))


(defun %switch-package (package)
  (list (cons ".*" (lambda (name)
                     (setf *symbol-package* package)
                     name))))


(defun switch-package (new-package)
  `(%switch-package ',(first new-package)))


(defun %except-for (types &rest pipelines)
  (list (cons ".*" (lambda (name)
                     (if (member *symbol-type* types :test #'eq)
                         name
                         (apply-pipeline pipelines name))))))


(defun except-for (configuration)
  (multiple-value-bind (types pipelines)
      (loop for (type . rest) on configuration
            while (keywordp type)
            collect type into types
            finally (return (values types (list* type rest))))
    `(%except-for ',types ,@(collect-renaming-pipelines pipelines))))


(defun %by-replacing (regex replacement)
  (list (cons regex (lambda (name)
                      (ppcre:regex-replace-all regex name replacement)))))


(defun by-replacing (configuration)
  (destructuring-bind (regex replacement) configuration
   `(%by-replacing ,regex ,replacement)))


(defun %only-for (types &rest pipelines)
  (list (cons ".*" (lambda (name)
                     (if (member *symbol-type* types :test #'eq)
                         (apply-pipeline pipelines name)
                         name)))))


(defun only-for (configuration)
  (multiple-value-bind (types pipelines)
      (loop for (type . rest) on configuration
            while (keywordp type)
            collect type into types
            finally (return (values types (list* type rest))))
    `(%only-for ',types ,@(collect-renaming-pipelines pipelines))))


(defun apply-pipeline (processors name)
  (let ((*symbol-renaming-pipeline* (reduce #'append processors)))
    (pipeline-rename name)))


(defun %in-pipeline (&rest processors)
  (list (cons ".*" (lambda (name)
                     (apply-pipeline processors name)))))


(defun in-pipeline (configuration)
  `(%in-pipeline ,@(collect-renaming-pipelines configuration)))


(defun %by-removing-complex-prefix (regex symbols-to-cut)
  (list (cons regex (lambda (name) (subseq name symbols-to-cut)))))


(defun by-removing-complex-prefix (configuration)
  `(%by-removing-complex-prefix ,@configuration))


(defun %by-prepending (prefix)
  (list (cons ".*" (lambda (name) (concatenate 'string prefix name)))))


(defun by-prepending (configuration)
  `(%by-prepending ,@configuration))


(defun %if-none-matched (&rest processors)
  (list (cons ".*" (lambda (name)
                     (if (zerop *hit-count*)
                         (apply-pipeline processors name)
                         name)))))


(defun if-none-matched (configuration)
  `(%if-none-matched ,@(collect-renaming-pipelines configuration)))


(defun collect-renaming-pipelines (configuration)
  (loop for description in configuration
        collect (parse-renaming-pipeline description)))


(defun parse-renaming-pipeline (description)
  (when-let ((descriptor (first description)))
    (funcall
     (eswitch (descriptor :test #'string=)
       ('in-pipeline #'in-pipeline)
       ('by-changing #'by-changing)
       ('by-replacing #'by-replacing)
       ('by-removing-prefixes #'by-removing-prefixes)
       ('by-removing-postfixes #'by-removing-postfixes)
       ('by-removing-complex-prefix #'by-removing-complex-prefix)
       ('by-prepending #'by-prepending)
       ('switch-package #'switch-package)
       ('if-none-matched #'if-none-matched)
       ('except-for #'except-for)
       ('only-for #'only-for))
     (rest description))))


;;;
;;; VARIOUS
;;;
(defun get-timestamp ()
  (local-time:format-timestring nil (local-time:now) :timezone local-time:+utc-zone+))


(defun common-prefix (strings)
  (let ((len (length strings))
        (strings (map 'vector #'string strings)))
    (if (> len 1)
        (let* ((sorted-strings (sort strings #'string<))
               (first (aref sorted-strings 0))
               (last (aref sorted-strings (1- (length sorted-strings))))
               (mismatch-idx (mismatch first last)))
          (if mismatch-idx
              (if-let ((hyphenated-prefix-idx (position #\- first :from-end t
                                                                  :end mismatch-idx)))
                (subseq first 0 (1+ hyphenated-prefix-idx))
                "")
              ""))
        "")))
