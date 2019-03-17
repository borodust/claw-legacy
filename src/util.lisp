(uiop:define-package :claw.util
  (:use :cl :alexandria)
  (:export #:+known-platforms+

           #:with-evaluated-variables
           #:with-evaluated-lists
           #:local-platform
           #:find-path
           #:list-all-known-include-paths
           #:list-all-known-framework-paths
           #:default-c-name-to-lisp

           #:get-timestamp

           #:in-pipeline
           #:by-changing
           #:by-removing-prefixes
           #:by-removing-complex-prefix))
(cl:in-package :claw.util)


(declaim (special *include-definitions*
                  *exclude-definitions*
                  *include-sources*
                  *exclude-sources*))


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


(defun substr* (str start &optional end)
  "Make a shared substring of STR using MAKE-ARRAY :displaced-to"
  (let* ((end (or end (length str)))
         (len (- end start)))
    (make-array len :element-type (array-element-type str)
                    :displaced-to str
                    :displaced-index-offset start)))

(declaim (inline string+))
(defun string+ (string &rest strings)
  (apply #'concatenate 'string (string string)
         (mapcar #'string strings)))

(defun find-prefix (list &key (pred 'string))
  (let* ((sorted-fields (sort (map 'vector pred list) #'string<))
         (first (elt sorted-fields 0))
         (last (when (> (length sorted-fields) 1)
                 (elt sorted-fields (1- (length sorted-fields))))))
    (if (and first last)
        (let ((n (mismatch first last)))
          (cond
            ;; Never allow blanks
            ((and n (> n 0) (= n (length first))) (1- n))
            (n n)
            (t 0)))
        0)))

(defun prefix-trim (list &key (pred 'string) regex)
  (if regex
      (let ((scanner (ppcre:create-scanner regex)))
        (mapcar (lambda (x) (ppcre:regex-replace-all scanner (funcall pred x) "")) list))
      (let* ((prefix-end (find-prefix list :pred pred)))
        (map 'list (lambda (x) (subseq (funcall pred x) prefix-end)) list))))

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

;; Symbol trimming

(defun trim-symbols-to-alist (list &optional regex)
  (let* ((scanner (ppcre:create-scanner "(\\W)(.*?)\\1"))
         (trimmed-symbols
           (mapcar (lambda (x)
                     (ppcre:regex-replace scanner (string x) "\\2"))
                   list))
         (keyword-symbols (mapcar #'make-keyword (prefix-trim trimmed-symbols :regex regex))))
    (loop for symbol in list
          for keyword in keyword-symbols
          collect ``(,',keyword . ,,symbol))))

;; output

(defun write-nicely (stream object)
  (write object
         :stream stream
         :case :downcase
         :circle t
         :pretty t
         :readably t)
  (format stream "~%~%"))

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
    (foreign-type
     (null (symbol-package (foreign-type-name form))))
    (cons
     (or (string= "" (aval :name form))
         (and (string= ":array" (aval :tag form))
              (string= "" (aval :name (aval :type form))))))))


;; files

(defun find-file-for-paths (file paths)
  (loop for path in paths
        as filename = (merge-pathnames file path)
        do (when (probe-file filename)
             (return filename))))

;; ASDF paths


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

;; Conditions

;; from pergamum
(defun report-simple-condition (condition stream)
  (apply #'format stream (simple-condition-format-control condition) (simple-condition-format-arguments condition)))

;; from pergamum
(defmacro define-simple-condition-for (base-type &key object-initarg (simple-condition-type 'simple-error) (signaler 'error)
                                                   (name (format-symbol t "SIMPLE-~A" base-type)))
  `(progn
     (define-condition ,name (,base-type ,simple-condition-type)
       ()
       (:report report-simple-condition))
     (defun ,base-type (,@(when object-initarg `(o)) format-control &rest format-arguments)
       (,signaler ',name ,@(when object-initarg `(,object-initarg o)) :format-control format-control :format-arguments format-arguments))))

;; from pergamum
(defmacro define-simple-error-for (base-type &key name object-initarg)
  "Define a simple error subclassing from BASE-TYPE and a corresponding
function, analogous to ERROR, but also optionally taking the object
against which to err, and passing it to ERROR via the OBJECT-INITARG
keyword. The name of the simple error is constructed by prepending
'SIMPLE-' to BASE-TYPE.
Whether or not the error signaller will require and pass the
object is specified by OBJECT-INITARG being non-NIL."
  `(define-simple-condition-for ,base-type :object-initarg ,object-initarg :simple-condition-type simple-error :signaler error
                                ,@(when name `(:name ,name))))


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


#+ccl
(defmacro with-float-traps-masked-on-ccl ((&rest masks) &body body)
  (flet ((expand-mask (mask)
           (list (case mask
                   (:divide-by-zero :division-by-zero)
                   (t mask))
                 nil)))
    (let ((args (reduce #'nconc (mapcar #'expand-mask masks))))
      (with-gensyms (current-mode)
        `(let ((,current-mode (ccl:get-fpu-mode)))
           (unwind-protect
                (progn
                  (ccl:set-fpu-mode ,@args)
                  ,@body)
             (apply #'ccl:set-fpu-mode ,current-mode)))))))


#+ecl
(defmacro with-float-traps-masked-on-ecl ((&rest masks) &body body)
  (declare (ignore masks))
  `(let ((masked-bits (si::trap-fpe 'cl:last t)))
     (unwind-protect
          (progn
            (si::trap-fpe masked-bits nil)
            ,@body)
       (si::trap-fpe masked-bits t))))



(defmacro with-float-traps-masked ((&rest masks) &body body)
  (let* ((masks (or masks
                    '(:overflow
                      :underflow
                      :inexact
                      :invalid
                      :divide-by-zero)))
         (masking #+sbcl `(sb-int:with-float-traps-masked ,masks)
                  #+ccl `(with-float-traps-masked-on-ccl ,masks)
                  #+ecl `(with-float-traps-masked-on-ecl ,masks)
                  #-(or sbcl ccl ecl) '(progn)))
    `(,@masking
      ,@body)))


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


(declaim (inline ptr))
(defun ptr (wrapper)
  (etypecase wrapper
    (cffi:foreign-pointer wrapper)
    (integer (cffi:make-pointer wrapper))
    (null (cffi:null-pointer))))


(declaim (inline null-pointer-p))
(defun null-pointer-p (value)
  (etypecase value
    (cffi:foreign-pointer (cffi-sys:null-pointer-p value))
    (null t)))


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
