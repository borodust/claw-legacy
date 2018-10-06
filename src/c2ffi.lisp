(cl:in-package :claw)

;;; Basic invocation for c2ffi with some architecture-related
;;; stuff.

 ;; Arch

;;; Note this is rather untested and not very extensive at the moment;
;;; it should probably work on linux/win/osx though.  Patches welcome.

(declaim (special *local-os*)
         (special *local-environment*)
         (special *local-cpu*))

(defvar *rebuild-spec* nil)

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

(defun local-arch ()
  (string+ (local-cpu) (local-vendor) (local-os) (local-environment)))

(defparameter *known-arches*
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
    "arm-pc-linux-gnu"))

 ;; c2ffi

(defvar *c2ffi-program* #-windows "c2ffi" #+windows "c2ffi.exe")

(defvar *trace-c2ffi* nil)

(defun run-check (program args &key output ignore-error-status)
  (when *trace-c2ffi*
    (format *debug-io* "~&; Invoking: ~A~{ ~A~}~%" program args))
  (zerop (nth-value 2 (uiop:run-program (list* program args) :output output :ignore-error-status ignore-error-status))))

(defun c2ffi-p ()
  "This is a hack to determine if c2ffi exists; it assumes if it
doesn't exist, we will get a return code other than 0."
  (zerop (nth-value 2 (uiop:run-program `(,*c2ffi-program* "-h") :ignore-error-status t))))


(defun pass-through-processor (input output)
  (uiop:copy-stream-to-stream input output :element-type 'base-char))


;;; UIOP:WITH-TEMPORARY-FILE does not seem to compile below as of asdf
;;; 3.1.5, and that's what SBCL is distributed with, so.
(defmacro with-temporary-file ((&key pathname keep (stream (gensym "STREAM") streamp)) &body body)
  `(uiop:call-with-temporary-file
    (lambda (,stream ,pathname)
      (unless ,streamp (close ,stream))
      ,@body)
    :keep ,keep))

(defun run-c2ffi (input-file output-basename &key arch sysincludes ignore-error-status
                                               (spec-processor #'pass-through-processor)
                                               language standard)
  "Run c2ffi on `INPUT-FILE`, outputting to `OUTPUT-FILE` and
`MACRO-OUTPUT-FILE`, optionally specifying a target triple `ARCH`."
  (with-temporary-file (:pathname tmp-macro-file
                        :keep *trace-c2ffi*)
    (let* ((output-spec (string+ output-basename ".spec"))
           (arch (when arch (list "-A" arch)))
           (includes (loop for dir in sysincludes
                           if (uiop:directory-exists-p dir)
                             append (list "-I" dir)
                           else
                             do (when *trace-c2ffi*
                                  (format *debug-io* "~&; c2ffi include ignored: ~A not found" dir)))))
      (ensure-directories-exist output-spec)
      ;; Invoke c2ffi to emit macros into TMP-MACRO-FILE
      (when (run-check *c2ffi-program* (append
                                        (when language
                                          (list "--lang" language))
                                        (when standard
                                          (list "--std" standard))
                                        (list* (namestring input-file)
                                               "-D" "null"
                                               "-M" (namestring tmp-macro-file)
                                               (append arch includes)))
                       :output *standard-output*
                       :ignore-error-status ignore-error-status)
        ;; Write a tmp header file that #include's the input file and the macros file.
        (with-temporary-file (:stream tmp-include-file-stream
                              :pathname tmp-include-file
                              :keep *trace-c2ffi*)
          (format tmp-include-file-stream "#include \"~A\"~%" input-file)
          (format tmp-include-file-stream "#include \"~A\"~%" tmp-macro-file)
          (close tmp-include-file-stream)
          ;; Invoke c2ffi again to generate the raw output.
          (with-temporary-file (:pathname tmp-raw-output)
            (run-check *c2ffi-program* (append
                                        (when language
                                          (list "-x" language))
                                        (list* (namestring tmp-include-file)
                                               "-o" (namestring tmp-raw-output)
                                               (append arch includes)))
                       :output *standard-output*
                       :ignore-error-status ignore-error-status)
            (with-open-file (raw-input tmp-raw-output)
              (with-open-file (final-output output-spec :direction :output :if-exists :supersede)
                (funcall spec-processor raw-input final-output)
                t))))))))
 ;; Specs and Loading

(defun find-local-spec (name &optional (spec-path *default-pathname-defaults*))
  "Return the path of the SPEC for this machine's architecture, or NIL
if the file does not exist."
  (let* ((arch (local-arch))
         (name (pathname-name name))
         (h-name (make-pathname :defaults spec-path
                                :name (string+ name "." arch)
                                :type "spec")))
    (when (probe-file h-name) h-name)))

(defun ensure-local-spec (name &key
                                 (spec-path *default-pathname-defaults*)
                                 arch-excludes
                                 arch-includes
                                 sysincludes
                                 version
                                 spec-processor
                                 language
                                 standard)
  (flet ((spec-path (arch) (string+ (namestring spec-path)
                                    (pathname-name name)
                                    (if version
                                        (string+ "-" version)
                                        "")
                                    "." arch)))
    (multiple-value-bind (h-name m-name) (find-local-spec name spec-path)
      (if (and h-name (not *rebuild-spec*))
          (values h-name m-name)
          (progn
            (unless (c2ffi-p)
              (error "No spec for ~S on arch '~A' and c2ffi not found"
                     name (local-arch)))
            (let ((arch (local-arch)))
              (run-c2ffi name (spec-path arch)
                         :arch arch
                         :sysincludes sysincludes
                         :spec-processor spec-processor
                         :language language
                         :standard standard))
            (loop with local-arch = (local-arch)
                  for arch in *known-arches* do
                    (unless (or (string= local-arch arch)
                                (member arch arch-excludes :test #'string=)
                                (and arch-includes
                                     (not (member arch arch-includes :test #'string=))))
                      (unless (run-c2ffi name (spec-path arch)
                                         :arch arch
                                         :sysincludes sysincludes
                                         :ignore-error-status t
                                         :spec-processor spec-processor
                                         :language language
                                         :standard standard)
                        (warn "Error generating spec for other arch: ~S" arch))))
            (if-let (h-name (find-local-spec name spec-path))
              h-name
              (error "Error finding spec for ~S after running c2ffi" name)))))))
