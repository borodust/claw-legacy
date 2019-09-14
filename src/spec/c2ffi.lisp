(cl:in-package :claw.spec)


(defvar *c2ffi-program* #-windows "c2ffi" #+windows "c2ffi.exe")


(defun run-check (program args &key output error-output ignore-error-status)
  (when (uiop:featurep :claw-trace-c2ffi)
    (format *debug-io* "~&; Invoking: ~A~{ ~A~}~%" program args))
  (zerop (nth-value 2 (uiop:run-program (list* program args)
                                        :output output
                                        :error-output error-output
                                        :ignore-error-status ignore-error-status))))

(defun c2ffi-p ()
  "This is a hack to determine if c2ffi exists; it assumes if it
doesn't exist, we will get a return code other than 0."
  (zerop (nth-value 2 (uiop:run-program `(,*c2ffi-program* "-h") :ignore-error-status t))))


;;; UIOP:WITH-TEMPORARY-FILE does not seem to compile below as of asdf
;;; 3.1.5, and that's what SBCL is distributed with, so.
(defmacro with-temporary-file ((&key pathname keep (stream (gensym "STREAM") streamp)) &body body)
  `(uiop:call-with-temporary-file
    (lambda (,stream ,pathname)
      (unless ,streamp (close ,stream))
      ,@body)
    :keep ,keep))


(defun prepare-includes (includes option)
  (loop for dir in includes
        if (uiop:directory-exists-p dir)
          append (list option (namestring dir))
        else
          do (when (uiop:featurep :claw-trace-c2ffi)
               (format *debug-io* "~&; c2ffi include ignored: ~A not found" dir))))


(defun run-c2ffi (input-file output-basename &key arch
                                               includes ignore-error-status
                                               framework-includes
                                               language standard)
  "Run c2ffi on `INPUT-FILE`, outputting to `OUTPUT-FILE` and
`MACRO-OUTPUT-FILE`, optionally specifying a target triple `ARCH`."
  (with-temporary-file (:pathname tmp-macro-file
                        :keep (uiop:featurep :claw-trace-c2ffi))
    (let* ((output-spec (namestring output-basename))
           (arch (when arch (list "-A" arch)))
           (includes (prepare-includes includes "-i"))
           (framework-includes (prepare-includes framework-includes "-F"))
           (common-arg-list (append (when language
                                      (list "--lang" (string-downcase
                                                      (string language))))
                                    (when standard
                                      (list "--std" (string-downcase
                                                     (string standard))))
                                    arch includes framework-includes)))
      (ensure-directories-exist output-spec)
      ;; Invoke c2ffi to emit macros into TMP-MACRO-FILE
      (when (run-check *c2ffi-program* (list* (namestring input-file)
                                              "-D" "null"
                                              "-M" (namestring tmp-macro-file)
                                              common-arg-list)
                       :output *standard-output*
                       :error-output (when (uiop:featurep :claw-spit-c2ffi-errors)
                                       *error-output*)
                       :ignore-error-status ignore-error-status)
        ;; Write a tmp header file that #include's the input file and the macros file.
        (with-temporary-file (:stream tmp-include-file-stream
                              :pathname tmp-include-file
                              :keep (uiop:featurep :claw-trace-c2ffi))
          (format tmp-include-file-stream "#include \"~A\"~%" input-file)
          (format tmp-include-file-stream "#include \"~A\"~%" tmp-macro-file)
          (close tmp-include-file-stream)
          ;; Invoke c2ffi again to generate the raw output.
          (run-check *c2ffi-program* (list* (namestring tmp-include-file)
                                            "-o" (namestring output-spec)
                                            common-arg-list)
                     :output *standard-output*
                     :ignore-error-status ignore-error-status))))))


(defun for-encoded-specification (action input-file &key arch
                                                      includes
                                                      ignore-error-status
                                                      framework-includes
                                                      language standard)
  "Run c2ffi on `INPUT-FILE`, outputting to `OUTPUT-FILE` and
`MACRO-OUTPUT-FILE`, optionally specifying a target triple `ARCH`."
  (with-temporary-file (:pathname tmp-spec-file)
    (when (run-c2ffi input-file tmp-spec-file
                     :arch arch
                     :includes includes
                     :framework-includes framework-includes
                     :language language
                     :standard standard
                     :ignore-error-status ignore-error-status)
      (with-open-file (stream tmp-spec-file)
        (funcall action stream arch))
      t)))


(defun write-include-header (path includes)
  (with-open-file (stream path :direction :output
                               :external-format :utf-8
                               :if-exists :supersede)
    (loop for include in includes
          do (format stream "#include \"~A\"~&" include))))


(defmacro with-include-header ((header includes) &body body)
  `(with-temporary-file (:pathname ,header
                         :keep (uiop:featurep :claw-trace-c2ffi))
     (write-include-header ,header ,includes)
     ,@body))


(defun for-encoded-specifications (action headers &key
                                                    arch-includes
                                                    includes
                                                    framework-includes
                                                    language
                                                    standard)
  (with-include-header (header headers)
    (unless (c2ffi-p)
      (error "c2ffi not found"))
    (loop for arch in arch-includes
          do (unless (for-encoded-specification
                      action header
                      :arch arch
                      :includes includes
                      :framework-includes framework-includes
                      :ignore-error-status t
                      :language language
                      :standard standard)
               (warn "Error generating spec for arch ~S" arch)))))


(defmacro do-encoded-library-specifications (((stream arch) headers
                                              &key
                                                arch-includes
                                                includes
                                                framework-includes
                                                language
                                                standard) &body body)
  `(for-encoded-specifications (lambda (,stream ,arch) ,@body)
                               ,headers
                               :arch-includes ,arch-includes
                               :includes ,includes
                               :framework-includes ,framework-includes
                               :language ,language
                               :standard ,standard))
