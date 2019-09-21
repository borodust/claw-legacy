(cl:in-package :claw.wrapper)


(defstruct (wrapper
            (:constructor make-wrapper (name
                                        headers
                                        specification
                                        standard
                                        includes
                                        last-update-time
                                        path-mapper)))
  (name nil :read-only t)
  (headers nil :read-only t)
  (specification nil :read-only t)
  (standard nil :read-only t)
  (includes nil :read-only t)
  (last-update-time 0 :read-only t)
  (path-mapper nil :read-only t))


(defun merge-wrapper-pathname (pathname wrapper)
  (let ((*path-mapper* (wrapper-path-mapper wrapper)))
    (map-path pathname)))


(defun generate-default-header-name (symbol)
  (format nil "~A.h" (substitute #\_ #\- (string-downcase (symbol-name symbol)))))


(defmacro defwrapper (name-and-opts &body configuration)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (destructuring-bind (&key
                           system
                           base-path
                           spec-path
                           language
                           standard
                           generator
                           windows-environment
                           headers
                           includes
                           framework-includes
                           arch-includes
                           include-sources include-definitions
                           exclude-sources exclude-definitions)
        (alist-plist opts)
      (with-evaluated-variables (base-path
                                 language
                                 standard
                                 generator
                                 windows-environment
                                 spec-path)
        (with-evaluated-lists (headers
                               includes
                               include-sources
                               include-definitions
                               exclude-sources
                               exclude-definitions
                               arch-includes)
          (let* ((system (or (first system) (when (asdf:find-system name nil) name)))
                 (base-path (when base-path
                              (find-path base-path :system system)))
                 (*path-mapper* (lambda (path)
                                  (find-path path :system system :path base-path)))
                 (spec-path (map-path (uiop:ensure-directory-pathname
                                       (or spec-path "spec/"))))
                 (language (or language :c))
                 (generator (or generator :claw/cffi))
                 (windows-environment (or windows-environment "gnu"))
                 (includes (mapcar #'map-path
                                   (append
                                    (list nil)
                                    includes
                                    (list-all-known-include-paths))))
                 (headers (or headers (list (generate-default-header-name name)))))
            (multiple-value-bind (spec last-update-time)
                (claw.spec:describe-foreign-library
                 (symbol-name name)
                 headers
                 :spec-path spec-path
                 :language language
                 :standard standard
                 :includes includes
                 :framework-includes (mapcar #'map-path
                                             (append
                                              (list nil)
                                              framework-includes
                                              (list-all-known-framework-paths)))
                 :include-sources include-sources
                 :include-definitions include-definitions
                 :exclude-sources exclude-sources
                 :exclude-definitions exclude-definitions
                 :arch-includes (append
                                 (list "x86_64-pc-linux-gnu"
                                       "i686-pc-linux-gnu"
                                       (concatenate 'string "x86_64-pc-windows-"
                                                    windows-environment)
                                       (concatenate 'string "i686-pc-windows-"
                                                    windows-environment)
                                       "x86_64-apple-darwin-gnu"
                                       "i686-apple-darwin-gnu")
                                 arch-includes))
              (expand-library-definition generator language
                                         (make-wrapper name headers spec
                                                       standard includes
                                                       last-update-time
                                                       *path-mapper*)
                                         configuration))))))))
