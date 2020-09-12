(cl:in-package :claw.wrapper)


(defun generate-default-header-name (symbol)
  (format nil "~A.h" (substitute #\_ #\- (string-downcase (symbol-name symbol)))))


(defstruct wrapper-options
  system
  base-path
  bindings-path
  persistent

  language
  standard
  parser
  generator

  headers
  includes
  framework-includes
  targets
  defines
  instantiations

  include-sources
  include-definitions
  exclude-sources
  exclude-definitions)


(defstruct wrapper
  name
  options
  configuration
  entities)


(defun merge-wrapper-pathname (pathname wrapper)
  (map-path pathname))


(defun predefined-targets (&key (linux "gnu") (windows "msvc") (darwin "gnu"))
  `(((:and :x86-64 :linux) . ,(string+ "x86_64-pc-linux-" linux))
    ((:and :x86 :linux) . ,(string+ "i686-pc-linux-" linux))
    ((:and :x86-64 :windows) . ,(string+ "x86_64-pc-windows-" windows))
    ((:and :x86-64 :windows) . ,(string+ "i686-pc-windows-" windows))
    ((:and :x86-64 :darwin) . ,(string+ "x86_64-apple-darwin-" darwin))
    ((:and :x86-64 :darwin) . ,(string+ "i686-apple-darwin-" darwin))))


(defun eval-targets (targets)
  (loop for (features target) in targets
        collect (cons features (eval target))))


(defun eval-opts (name opts)
  (destructuring-bind (&key
                         system
                         base-path
                         bindings-path
                         (persistent '(t))

                         language
                         standard
                         parser
                         generator

                         headers
                         includes
                         framework-includes
                         (targets '(:native))
                         defines
                         instantiations

                         include-sources include-definitions
                         exclude-sources exclude-definitions)
      (alist-plist opts)
    (with-evaluated-variables (base-path
                               language
                               standard
                               parser
                               generator
                               bindings-path
                               persistent)
      (with-evaluated-lists (headers
                             includes
                             include-sources
                             include-definitions
                             exclude-sources
                             exclude-definitions
                             defines
                             instantiations)
        (let* ((system (or (first system) (when (asdf:find-system name nil) name)))
               (base-path (when base-path
                            (find-path base-path :system system)))
               (path-mapper (lambda (path)
                              (find-path path :system system :path base-path)))
               (bindings-path (uiop:ensure-directory-pathname
                               (funcall path-mapper (or bindings-path "bindings/"))))
               (parser (or parser :claw/resect))
               (headers (or headers (list (generate-default-header-name name))))
               (targets (case (first targets)
                          (:local `((t . ,(local-platform))))
                          (:gnu (predefined-targets :linux "gnu"
                                                    :windows "gnu"
                                                    :darwin "gnu"))
                          (:native (predefined-targets))
                          (t (eval-targets targets))))
               (includes (mapcar path-mapper
                                 (append
                                  (list nil)
                                  includes
                                  (list-all-known-include-paths))))
               (framework-includes (mapcar path-mapper
                                           (append
                                            (list nil)
                                            framework-includes
                                            (list-all-known-framework-paths)))))
          (make-wrapper-options :system system
                                :base-path base-path
                                :bindings-path bindings-path
                                :persistent persistent

                                :language language
                                :standard standard
                                :parser parser
                                :generator generator

                                :headers headers
                                :includes includes
                                :framework-includes framework-includes
                                :targets targets

                                :include-sources include-sources
                                :include-definitions include-definitions
                                :exclude-sources exclude-sources
                                :exclude-definitions exclude-definitions
                                :defines defines
                                :instantiations instantiations))))))


(defun make-bindings-table (name opts configuration)
  (loop with table = (make-hash-table :test 'equal)
        for (nil . target) in (wrapper-options-targets opts)
        for library = (describe-foreign-library
                       (wrapper-options-parser opts)
                       (wrapper-options-headers opts)
                       :language (wrapper-options-language opts)
                       :standard (wrapper-options-standard opts)
                       :includes (wrapper-options-includes opts)
                       :framework-includes (wrapper-options-framework-includes opts)
                       :target target
                       :defines (wrapper-options-defines opts)
                       :instantiations (wrapper-options-instantiations opts)
                       :include-sources (wrapper-options-include-sources opts)
                       :include-definitions (wrapper-options-include-definitions opts)
                       :exclude-sources (wrapper-options-exclude-sources opts)
                       :exclude-definitions (wrapper-options-exclude-definitions opts))
        for selected-language = (or (wrapper-options-language opts)
                                    (foreign-library-language library))
        for selected-generator = (or (wrapper-options-generator opts)
                                     (ecase selected-language
                                       (:c :claw/cffi)
                                       (:c++ :claw/iffi)))
        for entities = (foreign-library-entities library)
        do (setf (gethash target table) (generate-bindings selected-generator
                                                           selected-language
                                                           (make-wrapper :name name
                                                                         :options opts
                                                                         :configuration configuration
                                                                         :entities entities)
                                                           configuration))
        finally (return table)))


(defun persist-and-load-bindings (opts bindings-table)
  (let (selected-target)
    (flet ((%bindings-file (target)
             (merge-pathnames (format nil "~A.lisp" target)
                              (wrapper-options-bindings-path opts))))
      (loop for (features . target) in (wrapper-options-targets opts)
            for bindings = (gethash target bindings-table)
            for bindings-file = (%bindings-file target)
            when (or (null selected-target)
                     (eq t features)
                     (uiop:featurep features))
              do (setf selected-target target)
            unless (uiop:file-exists-p bindings-file)
              do (with-open-file (out bindings-file
                                      :direction :output
                                      :external-format :utf-8)
                   (let ((*print-pretty* t)
                         (*print-case* :downcase)
                         (*print-circle* nil)
                         (*package* (find-package :%claw.wrapper.pristine)))
                     (loop for binding in bindings
                           do (prin1 binding out)
                              (fresh-line out)
                              (terpri out)))))
      (when selected-target
        `(load ,(%bindings-file selected-target))))))


(defun expand-bindings (opts bindings-table)
  `(progn
     ,@(loop for (features . target) in (wrapper-options-targets opts)
             when (or (eq t features)
                      (uiop:featurep features))
               return (gethash target bindings-table))))


(defmacro defwrapper (name-and-opts &body configuration)
  (destructuring-bind (name &rest opts) (ensure-list name-and-opts)
    (let* ((opts (eval-opts name opts))
           (*path-mapper* (lambda (path)
                            (find-path path :system (wrapper-options-system opts)
                                            :path (wrapper-options-base-path opts))))
           (bindings-table (make-bindings-table name opts configuration)))
      (if (wrapper-options-persistent opts)
          (persist-and-load-bindings opts bindings-table)
          (expand-bindings opts bindings-table)))))


(defmacro include (path-or-paths &key in-package)
  (with-gensyms (name)
    `(defwrapper (,name
                  (:headers ,@(ensure-list path-or-paths))
                  (:targets :local)
                  (:persistent nil))
       :in-package ,in-package)))
