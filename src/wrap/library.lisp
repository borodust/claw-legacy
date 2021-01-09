(cl:in-package :claw.wrapper)

(declaim (special *path-mapper*))


(defun map-path (path)
  (funcall *path-mapper* path))


(defgeneric generate-bindings (generator language wrapper configuration))

(defgeneric bindings-definition (bindings))
(defgeneric unexport-bindings (bindings))
(defgeneric reexport-bindings (bindings))
(defgeneric bindings-required-systems (bindings))
(defgeneric bindings-required-packages (bindings))

(defgeneric describe-foreign-library (parser headers
                                      &key
                                        includes
                                        frameworks
                                        language
                                        standard
                                        target
                                        defines
                                        intrinsics

                                        include-definitions
                                        include-sources
                                        exclude-definitions
                                        exclude-sources
                                      &allow-other-keys))

(defgeneric foreign-library-entities (library))
(defgeneric foreign-library-language (library))
