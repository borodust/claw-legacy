(cl:in-package :claw.wrapper)

(declaim (special *path-mapper*))


(defun map-path (path)
  (funcall *path-mapper* path))


(defgeneric generate-bindings (generator language wrapper configuration))

(defgeneric describe-foreign-library (parser headers
                                      &key
                                        includes
                                        frameworks
                                        language
                                        standard
                                        target
                                      &allow-other-keys))

(defgeneric foreign-library-entities (library))
(defgeneric foreign-library-language (library))
