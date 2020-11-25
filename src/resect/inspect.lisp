(cl:in-package :claw.resect)


(declaim (special *translation-unit*))

(defgeneric inspect-declaration (inspector kind declaration))

(defgeneric inspect-foreign-library (inspector
                                     header-path
                                     includes
                                     frameworks
                                     language
                                     standard
                                     target
                                     intrinsics
                                     &key &allow-other-keys))

(defmethod inspect-foreign-library :around (inspector
                                            header-path
                                            includes frameworks
                                            language standard target
                                            intrinsics
                                            &key (diagnostics t))
  (declare (ignore inspector))
  (flet ((%stringify (value)
           (when value
             (if (stringp value)
                 value
                 (string-downcase value)))))
    (resect:with-translation-unit (unit (uiop:native-namestring header-path)
                                   :include-paths includes
                                   :framework-paths frameworks
                                   :language (%stringify language)
                                   :standard (%stringify standard)
                                   :target (%stringify target)
                                   :diagnostics diagnostics
                                   :intrinsics intrinsics)
      (let ((*translation-unit* unit))
        (call-next-method)))))


(defmethod inspect-foreign-library (inspector
                                    header-path
                                    includes frameworks
                                    language standard target
                                    intrinsics
                                    &key)
  (declare (ignore header-path includes frameworks language standard target intrinsics))
  (resect:docollection (decl (%resect:translation-unit-declarations *translation-unit*))
    (inspect-declaration inspector (%resect:declaration-kind decl) decl)))
