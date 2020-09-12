(cl:in-package :claw.resect)


(defgeneric inspect-declaration (inspector kind declaration))

(defgeneric prepare-inspector (inspector unit)
  (:method (inspector unit)
    (declare (ignore inspector unit))))


(defun inspect-foreign-library (inspector header-path includes frameworks language standard target)
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
                                   :target (%stringify target))
      (prepare-inspector inspector unit)
      (resect:docollection (decl (%resect:translation-unit-declarations unit))
        (inspect-declaration inspector (%resect:declaration-kind decl) decl)))))
