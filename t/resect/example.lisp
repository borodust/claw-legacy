(cl:in-package :claw.tests)


(defun test-resect (path)
  #++(claw.resect:with-cursor-locations (location)
       (claw.resect:with-resect-strings (debug-info resect-string)
         (claw.resect:with-resect-types (type)
           (flet ((visit (current parent)
                    (declare (ignore parent))
                    (claw.resect:get-cursor-location location current)
                    (claw.resect:get-cursor-comment resect-string current)
                    (claw.resect:get-cursor-type type current)
                    (claw.resect:get-cursor-debug-info debug-info current)
                    (format t "~&~%~A ~A:~A:~A"
                            (claw.resect:cursor-kind current)
                            (claw.resect:cursor-location-name location)
                            (claw.resect:cursor-location-line location)
                            (claw.resect:cursor-location-column location))
                    (format t "~&Comment: ~A"
                            (claw.resect:resect-string-to-lisp resect-string))
                    (format t "~&Size: ~A~&Alignment: ~A"
                            (claw.resect:resect-type-size type)
                            (claw.resect:resect-type-alignment type))
                    (format t "~&Debug: ~A"
                            (claw.resect:resect-string-to-lisp debug-info))))
             (claw.resect:parse path #'visit))))))
