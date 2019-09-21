(cl:in-package :claw.spec)

(defparameter +json-escaped-chars+
  '((#\" . #\")
    (#\\ . #\\)
    (#\Backspace . #\b)
    (#\ . #\f)
    (#\Newline . #\n)
    (#\Return . #\r)
    (#\Tab . #\t)))


(declaim (inline akey aval (setf aval)))
(defun akey (val alist &key (test 'eql)) (car (rassoc val alist :test test)))
(defun aval (key alist &key (test 'eql)) (cdr (assoc key alist :test test)))
(defun (setf aval) (value key alist &key (test 'eql))
  (setf (cdr (assoc key alist :test test)) value))

(defmacro alist-bind ((&rest vars) alist &body body)
  (once-only (alist)
    `(let (,@(mapcar (lambda (x)
                       (if (consp x)
                           `(,(car x) (aval ,(cadr x) ,alist))
                           `(,x (aval ,(make-keyword x) ,alist))))
                     vars))
       ,@body)))


(defun alist (&rest args &key &allow-other-keys)
  (plist-alist args))


(defmethod json:encode-json ((object (eql 'true)) &optional (stream json:*json-output*))
  (format stream "true"))


(defmethod json:encode-json ((object (eql 'false)) &optional (stream json:*json-output*))
  (format stream "false"))


(defun encode-json-string (string stream)
  (write-char #\" stream)
  (loop for char across string
        for special = (assoc-value +json-escaped-chars+ char)
        if special
          do (write-char #\\ stream)
             (write-char special stream)
        else
          do (write-char char stream))
  (write-char #\" stream))


(defmethod json:encode-json ((object string) &optional (stream json:*json-output*))
  (encode-json-string object stream))


(defun decode-json (stream)
  (let ((*read-default-float-format* 'double-float))
    (json:decode-json stream)))


(defun encode-json (object stream)
  (json:encode-json object stream))
