(cl:in-package :claw)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (let (unsigned signed)
    (map nil
         (lambda (x)
           (push (cons (foreign-type-size x) x) unsigned))
         '(:unsigned-char :unsigned-short :unsigned-int
           :unsigned-long :unsigned-long-long))
    (map nil
         (lambda (x)
           (push (cons (foreign-type-size x) x) signed))
         '(:char :short :int :long :long-long))
    (define-foreign-type 'int8 (aval 1 signed))
    (define-foreign-type 'uint8 (aval 1 unsigned))
    (define-foreign-type 'int16 (aval 2 signed))
    (define-foreign-type 'uint16 (aval 2 unsigned))
    (define-foreign-type 'int32 (aval 4 signed))
    (define-foreign-type 'uint32 (aval 4 unsigned))
    (define-foreign-type 'int64 (aval 8 unsigned))
    (define-foreign-type 'uint64 (aval 8 unsigned))
    #+x86-64
    (define-foreign-type 'size-t 'uint64)
    #+x86
    (define-foreign-type 'size-t 'uint32)
    #+arm
    (define-foreign-type 'size-t 'uint32))

  (define-foreign-function '(c-malloc "malloc") :pointer
    '((size size-t)))

  (define-foreign-function '(c-calloc "calloc") :pointer
    '((nmemb size-t)
      (size size-t)))

  (define-foreign-function '(c-free "free") :void
    '((ptr :pointer)))

  (define-foreign-function '(c-realloc "realloc") :pointer
    '((ptr :pointer)
      (size size-t)))

  (define-foreign-function '(c-memset "memset") :pointer
    '((s :pointer)
      (c :int)
      (n size-t)))

  (define-foreign-function '(c-memcpy "memcpy") :pointer
    '((dest :pointer)
      (src :pointer)
      (n size-t)))

  (define-foreign-function '(c-memcmp "memcmp") :int
    '((src0 :pointer)
      (src1 :pointer)
      (n size-t))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-cfun c-malloc)
  (define-cfun c-calloc)
  (define-cfun c-free)
  (define-cfun c-realloc)
  (define-cfun c-memset)
  (define-cfun c-memcpy)
  (define-cfun c-memcmp))

 ;; Allocating things

(defun alloc-ptr (type &optional (count 1))
  "Return a pointer allocated to the size of `TYPE`"
  (c-malloc
   (* count (foreign-type-size
             (require-type type "allocate an instance of foreign type ~S" type)))))

(defun calloc-ptr (type &optional (count 1))
  "Return a pointer allocated to the size of `TYPE`, initialized to zero"
  (c-calloc
   count
   (foreign-type-size
    (require-type type "allocate an instance of foreign type ~S" type))))

(defun alloc (type &optional (count 1))
  "Return a pointer allocated to the size of `TYPE`"
  (alloc-ptr type count))

(defun calloc (type &optional (count 1))
  "Return a pointer allocated to the size of `TYPE`, initialized to zero"
  (calloc-ptr type count))

(defun realloc (ptr type count)
  (let ((size (foreign-type-size type)))
    (c-realloc ptr (* count size))))

(defun free (object)
  "Free memory allocated at pointer"
  (c-free (ptr object))
  (values))

(declaim (inline alignof))
(defun alignof (type)
  (foreign-type-alignment (find-type type)))

(declaim (inline sizeof))
(defun sizeof (type)
  (foreign-type-size (find-type type)))

(defun offsetof (struct field)
  (if-let ((type (basic-foreign-type (find-type struct))))
    (progn
      (check-type type foreign-record)
      (if-let ((record-field (find-record-field type field)))
        (values (truncate (frf-bit-offset record-field) +byte-size+))
        (error 'c-unknown-field :type type :field field)))
    (error 'undefined-foreign-type :typespec struct)))

(defmacro with-alloc ((name type &optional (count 1)) &body body)
  `(let ((,name (alloc ,type ,count)))
     (unwind-protect (progn ,@body)
       (free ,name))))

(defmacro with-free ((name value) &body body)
  `(let ((,name ,value))
     (unwind-protect (progn ,@body)
       (free ,name))))

(defmacro with-calloc ((name type &optional (count 1)) &body body)
  `(let ((,name (calloc ,type ,count)))
     (unwind-protect (progn ,@body)
       (free ,name))))

(defmacro with-many-alloc ((&rest bindings) &body body)
  `(let* ,(mapcar #'(lambda (bind) `(,(car bind) (alloc ,@(cdr bind))))
          bindings)
     (unwind-protect (progn ,@body)
       ,@(mapcar #'(lambda (bind) `(free ,(car bind))) bindings))))

(defmacro with-many-free ((&rest bindings) &body body)
  `(let* ,(mapcar #'(lambda (bind) `(,(car bind) ,@(cdr bind)))
          bindings)
     (unwind-protect (progn ,@body)
       ,@(mapcar #'(lambda (bind) `(free ,(car bind))) bindings))))

(defun memcpy (dest src &optional (count 1) (type 'int8))
  (c-memcpy (ptr dest) (ptr src) (* count (sizeof type))))

(defun memcmp (this that &optional (count 1) (type 'int8))
  (c-memcmp (ptr this) (ptr that) (* count (sizeof type))))

(defun alloc-string (string)
  (let ((ptr (calloc :char (1+ (length string)))))
    (cffi:lisp-string-to-foreign string ptr (1+ (length string)))
    ptr))
