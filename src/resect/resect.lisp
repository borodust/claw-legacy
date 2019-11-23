(cl:in-package :claw.resect)

(declaim (special *visitor*
                  *value*

                  *strings*
                  *types*
                  *cursors*
                  *locations*))

(defvar *cursor-alist* nil)

(cffi:defcenum visit-result
  (:recurse 0)
  (:break 1)
  (:continue 2))

(cffi:defctype cursor :pointer)
(cffi:defctype resect-string :pointer)
(cffi:defctype parse-options :pointer)
(cffi:defctype cursor-location :pointer)
(cffi:defctype resect-type :pointer)


;;;
;;; TYPE
;;;
(cffi:defcenum type-kind
  (:unknown 0)
  (:void 2)
  (:bool 3)
  (:char-u 4)
  (:unsigned-char 5)
  (:char16 6)
  (:char32 7)
  (:unsigned-short 8)
  (:unsigned-int 9)
  (:unsigned-long 10)
  (:unsigned-long-long 11)
  (:unsigned-int128 12)
  (:char-s 13)
  (:schar 14)
  (:wchar 15)
  (:short 16)
  (:int 17)
  (:long 18)
  (:long-long 19)
  (:int128 20)
  (:float 21)
  (:double 22)
  (:long-double 23)
  (:nullptr 24)
  (:overload 25)
  (:dependent 26)
  (:float128 30)
  (:half 31)
  (:float16 32)
  (:complex 100)
  (:pointer 101)
  (:block-pointer 102)
  (:lvalue-reference 103)
  (:rvalue-reference 104)
  (:record 105)
  (:enum 106)
  (:typedef 107)
  (:function-no-prototype 110)
  (:function-prototype 111)
  (:constant-array 112)
  (:vector 113)
  (:incomplete-array 114)
  (:variable-array 115)
  (:dependent-sized-array 116)
  (:member-pointer 117)
  (:auto 118)
  (:elaborated 119)
  (:attributed 163)
  (:extended-vector 178))


(cffi:defcfun ("resect_allocate_type" %allocate-resect-type) resect-type)
(cffi:defcfun ("resect_free_type" %free-resect-type) :void
  (object resect-type))
(cffi:defcfun ("resect_type_get_kind" %type-kind) type-kind
  (object resect-type))
(cffi:defcfun ("resect_type_get_declaration" %type-declaration) cursor
  (result cursor)
  (object resect-type))
(cffi:defcfun ("resect_type_sizeof" %type-sizeof) :long-long
  (object resect-type))
(cffi:defcfun ("resect_type_alignof" %type-alignof) :long-long
  (object resect-type))
(cffi:defcfun ("resect_type_offsetof" %type-offsetof) :long-long
  (object resect-type)
  (field-name :string))

;;;
;;; STRING
;;;
(cffi:defcfun ("resect_allocate_string" %allocate-resect-string) resect-string
  (initial-capacity :unsigned-int))
(cffi:defcfun ("resect_free_string" %free-resect-string) :void
  (string resect-string))
(cffi:defcfun ("resect_string_to_c" %resect-string-to-c) :string
  (string resect-string))

;;;
;;; CURSOR LOCATION
;;;
(cffi:defcfun ("resect_allocate_cursor_location" %allocate-cursor-location) cursor-location)
(cffi:defcfun ("resect_free_cursor_location" %free-cursor-location) :void
  (location cursor-location))
(cffi:defcfun ("resect_cursor_location_line" %cursor-location-line) :unsigned-int
  (location cursor-location))
(cffi:defcfun ("resect_cursor_location_column" %cursor-location-column) :unsigned-int
  (location cursor-location))
(cffi:defcfun ("resect_cursor_location_name" %cursor-location-name) :string
  (location cursor-location))

;;;
;;; CURSOR
;;;
(cffi:defcenum cursor-kind
  (:unknown 0)
  (:translation-unit 1)
  (:struct 2)
  (:union 3)
  (:class 4)
  (:enum 5)
  (:field 6)
  (:function 7)
  (:variable 8)
  (:parameter 9)
  (:typedef 10)
  (:method 11)
  (:namespace 12)
  (:constructor 13)
  (:destructor 14)
  (:converter 15)
  (:type-reference 16)
  (:template-reference 17)
  (:enum-constant 18))


(cffi:defcfun ("resect_parse" %parse) :void
  (filename :string)
  (visitor :pointer)
  (options parse-options))


(cffi:defcfun ("resect_allocate_cursor" %allocate-cursor) cursor)
(cffi:defcfun ("resect_free_cursor" %free-cursor) :void
  (cursor cursor))
(cffi:defcfun ("resect_cursor_get_location" %cursor-location) cursor-location
  (location cursor-location)
  (cursor cursor))
(cffi:defcfun ("resect_cursor_equal" %cursor-equal) :int
  (this cursor)
  (that cursor))
(cffi:defcfun ("resect_cursor_get_name" %cursor-name) resect-string
  (string resect-string)
  (cursor cursor))
(cffi:defcfun ("resect_cursor_get_comment" %cursor-comment) resect-string
  (string resect-string)
  (cursor cursor))
(cffi:defcfun ("resect_cursor_get_debug_info" %cursor-debug-info) resect-string
  (string resect-string)
  (cursor cursor))
(cffi:defcfun ("resect_cursor_get_kind" %cursor-kind) cursor-kind
  (cursor cursor))
(cffi:defcfun ("resect_cursor_get_id" %cursor-id) resect-string
  (string resect-string)
  (cursor cursor))
(cffi:defcfun ("resect_cursor_get_type" %cursor-type) resect-type
  (cursor-type resect-type)
  (cursor cursor))

;;;
;;; TYPEDEF
;;;
(cffi:defcfun ("resect_cursor_get_aliased_type" %cursor-aliased-type) resect-type
  (result resect-type)
  (cursor cursor))

;;;
;;; ENUM
;;;
(cffi:defcfun ("resect_cursor_get_enum_value" %cursor-enum-value) :long-long
  (cursor cursor))


;;;
;;; ALLOCATOR
;;;
(defgeneric allocator-alloc (allocator))
(defgeneric allocator-free (allocator value))
(defgeneric destroy-allocator (allocator))
(defgeneric acquire-foreign-memory (allocator))
(defgeneric release-foreign-memory (allocator))


(defmacro with-allocated-object ((object) allocator &body body)
  (alexandria:once-only (allocator)
    `(let ((,object (acquire-foreign-object ,allocator)))
       (unwind-protect
            (progn ,@body)
         (release-foreign-object ,allocator)))))


(defclass trivial-allocator ()
  ((free :initform nil)
   (acquired :initform nil)))


(defmethod destroy-allocator ((this trivial-allocator))
  (with-slots (free acquired) this
    (when acquired
      (warn "Allocator resources are not properly released"))
    (loop for object in free
          do (allocator-free this object))
    (loop for object in acquired
          do (allocator-free this object))))


(defmethod acquire-foreign-memory ((this trivial-allocator))
  (with-slots (free acquired) this
    (first
     (push (if free
               (pop free)
               (allocator-alloc this))
           acquired))))

(defmethod release-foreign-memory ((this trivial-allocator))
  (with-slots (free acquired) this
    (push (pop acquired) free)))


(defmacro defallocator (name &body body)
  (destructuring-bind (alloc-form free-form) body
    (alexandria:with-gensyms (this value)
      `(progn
         (defclass ,name (trivial-allocator) ())
         (defmethod allocator-alloc ((,this ,name))
           ,alloc-form)
         (defmethod allocator-free ((,this ,name) ,value)
           (let ((*value* ,value))
             ,free-form))))))


(defallocator string-allocator
  (%allocate-resect-string 0)
  (%free-resect-string *value*))

(defallocator location-allocator
  (%allocate-cursor-location)
  (%free-cursor-location *value*))

(defallocator type-allocator
  (%allocate-resect-type)
  (%free-resect-type *value*))

(defallocator cursor-allocator
  (%allocate-cursor)
  (%free-cursor *value*))


(defmacro with-resect-allocators (&body body)
  `(let ((*strings* (make-instance 'string-allocator))
         (*locations* (make-instance 'location-allocator))
         (*types* (make-instance 'type-allocator))
         (*cursors* (make-instance 'cursor-allocator)))
     (unwind-protect
          (progn ,@body)
       (destroy-allocator *strings*)
       (destroy-allocator *locations*)
       (destroy-allocator *types*)
       (destroy-allocator *cursors*))))

;;;
;;; LISPIFIED
;;;
(defmacro with-many-things ((&rest things) allocator-name &body body)
  `(let (,@(loop for thing in things
                 collect `(,thing (acquire-foreign-memory ,allocator-name))))
     (unwind-protect
          (progn ,@body)
       ,@(mapcar (lambda (thing)
                   (declare (ignore thing))
                   `(release-foreign-memory ,allocator-name))
                 things))))


(defmacro with-cursors ((&rest cursors) &body body)
  `(with-many-things ,cursors *cursors*
     ,@body))


(defmacro with-resect-strings ((&rest strings) &body body)
  `(with-many-things ,strings *strings*
     ,@body))


(defmacro with-cursor-locations ((&rest locations) &body body)
  `(with-many-things ,locations *locations*
     ,@body))


(defmacro with-resect-types ((&rest types) &body body)
  `(with-many-things ,types *types*
     ,@body))


(defun resect-string-to-lisp (string)
  (%resect-string-to-c string))


(defstruct (cursor-location
            (:constructor %make-cursor-location))
  name
  line
  column)


(defun make-cursor-location (location)
  (%make-cursor-location :name (%cursor-location-name location)
                         :line (%cursor-location-line location)
                         :column (%cursor-location-column location)))


(defstruct (cursor
            (:constructor %make-cursor))
  id
  kind
  name
  type
  location
  comment
  debug-info
  aliased-type
  value)


(defstruct (resect-type
            (:constructor %make-resect-type))
  kind
  declaration
  alignment
  size)


(defun make-resect-type (type)
  (with-cursors (declaration)
    (%type-declaration declaration type)
    (%make-resect-type :kind (%type-kind type)
                       :declaration (make-cursor declaration)
                       :alignment (%type-alignof type)
                       :size (%type-sizeof type))))


(defun make-cursor (cursor)
  (alexandria:if-let ((parent-cursor
                       (alexandria:assoc-value *cursor-alist* cursor
                                               :test #'%cursor-equal)))
    parent-cursor
    (with-resect-types (type)
      (with-cursors (declaration)
        (%cursor-type type cursor)
        (%type-declaration declaration type)
        (let* ((parsed-location (with-cursor-locations (location)
                                  (%cursor-location location cursor)
                                  (make-cursor-location location)))
               (parsed-debug-info (with-resect-strings (debug-info)
                                    (%resect-string-to-c
                                     (%cursor-debug-info debug-info cursor))))
               (parsed-comment (with-resect-strings (comment)
                                 (%resect-string-to-c
                                  (%cursor-comment comment cursor))))
               (parsed-id (with-resect-strings (id)
                            (let ((parsed (%resect-string-to-c
                                           (%cursor-id id cursor))))
                              (when (> (length parsed) 0)
                                parsed))))
               (parsed-kind (%cursor-kind cursor))
               (parsed-name (with-resect-strings (name)
                              (%resect-string-to-c
                               (%cursor-name name cursor))))
               (parsed-cursor (%make-cursor :id parsed-id
                                            :kind parsed-kind
                                            :name parsed-name
                                            :location parsed-location
                                            :comment parsed-comment
                                            :debug-info parsed-debug-info))
               (*cursor-alist* (cons (cons cursor parsed-cursor)
                                     *cursor-alist*))
               (parsed-type (make-resect-type type)))
          (setf (cursor-type parsed-cursor) parsed-type)
          (case parsed-kind
            (:typedef
             (setf (cursor-aliased-type parsed-cursor)
                   (with-resect-types (aliased)
                     (make-resect-type (%cursor-aliased-type aliased cursor)))))
            (:enum-constant
             (setf (cursor-value parsed-cursor) (%cursor-enum-value cursor))))
          parsed-cursor)))))


(cffi:defcallback visitor visit-result
    ((current cursor)
     (parent cursor))
  (case (funcall *visitor* (make-cursor current) (make-cursor parent))
    (:abort :break)
    (:skip :continue)
    (:continue :recurse)
    (t :recurse)))


(defun parse (filename visitor)
  (let ((*visitor* visitor))
    (with-resect-allocators
      (%parse (if (cffi:pointerp filename)
                  filename
                  (uiop:native-namestring filename))
              (cffi:callback visitor) (cffi:null-pointer)))))
