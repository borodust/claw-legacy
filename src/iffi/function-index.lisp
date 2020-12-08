(cl:in-package :iffi)


(defstruct function-index
  (function-table (make-hash-table :test 'equal))
  (argument-table (make-hash-table :test 'equal))
  (alias-table (make-hash-table :test 'equal)))


(defvar *function-index* (make-function-index))


(defun find-alias-node (name)
  (gethash name (function-index-alias-table *function-index*)))

(defun register-alias-node (name parent children)
  (setf
   (gethash name (function-index-alias-table *function-index*))
   (list* parent children)))


(defun alias-node-parent (node)
  (first node))

(defun alias-node-children (node)
  (rest node))

(defun add-alias-node-child (node child)
  (pushnew child (rest node) :test #'equal)
  node)

(defun update-alias-node-parent (node new-parent)
  (setf (first node) new-parent)
  node)


(defun find-base-type (type)
  (if (and (listp type)
           (eq :pointer (first type)))
      (list :pointer (find-base-type (second type)))
      (let* ((node (find-alias-node type))
             (parent (and node (alias-node-parent node))))
        (if parent
            (find-base-type parent)
            type))))


(defun find-intricate-aliases (name)
  (labels ((%collect-aliases (id)
             (let ((node (find-alias-node id)))
               (list* id (loop for child in (alias-node-children node)
                               append (%collect-aliases child))))))
    (%collect-aliases (find-base-type name))))


(defun maintain-base-functions (current-base new-base)
  (let* ((new-base (find-base-type new-base))
         (fids (gethash current-base (function-index-argument-table *function-index*))))
    (loop for fid in fids
          for function = (gethash fid (function-index-function-table *function-index*))
          for new-fid = (list* (first fid) (nsubstitute new-base current-base (rest fid) :test #'equal))
          do (remhash fid (function-index-function-table *function-index*))
             (setf (gethash new-fid (function-index-function-table *function-index*)) function)
          collect new-fid into new-fids
          finally (remhash current-base (function-index-argument-table *function-index*))
                  (setf
                   (gethash new-base (function-index-argument-table *function-index*))
                   new-fids))))


(defun register-intricate-alias (base alias)
  (labels ((%register-alias (name base)
             (register-alias-node name base nil))
           (%register-base (name &rest children)
             (register-alias-node name nil children)))
    (let ((base-node (gethash base (function-index-alias-table *function-index*)))
          (alias-node (gethash alias (function-index-alias-table *function-index*))))
      (cond
        ((not (or base-node alias-node))
         (%register-base base alias)
         (%register-alias alias base)
         (maintain-base-functions alias base))
        ((and base-node alias-node)
         (unless (and (equal (alias-node-parent alias-node) base)
                      (member alias (alias-node-children base-node) :test #'equal))
           (error "Alias ~A exists: current base ~A" alias (find-base-type alias))))
        (base-node
         (%register-alias alias base)
         (add-alias-node-child base-node alias)
         (maintain-base-functions alias base))
        (alias-node
         (if (alias-node-parent alias-node)
             (error "Alias ~A exists: current base ~A" alias (find-base-type alias))
             (progn
               (%register-base base alias)
               (update-alias-node-parent alias-node base)
               (maintain-base-functions alias base))))))))


(defun intricate-function (name &rest arg-types)
  (let ((function-table (function-index-function-table *function-index*)))
    (gethash (list* name (mapcar #'find-base-type arg-types)) function-table)))


(defun (setf intricate-function) (value name &rest arg-types)
  (let* ((function-table (function-index-function-table *function-index*))
         (base-types (mapcar #'find-base-type arg-types))
         (function-id (list* name base-types)))
    (loop for base in base-types
          do (push function-id (gethash base (function-index-argument-table *function-index*))))
    (setf (gethash function-id function-table) value)))
