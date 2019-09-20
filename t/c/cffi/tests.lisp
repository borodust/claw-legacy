(cl:in-package :claw.tests)
(5am:in-suite :claw.tests)

(declaim (special *values*))

(5am:test version
  (5am:is (equal "1.0.0" (cffi:foreign-string-to-lisp
                          (%libctest:version-string)))))


(5am:test named-node-creation
  (c-let ((node (:struct %libctest:node-t) :alloc t))
    (%libctest:create-named-node (node &) "YO")
    (unwind-protect
         (5am:is (equal "YO"
                        (cffi:foreign-string-to-lisp
                         (node (:data (:pointer (:struct %libctest:named-node-t))) *
                               :name &))))
      (%libctest:destroy-node (node &)))))


(5am:test color-node-creation
  (c-let ((node (:struct %libctest:node-t) :alloc t))
    (c-with ((color (:union %libctest:color-t)))
      (setf (color :encoded) %libctest:+white+)
      (%libctest:create-colored-node (node &) (color &)))
    (unwind-protect
         (c-let ((colored (:struct %libctest:colored-node-t) :from (node :data)))
           (5am:is (and (equal (colored :color :component :r) 255)
                        (equal (colored :color :component :g) 255)
                        (equal (colored :color :component :b) 255)
                        (equal (colored :color :component :a) 255))))
      (%libctest:destroy-node (node &)))))


(cffi:defcallback visit-node :void ((node-ptr (:pointer (:struct %libctest:node-t))))
  (push (ecase (%libctest:get-node-kind node-ptr)
          (:named (cffi:foreign-string-to-lisp
                   (%libctest:get-node-name node-ptr)))
          (:colored (c-with ((color (:union %libctest:color-t)))
                      (%libctest:get-node-color (color &) node-ptr)
                      (list (color :component :r)
                            (color :array 1)
                            (color :component :b)
                            (color :array 3)))))
        *values*))


(5am:test tree-manipulation
  (let ((root (%libctest:create-named-node (cffi:foreign-alloc
                                            '(:struct %libctest:node-t))
                                           "ROOT")))
    (c-let ((tree %libctest:tree-t :from (%libctest:create-tree root)))
      (unwind-protect
           (progn
             (5am:is (equal 1 (tree :info :node-count)))
             (c-with ((color (:union %libctest:color-t) :clear t))
               (setf (color :array 0) 0
                     (color :array 1) 1
                     (color :array 2) 2
                     (color :array 3) 3)
               (c-with ((colored (:struct %libctest:node-t)))
                 (%libctest:create-colored-node (colored &) (color &))
                 (%libctest:add-child (tree :root &) (colored &))
                 (5am:is (equal 2 (tree :info :node-count)))))
             (let (*values*)
               (%libctest:visit-tree-nodes tree (cffi:callback visit-node))
               (5am:is (equal '((0 1 2 3) "ROOT") *values*))))
        (%libctest:destroy-tree tree)))))
