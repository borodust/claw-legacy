(cl:in-package :claw.spec)


(defclass foreign-alias-template (foreign-entity) ())


(defclass foreign-type-alias (foreign-entity) ())


(defclass foreign-var-template (foreign-entity) ())


(defmethod parse-form (form (tag (eql :unhandled)))
  (alist-bind (name kind location) form
    (when-let ((entity (switch (kind :test #'string=)
                         ("TypeAliasTemplate" (make-instance 'foreign-alias-template
                                                             :name name
                                                             :type name
                                                             :location location))
                         ("TypeAlias" (make-instance 'foreign-type-alias
                                                     :name name
                                                     :type name
                                                     :location location))
                         ("VarTemplate" (make-instance 'foreign-var-template
                                                       :name name
                                                       :type name
                                                       :location location))
                         ("Using" nil)
                         ("UsingShadow" nil)
                         ("UsingDirective" nil)
                         ("UnresolvedUsingValue" nil)
                         (t (error "Unrecognized unhandled tag: ~A" )))))
      (register-foreign-entity name entity)
      name)))


(defmethod parse-form (form (tag (eql :other)))
  #++(warn "Skipping other ~A" form))
