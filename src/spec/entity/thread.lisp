(cl:in-package :claw.spec)


(defmethod parse-form (form (tag (eql :pthread_t)))
  (register-primitive-type-renaming form ":pthread-t"))


(defmethod parse-form (form (tag (eql :pthread_key_t)))
  (register-primitive-type-renaming form ":pthread-key-t"))


(defmethod parse-form (form (tag (eql :pthread_once_t)))
  (register-primitive-type-renaming form ":pthread-once-t"))


(defmethod parse-form (form (tag (eql :pthread_mutex_t)))
  (register-primitive-type-renaming form ":pthread-mutex-t"))


(defmethod parse-form (form (tag (eql :pthread_cond_t)))
  (register-primitive-type-renaming form ":pthread-cond-t"))


(defmethod parse-form (form (tag (eql :pthread_attr_t)))
  (register-primitive-type-renaming form ":pthread-attr-t"))


(defmethod parse-form (form (tag (eql :pthread_mutexattr_t)))
  (register-primitive-type-renaming form ":pthread-mutexattr-t"))


(defmethod parse-form (form (tag (eql :pthread_condattr_t)))
  (register-primitive-type-renaming form ":pthread-condattr-t"))
