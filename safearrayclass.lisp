(in-package :safearray)

(defclass safe-array ()
  ((vt-type :accessor vt-type :initarg :vt-type)
   (bounds :initarg :bounds)
   (foreign-safe-array)))

(defmethod initialize-instance :after ((self safe-array) &key)
  (let ((foreign-safe-array (safe-array-create (vt-type self) (slot-value self 'bounds))))
    (safe-array-lock foreign-safe-array)
    (setf (slot-value self 'foreign-safe-array) foreign-safe-array)
    (trivial-garbage:finalize self #'(lambda ()
				       (safe-array-unlock foreign-safe-array)
				       (unwind-protect
					    (safe-array-destroy foreign-safe-array))))))

(defmacro define-simple-safe-array-method (name &body body)
  `(defmethod ,name ((self safe-array))
     (symbol-macrolet ((foreign-safe-array (slot-value self 'foreign-safe-array)))
       ,@body)))

(define-simple-safe-array-method get-bounds
  (safe-array-get-bounds foreign-safe-array))

(defmethod get-element ((self safe-array) indices foreign-type)
  (safe-array-get-element (slot-value self 'foreign-safe-array) foreign-type indices))

(define-simple-safe-array-method get-vartype
  (safe-array-get-vartype foreign-safe-array))

(define-simple-safe-array-method access-data
  (safe-array-access-data foreign-safe-array))

(define-simple-safe-array-method unaccess-data
    (safe-array-unaccess-data foreign-safe-array))

(define-simple-safe-array-method get-element-size
  (safe-array-get-elem-size foreign-safe-array))

(define-simple-safe-array-method lock
  (safe-array-lock foreign-safe-array))

(define-simple-safe-array-method unlock
  (safe-array-unlock foreign-safe-array))

(define-simple-safe-array-method get-dimensions
  (safe-array-get-dim foreign-safe-array))

;;; ssafe-array-ptr-of-index safe-array-put-element

(defmethod ptr-of-index ((self safe-array) indices)
  (safe-array-ptr-of-index (slot-value self 'foreign-safe-array) indices))

(defmethod print-object ((self safe-array) stream)
  (print-unreadable-object (self stream)
    (format stream "safe-array@#X~X~%" (cffi:pointer-address (slot-value self 'foreign-safe-array)))
    (format stream "vt-type: ~a~%" (get-vartype self))
    (format stream "bounds: ~a" (get-bounds self))))
