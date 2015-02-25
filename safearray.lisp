;;;; safearray.lisp

(in-package #:safearray)

;;; "safearray" goes here. Hacks and glory await!

(cffi:defctype hresult :int)

(defun succeeded (hresult)
  (>= hresult 0))

(defun handle-com-error (hresult)
  (error "Com error 0x~x [~a]" (+ hresult (expt 2 32)) (decode-com-error hresult)))


(cffi:defctype vartype :short)

(cffi:defcfun ("SafeArrayCreate" %safe-array-create) :pointer (vt-type vartype) (cdims :int) (foreign-array-bounds :pointer))

(defun ensure-list (l)
  (if (atom l)
      (list l)
      l))
(defun safe-array-create (vt-type array-bounds)
  "array bounds are of the form (count lower-bound)"
  (cffi:with-foreign-object (foreign-bounds :int (* 2 (length array-bounds)))
    (let ((index -1))
      (iter (for bounds in array-bounds)
	    (destructuring-bind (count &optional (lbound 0)) (ensure-list bounds)
	      (setf (cffi:mem-aref foreign-bounds :int (incf index)) count)
	      (setf (cffi:mem-aref foreign-bounds :int (incf index)) lbound))))
    (%safe-array-create vt-type (length array-bounds) foreign-bounds)))

(cffi:defcfun ("SafeArrayGetDim" safe-array-get-dim) :int (safe-array :pointer))

(cffi:defcfun ("SafeArrayDestroy" %safe-array-destroy) hresult (safe-array :pointer))

(defun safe-array-destroy (safe-array)
  (com-call (%safe-array-destroy safe-array)))

(cffi:defcfun ("SafeArrayGetLBound" %safe-array-get-lbound) hresult (safe-array :pointer) (dim :int) (foreign-result :pointer))

(defun safe-array-get-lbound (safe-array dim)
  (com-call-with-foreign-result lbound :int
    (%safe-array-get-lbound safe-array dim lbound)))


(cffi:defcfun ("SafeArrayGetUBound" %safe-array-get-ubound) hresult (safe-array :pointer) (dim :int) (foreign-result :pointer))

(defun safe-array-get-ubound (safe-array dim)
  (com-call-with-foreign-result ubound :int
    (%safe-array-get-ubound safe-array dim ubound)))

(defun safe-array-get-bounds (safe-array)
  (iter (for dim from 1 to (safe-array-get-dim safe-array))
	(collect (list (safe-array-get-lbound safe-array dim)
		       (safe-array-get-ubound safe-array dim)))))

(cffi:defcfun ("SafeArrayGetVartype" %safe-array-get-vartype) hresult (safe-array :pointer) (foreign-result :pointer))

(defun safe-array-get-vartype (safe-array)
  (com-call-with-foreign-result vt-type :short
    (%safe-array-get-vartype safe-array vt-type)))


(defmacro with-foreign-indices (foreign-indices-name indices &body body)
  `(cffi:with-foreign-object (,foreign-indices-name :int (length indices))
     (iter (for i from 0)
	   (for index in ,indices)
	   (setf (cffi:mem-aref foreign-indices :int i) index))
     ,@body))

(cffi:defcfun ("SafeArrayPutElement" %safe-array-put-element) hresult (safe-array :pointer) (foreign-indices :pointer) (foreign-value :pointer))

(defun safe-array-put-element (safe-array foreign-type value indices)
  (with-foreign-indices foreign-indices indices
      (cffi:with-foreign-object (foreign-value foreign-type)
	(setf (cffi:mem-ref foreign-value foreign-type) value)
	(let ((hresult (%safe-array-put-element safe-array foreign-indices foreign-value)))
	  (if (succeeded hresult)
	      (cffi:mem-ref foreign-value foreign-type)
	      (handle-com-error hresult))))))

(cffi:defcfun ("SafeArrayGetElement" %safe-array-get-element) hresult (safe-array :pointer) (foreign-indices :pointer) (foreign-result :pointer))

(defun safe-array-get-element (safe-array foreign-type indices)
  (with-foreign-indices foreign-indices indices
    (cffi:with-foreign-object (foreign-result foreign-type)
      (let ((hresult (%safe-array-get-element safe-array foreign-indices foreign-result)))
	(if (succeeded hresult)
	    (cffi:mem-ref foreign-result foreign-type)
	    (handle-com-error hresult))))))

(cffi:defcfun ("SafeArrayGetElemsize" safe-array-get-elem-size) :int (safe-array :pointer))

(cffi:defcfun ("SafeArrayAccessData" %safe-array-access-data) hresult (safe-array :pointer) (ppv-data :pointer))

(defun safe-array-access-data (safe-array)
  (com-call-with-foreign-result ppv-data :pointer
    (%safe-array-access-data safe-array ppv-data)))

(cffi:defcfun ("SafeArrayUnaccessData" %safe-array-unaccess-data) hresult (safe-array :pointer))

(defun safe-array-unaccess-data (safe-array)
  (com-call (%safe-array-unaccess-data safe-array)))

(cffi:defcfun ("SafeArrayPtrOfIndex" %safe-array-ptr-of-index) hresult (safe-array :pointer) (foreign-indices :pointer) (ppv-data :pointer))

(defun safe-array-ptr-of-index (safe-array indices)
  (with-foreign-indices foreign-indices indices
    (com-call-with-foreign-result ppv-data :pointer
      (%safe-array-ptr-of-index safe-array foreign-indices ppv-data))))

(cffi:defcfun ("SafeArrayLock" %safe-array-lock) hresult (safe-array :pointer))
(defun safe-array-lock (safe-array)
  (com-call (%safe-array-lock safe-array)))

(cffi:defcfun ("SafeArrayUnlock" %safe-array-unlock) hresult (safe-array :pointer))
(defun safe-array-unlock (safe-array)
  (com-call (%safe-array-unlock safe-array)))

(defun decode-vt-type (vt-type)
  (list
   (0 '+vt-empty+)
   (1 '+vt-null+)
   (2 '+vt-i2+)
   (3 '+vt-i4+)
   (4 '+vt-r4+)
   (5 '+vt-r8+)
   (6 '+vt-cy+)
   (7 '+vt-date+)
   (8 '+vt-bstr+)
   (9 '+vt-dispatch+)
   (10 '+vt-error+)
   (11 '+vt-bool+)
   (12 '+vt-variant+)
   (13 '+vt-unknown+)
   (14 '+vt-decimal+)
   (16 '+vt-i1+)
   (17 '+vt-ui1+)
   (18 '+vt-ui2+)
   (19 '+vt-ui4+)
   (20 '+vt-i8+)
   (21 '+vt-ui8+)
   (22 '+vt-int+)
   (23 '+vt-uint+)
   (24 '+vt-void+)
   (25 '+vt-hresult+)
   (26 '+vt-ptr+)
   (27 '+vt-safearray+)
   (28 '+vt-carray+)
   (29 '+vt-userdefined+)
   (30 '+vt-lpstr+)
   (31 '+vt-lpwstr+)
   (36 '+vt-record+)
   (37 '+vt-int-ptr+)
   (38 '+vt-uint-ptr+)
   (64 '+vt-filetime+)
   (65 '+vt-blob+)
   (66 '+vt-stream+)
   (67 '+vt-storage+)
   (68 '+vt-streamed-object+)
   (69 '+vt-stored-object+)
   (70 '+vt-blob-object+)
   (71 '+vt-cf+)
   (72 '+vt-clsid+)
   (73 '+vt-versioned-stream+)))
