(in-package :safearray)

(defun com-error-codes-table ()
  '((-2147352565 "Invalid index." :disp-e-badindex)
    (-2147418113 "Catastrophic failure." :e-unexpected)
    (-2147352563 "Memory is locked." :disp-e-arrayislocked)))


(defun decode-com-error (hresult)
  (let ((decode-entry (assoc hresult (com-error-codes-table))))
    (if decode-entry
	(format nil "~a, ~a" (cadr decode-entry) (caddr decode-entry))
	"Unknown error code.")))
