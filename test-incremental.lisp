;;; -*- Mode: Common-lisp; Package: Clim-user -*-

(in-package :clim-user)

(defun test-incremental (&optional (stream *standard-output*))
  (let* ((list (list 1 2 3 4 5))
	 (iteration 0)
         (record
	  (updating-output (stream)
	    (incf iteration)
	    (do* ((elements list (cdr elements))
		  (count 0 (1+ count)))
		((null elements))
	      (let ((element (first elements)))
		(when (if (oddp iteration) (oddp element) (evenp element))
		  (updating-output (stream :unique-id count
					   :cache-value element)
		    (format stream "Element ~D" element)
		    (terpri stream))))))))
    (loop for i below 10
	do (sleep 2)
	   (loop for j below 5
	       do (incf (nth j list)))
	   (redisplay record stream))))
