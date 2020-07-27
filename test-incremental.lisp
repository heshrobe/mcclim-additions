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

;;; You can't do an updating output
;;; on the stream passed in but normal
;;; output is updating if it changes
;;; and text inside conditionals pops in and out as expected.
;;; so you don't need updating-output.

(defun test-avv (&optional (stream *standard-output*))
  (let ((x 1) (a 0)
        (y 2) (b 0))
    (accepting-values (stream :resynchronize-every-pass t)
      (format stream "~%Adaptive Dialog~%")
      (format t "~%X = ~a Y = ~a A = ~a B = ~a~%" x y a b)
      (if (> x y)
          (format stream "~%X is larger than Y")
          (format stream "~%X is not larger than Y"))
      (terpri stream)
      (setq x (clim:accept 'number :default x :prompt "X?" :stream stream))
      (when (> x y)
        (terpri stream)
        (setq a (clim:accept 'number :default a :prompt "A?" :stream stream)))
      (terpri stream)
      (setq y (clim:accept 'number :default y :prompt "Y?" :stream stream))
      (when (>= y x)
        (terpri stream)
        (setq b (clim:accept 'number :default b :prompt "B?" :stream stream)))
      (values x y a b)
      )))
