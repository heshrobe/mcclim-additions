;;; -*- Mode: Common-lisp; Package: Clim-user -*-

(in-package :clim-user)

(defun test-avv (&key (own-window nil) (stream *standard-output*))
  (let ((x 1) (a 0) (boolean nil)
        (y 2) (b 0)
        (z 'a))
    (accepting-values (stream :resynchronize-every-pass t :own-window own-window)
      (format stream "~%Adaptive Dialog~%")
      (setq boolean (clim:accept 'boolean
                                 :stream stream
                                 :default boolean
                                 :view clim:+toggle-button-view+))
      (terpri stream)
      (format t "~%X = ~a Y = ~a A = ~a B = ~a~% Z = ~a" x y a b z)
      (terpri stream)
      (setq z (clim:accept `(clim:member a b c d e)
                             :stream stream
                             :view clim:+option-pane-view+
                             :prompt "Which"
                             :default z))
      (terpri stream)
      (if (> x y)
          (format stream "~%X is larger than Y")
          (format stream "~%X is not larger than Y"))
      (terpri stream)
      (setq x (clim:accept '(integer 0 20) :stream stream :prompt "X?"
                                           :view '(slider-view :orientation :horizontal :decimal-places 2) :Default x ))
      (when (> x y)
        (terpri stream)
        (setq a (clim:accept 'number :default a :prompt "A?" :stream stream)))
      (terpri stream)
      (setq y (clim:accept 'number :default y :prompt "Y?" :stream stream))
      (when (>= y x)
        (terpri stream)
        (setq b (clim:accept 'number :default b :prompt "B?" :stream stream))))
    (values x y a b z)))
