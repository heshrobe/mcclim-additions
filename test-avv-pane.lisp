;;; -*- Mode: lisp; Package: clim-user -*-

(in-package :clim-user)

;;; Note that this doesn't actually work
;;; it illustrates why you need an accept-values pane


(define-application-frame test-avv ()
  ((a :accessor a :initform 5)
   (b :accessor b :initform 2)
   (c :accessor c :initform 3)
   (d :accessor d :initform 4))
  (:top-level (test-avv-top-level))
  (:menu-bar nil)
  (:command-table (test-avv :inherit-from (clim-internals::accept-values-pane clim-listener::listener)))
  (:panes
   (pointer-doc :pointer-documentation :scroll-bars nil :borders t :max-height '(2 :line) :height '(2 :line))
   (shower :application
           :display-time :command-loop
           :incremental-redisplay nil
           :scroll-bars t
           :borders t
           :display-function 'test-avv-shower
           )
   (accept-values-1 :accept-values
                    :scroll-bars t
                    :borders t
                    :display-function '(clim-internals::accept-values-pane-displayer
                                        :displayer accept-values-diplayer-1
                                        ))
   (accept-values-2 :accept-values
                    :scroll-bars t
                    :borders t
                    :display-function '(clim-internals::accept-values-pane-displayer
                                        :displayer accept-values-diplayer-2
                                        ))
   )
  (:layouts
   (normal
    (clim:vertically ()
      (.5 shower)
      (:fill  (clim:horizontally ()
                (.5 accept-values-1)
                (.5 accept-values-2)))
      pointer-doc))))

(defmethod accept-values-diplayer-1 ((frame test-avv) stream)
  (setf (a frame) (clim:accept '(integer 0 20) :stream stream :prompt "A" :view clim:+slider-View+ :Default (a frame)))
  (fresh-line stream)
  (setf (b frame) (clim:accept 'integer :stream stream :prompt "B" :view clim:+textual-dialog-view+ :default (b frame)))
  )

(defmethod accept-values-diplayer-2 ((frame test-avv) stream)
  (setf (c frame) (clim:accept 'integer :stream stream :prompt "C" :view clim:+textual-dialog-view+ :default (c frame)))
  (fresh-line stream)
  (setf (d frame) (clim:accept 'integer :stream stream :prompt "D" :view clim:+textual-dialog-view+ :default (d frame)))
  )

(defmethod test-avv-shower ((frame test-avv) stream)
  (clim:formatting-table (stream)
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream) (write-string "A" stream))
      (clim:formatting-cell (stream) (write-string "B" stream))
      (clim:formatting-cell (stream) (write-string "C" stream))
      (clim:formatting-cell (stream) (write-string "D" stream)))
    (clim:formatting-row (stream)
      (clim:formatting-cell (stream) (princ (a frame) stream))
      (clim:formatting-cell (stream) (princ (b frame) stream))
      (clim:formatting-cell (stream) (princ (c frame) stream))
      (clim:formatting-cell (stream) (princ (d frame) stream)))))


(defmethod test-avv-top-level ((frame test-avv) &REST OPTIONS)
  (APPLY #'clim:default-frame-top-level
         frame
         :prompt ">"
         OPTIONS))

(defun run-avv-test (&key
		     (new-process t)
		     (debugger t)
		     (width 790)
		     (height 550)
		     port
		     frame-manager
		     (process-name "avv test")
		     (package :clim-user))
  (let* ((fm (or frame-manager (clim:find-frame-manager :port (or port (clim:find-port)))))
	 (frame (clim:make-application-frame 'test-avv
					     :pretty-name "test avv"
					     :frame-manager fm
					     :width width
					     :height height)))
    (flet ((run ()
	     (let ((*package* (find-package package)))
	       (unwind-protect
		    (if debugger
			(clim-debugger:with-debugger () (clim:run-frame-top-level frame))
			(clim:disown-frame fm frame))))))
      (if new-process
	  (values (clim-sys:make-process #'run :name process-name)
		  frame)
	  (run)))))




(defun accepting-interval (&key (min 1) (max 20) (stream *query-io*) (ow t))
  (clim:accepting-values (stream :own-window ow)
    (fresh-line stream)
    (setq min (clim:accept '(integer 0 20) :default min :prompt "Min":stream stream ))
    (fresh-line stream)
    (setq max (clim:accept '(integer 0 20) :default max :prompt "Max" :stream stream))
    (when (< max min)
      (rotatef min max))
  (values min max))
