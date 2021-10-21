;;; -*- Mode: Common-lisp; Package: Clim-user -*-

(in-package :clim-user)


(defparameter *remote-scripts* '(("Itunes" . "itunes")
				 ("Powerpoint". "ppt-view-show")))

(defparameter *station-alist*
    `(("WBUR" . "http://wbur-sc.streamguys.com:80/")
      ("WUMB" . "http://wumb.streamguys1.com/wumb919fast")
      ("WGBH" . "https://streams.audio.wgbh.org:8200/wgbh-aac")))

(clim:define-command (com-play-radio :name t
				     :command-table clim-listener::lisp-commands)
    ((station `(clim:member-alist ,*station-alist*)))
  (play-radio station))

(defun play-radio (station)
  (let ((file-name "~/scripts/itunes.scpt"))
      (run-ascript file-name "play" (list station))
      ))

(clim:define-command (com-stop-radio :name t
				     :command-table clim-listener::lisp-commands)
    ()
  (stop-radio))

(defun stop-radio ()
  (let ((file-name "~/scripts/itunes.scpt"))
    ;; (format t "~%Invoking script ~a on host ~a port ~a with arg ~a" file-name host port "stop")
    (run-ascript file-name "stop" (list ""))))


(defun run-ascript (script-pathname function args)
  (let ((command (format nil "osascript ~a ~a ~{\"~a\"~^ ~}" script-pathname function args)))
    ;; (format t "~%~a" command)
    #+sbcl
    (uiop:run-program command)
    command))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Graphical Radio Command Buttons
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *radio-icon-pathname* "~/joshua-dist/clim-fixes/radio.jpg")
(defparameter *radio-pattern* (clim-internals::make-pattern-from-bitmap-file *radio-icon-pathname* :format :jpg))
(defparameter *radio-frame* nil)
(defparameter *radio-window* nil)

(define-application-frame radio-commands
    ()
  ()
  (:menu-bar nil)
  (:panes
   (radios :application
	   :display-time t
	   :incremental-redisplay nil
           :display-function 'draw-radios
	   :scroll-bars ()
	   :borders t
	   )
   )
  (:layouts
   (normal
    (clim:vertically ()
      ;; title
      (:fill (scrolling () radios)))))
  (:command-definer define-radio-command)
  ;; Note: McClim requires this to be a list
  ;; while Allegro's CLIM allows a bare symbol
  (:command-table (radio)))

(defmethod draw-radios ((frame radio-commands) stream)
  (formatting-table (stream)
    (formatting-row (stream)
      (formatting-cell (stream)
        (graphical-play-radio *radio-pattern* stream "wumb" 0 0))
      (formatting-cell (stream)
        (graphical-play-radio *radio-pattern* stream "wbur" 0 0)))
    (formatting-row (stream)
      (formatting-cell (stream)
        (graphical-play-radio *radio-pattern* stream "wgbh" 0 0))
      (formatting-cell (stream)
        (graphical-stop-radio *radio-pattern* stream 0 0)))))

(defun graphical-play-radio (pattern stream &optional (station "wumb") x y)
  (let ((url (cdr (assoc station *station-alist* :test #'string-equal))))
    (clim:with-output-as-presentation
	(stream `(com-graphical-play-radio ,url) 'clim:command :single-box t)
      (clim:draw-pattern* stream pattern x y)
      (clim:draw-text* stream (string-upcase station)
		       (+ x (round (clim:pattern-width pattern) 2))
		       (+ y (round (clim:pattern-height pattern) 2))
		       :ink clim:+white+
		       :text-size :very-large
		       :text-face :bold
		       :align-x :center :align-y :center))))

(defun graphical-stop-radio (pattern stream x y)
  (clim:with-output-as-presentation
      (stream `(com-graphical-stop-radio) 'clim:command :single-box t)
    (clim:draw-pattern* stream pattern x y)
    (let ((width (clim:pattern-width pattern))
          (height (clim:pattern-height pattern)))
      (clim:draw-line* stream x y (+ x width) (+ y height)
		       :ink clim:+red+)
      (clim:draw-line* stream  (+ x width) y x (+ y height)
		       :ink clim:+red+))))

(define-radio-command (com-graphical-play-radio :name nil)
    ((station `(clim:member-alist ,*station-alist*)))
  (play-radio station))

(define-radio-command (com-graphical-stop-radio :name nil)
    ()
  (stop-radio))

(defun kill-radio-commands ()
  (clim:destroy-frame *radio-frame*))

(define-command (com-show-radio-commands :name t
				     :command-table clim-listener::lisp-commands)
    ()
  (let* ((fm (clim:find-frame-manager :port (clim:find-port)))
	 (frame (clim:make-application-frame 'radio-commands
					     :pretty-name "Radio Commands"
					     :frame-manager fm
					     :width (* 2.2 (pattern-width *radio-pattern*))
					     :height (* 2.3 (pattern-height *radio-pattern*)))))
    (setq *radio-frame* frame)
    (flet ((run ()
	     (let ((*package* (find-package :clim-user)))
	       (unwind-protect
		    (clim:run-frame-top-level frame)
                 (clim:disown-frame fm frame)))))
      (values (clim-sys:make-process #'run :name "Radio Commands")
	      frame))))

(define-command (com-kill-radio-commands :name t
				         :command-table clim-listener::lisp-commands)
    ()
  (kill-radio-commands))
