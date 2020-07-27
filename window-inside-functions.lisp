;;; -*- Mode: Common-lisp; Package: clim-internals -*-

(in-package :clim-internals)


(defmethod window-set-viewport-position ((stream clim-stream-pane) x y)
  (when (pane-viewport stream)
    (scroll-extent stream x y)))

;; (defgeneric* (setf window-viewport-position) (x y stream))
(defmethod* (setf window-viewport-position) (x y (stream clim-stream-pane))
  (window-set-viewport-position stream x y))

(defmethod window-inside-edges ((stream clim-stream-pane))
  (bounding-rectangle* (sheet-region (or (pane-viewport stream) stream))))

(defun window-inside-left (stream)
  (multiple-value-bind (left top right bottom)
      (window-inside-edges stream)
    (declare (ignore top right bottom))
    left))

(defun window-inside-top (stream)
  (multiple-value-bind (left top right bottom)
      (window-inside-edges stream)
    (declare (ignore left right bottom))
    top))

(defun window-inside-right (stream)
  (multiple-value-bind (left top right bottom)
      (window-inside-edges stream)
    (declare (ignore left top bottom))
    right))

(defun window-inside-bottom (stream)
  (multiple-value-bind (left top right bottom)
      (window-inside-edges stream)
    (declare (ignore left top right))
    bottom))

(defmethod window-inside-size ((stream clim-stream-pane))
  (bounding-rectangle-size (window-viewport stream)))

(defmethod window-set-inside-size ((stream clim-stream-pane) width height)
  (change-space-requirements stream :width width :height height :resize-frame t))

(defmethod window-inside-width ((stream clim-stream-pane))
  (bounding-rectangle-width (window-viewport stream)))

(defmethod window-inside-height ((stream clim-stream-pane))
  (bounding-rectangle-height (window-viewport stream)))

(export '(window-inside-height window-inside-width window-set-inside-size window-inside-size window-inside-bottom window-inside-right window-inside-top window-inside-left window-inside-edges window-viewport-position))
(import '(window-inside-height window-inside-width window-set-inside-size window-inside-size window-inside-bottom window-inside-right window-inside-top window-inside-left window-inside-edges window-viewport-position) 'clim)
(export '(window-inside-height window-inside-width window-set-inside-size window-inside-size window-inside-bottom window-inside-right window-inside-top window-inside-left window-inside-edges window-viewport-position) 'clim)
