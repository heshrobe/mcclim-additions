;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-INTERNALS; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: Graph Formatting
;;;   Created: 2002-08-13
;;;   License: LGPL (See file COPYING for details).
;;;       $Id: graph-formatting.lisp,v 1.23 2008/11/09 19:58:26 ahefner Exp $
;;; ---------------------------------------------------------------------------

;;;  (c) copyright 2002 by Gilbert Baumann
;;;  (c) copyright 2005 by Robert P. Goldman
;;;  (c) copyright 2017 by John A. Carroll

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :clim-internals)

(defun format-graph-from-roots (root-objects object-printer inferior-producer
                                &rest rest-args
                                &key stream (orientation :horizontal) cutoff-depth
                                     merge-duplicates duplicate-key duplicate-test
                                     (GENERATION-SEPARATION '(4 :CHARACTER) GENERATION-SEPARATION-SUPPLIED-P)
                                     (WITHIN-GENERATION-SEPARATION '(1/2 :LINE) WITHIN-GENERATION-SEPARATION-SUPPLIED-P)
                                     center-nodes
                                     arc-drawer arc-drawing-options
                                     graph-type maximize-generations (move-cursor t)
                                &allow-other-keys)
  (declare (ignore center-nodes maximize-generations))
  (WHEN (AND (NULL GENERATION-SEPARATION-SUPPLIED-P)
             (NULL WITHIN-GENERATION-SEPARATION-SUPPLIED-P))
    (let ((horizontal? (eql orientation :horizontal)))
      (setf (GETF REST-ARGS :GENERATION-SEPARATION) (if horizontal? GENERATION-SEPARATION WITHIN-GENERATION-SEPARATION)
            (GETF REST-ARGS :WITHIN-GENERATION-SEPARATION) (if horizontal? WITHIN-GENERATION-SEPARATION GENERATION-SEPARATION))))
  ;; Don'T destructively modify the &rest arg
  (let ((graph-options (alexandria:remove-from-plist
                        rest-args :stream :duplicate-key :duplicate-test
                        :arc-drawer :arc-drawing-options
                        :graph-type :move-cursor)))
    ;; munge some of the arguments
    (check-type cutoff-depth (or null integer))
    (check-type root-objects sequence)
    (check-type orientation (member :horizontal :vertical))
    (setf stream (or stream *standard-output*)
          graph-type (or graph-type (if merge-duplicates :digraph :tree))
          duplicate-key (or duplicate-key #'identity)
          duplicate-test (or duplicate-test #'eql) )

    (multiple-value-bind (cursor-old-x cursor-old-y)
        (stream-cursor-position stream)
      (let ((graph-output-record
             (labels ((cont (stream graph-output-record)
                        (with-output-recording-options (stream :draw nil :record t)
                          (generate-graph-nodes graph-output-record stream root-objects
                                                object-printer inferior-producer
                                                :duplicate-key duplicate-key
                                                :duplicate-test duplicate-test)
                          (layout-graph-nodes graph-output-record stream arc-drawer arc-drawing-options)
                          (layout-graph-edges graph-output-record stream arc-drawer arc-drawing-options))))
               (apply #'invoke-with-new-output-record stream
                      #'cont
                      (find-graph-type graph-type)
                      graph-options))))
        (setf (output-record-position graph-output-record)
          (values cursor-old-x cursor-old-y))
        (when (and (stream-drawing-p stream)
                   (output-record-ancestor-p (stream-output-history stream)
                                             graph-output-record))
          (with-output-recording-options (stream :draw t :record nil)
            (replay graph-output-record stream)))
        (when move-cursor
          (setf (stream-cursor-position stream)
            (values (bounding-rectangle-max-x graph-output-record)
                    (bounding-rectangle-max-y graph-output-record))))
        graph-output-record))))
