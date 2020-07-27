;;; -*- Mode: Common-lisp; Packae: cl-user -*-

(in-package :common-lisp-user)

(defun startup ()
  (ql:quickload '(:mcclim :clim-listener :cl-json))
  (load "~/josh-dist/clim-fixes/centering-output.lisp")
  (setq *compile-print* nil)
  (load "~/josh-dist/joshua/code/joshua.asd")
  (asdf:load-system :joshua)
  (load "~/Research-projects/attack-planning/code/aplan.asd")
  (asdf:load-system :aplan)
  ;; load patches -- or are they merged into my-version branch
  ;; (load "~/quicklisp/local-projects/my-mcclim/font.lisp")  this is merged
  (load "~/quicklisp/local-projects/my-mcclim/graph-formatting.lisp") ;; not yet merged
  (load "~/quicklisp/local-projects/my-mcclim/howie-patches.lisp")
  (load "~/quicklisp/local-projects/my-mcclim/accept-values-pane.lisp")
  (load "~/quicklisp/local-projects/my-mcclim/window-inside-functions.lisp")
  (load "~/josh-dist/ideal/load-ideal.lisp")
  (load-ideal)
  (load "~/Research-Projects/awdrat/code/awdrat.asd")
  (asdf:load-system :awdrat)
  (load "~/Research-Projects/control-system/defsystem.lisp")
  (asdf:load-system :controls)
  ;; This file contains things that don't compile
  ;; check if some of it was merged in successfully
  ;;(load "~/quicklisp/local-projects/my-mcclim/or-type.lisp")
  (apply (intern (string-upcase "run-listener") (find-package 'clim-listener))
         '( :new-process t)))
