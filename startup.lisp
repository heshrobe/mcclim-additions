;;; -*- Mode: Common-lisp; Packae: cl-user -*-

(in-package :common-lisp-user)

(defun startup ()
  (setq *compile-print* nil)
  (ql:quickload '(:mcclim :clim-listener :cl-json))
  ;; load patches -- or are they merged into my-version branch
  (load "~/quicklisp/local-projects/my-mcclim/mcclim-fixes.asd")
  (asdf:load-system :mcclim-fixes)
  (load "~/josh-dist/joshua/code/joshua.asd")
  (asdf:load-system :joshua)
  (load "~/Research-projects/attack-planning/code/aplan.asd")
  (asdf:load-system :aplan)
  (load "~/josh-dist/ideal/load-ideal.lisp")
  ;; Need to strip down ideal to what I need and to fix this loading process
  (funcall (intern (string-upcase "load-ideal")))
  (load "~/Research-Projects/awdrat/code/awdrat.asd")
  (asdf:load-system :awdrat)
  (load "~/Research-Projects/control-system/controls.asd")
  (asdf:load-system :controls)
  ;; This file contains things that don't compile
  ;; check if some of it was merged in successfully
  ;;(load "~/quicklisp/local-projects/my-mcclim/or-type.lisp")
  (apply (intern (string-upcase "run-listener") (find-package 'clim-listener))
         '( :new-process t)))
