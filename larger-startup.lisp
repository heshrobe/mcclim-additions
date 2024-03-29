;;; -*- Mode: Common-lisp; Packae: cl-user -*-

(in-package :common-lisp-user)

;;; The following lines added by ql:add-to-init-file:
(unless (find-package :quicklisp)
  (format t "~%Instaliing Quicklisp")
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                         (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init)
      (funcall (intern (string-upcase "add-to-init-file") (find-package :quicklisp)))
      )))


(defun startup ()
  (setq *compile-print* nil)
  (ql:quickload '(:mcclim :clim-listener :cl-json))
  ;; load patches -- or are they merged into my-version branch
  (let ((sb-ext:*muffled-warnings* 'sb-kernel:redefinition-warning))
    (load "~/quicklisp/local-projects/mcclim-additions/mcclim-fixes.asd")
    (asdf:load-system :mcclim-fixes))
  (load "~/joshua-dist/joshua/code/joshua.asd")
  (asdf:load-system :joshua)
  (load "~/joshua-dist/joshua/developer/joshua-developer.asd")
  (asdf:load-system :joshua-developer)
  (load "~/Research-projects/attack-planning/code/aplan.asd")
  (asdf:load-system :aplan)
  (when (equal (sb-unix::posix-getenv "USER") "hes")
    (load "~/joshua-dist/ideal/sysdefs/ideal.asd")
    (asdf:load-system :ideal)
    (load "~/Research-Projects/awdrat/code/awdrat.asd")
    (asdf:load-system :awdrat)
    (load "~/Research-Projects/control-system/controls.asd")
    (asdf:load-system :controls)
    (load "~/Research-Projects/start-interface/code/start-interface.asd")
    (asdf:load-system :start-interface)
    (load "~/Research-Projects/recipes/code/recipes.asd")
    (asdf:load-system :recipes)
    (load "~/Research-Projects/ASIST/guide/guide.asd")
    (asdf:load-system :guide)
    )
  (apply (intern (string-upcase "run-listener") (find-package 'clim-listener))
         '( :new-process t)))

(startup)
