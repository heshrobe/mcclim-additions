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
  (ql:quickload '(:mcclim :clim-listener :cl-json :cl-yaml))

  (load "~/joshua-dist/joshua/code/joshua.asd")
  (asdf:load-system :joshua)

  (let ((sb-ext:*muffled-warnings* 'sb-kernel:redefinition-warning))
    (load "~/quicklisp/local-projects/mcclim-additions/mcclim-additions.asd")
    (asdf:load-system :mcclim-additions))

  (load "~/joshua-dist/joshua/developer/joshua-developer.asd")
  (asdf:load-system :joshua-developer)

  (load "~/Research-Projects/attack-planning/code/aplan.asd")
  (asdf:load-system :aplan)
  (apply (intern (string-upcase "run-listener") (find-package 'clim-listener))
         '( :new-process t)))

(startup)
