;;; -*- Mode: Common-lisp; Package: Common-lisp-user -*-

(in-package :common-lisp-user)

(asdf:defsystem mcclim-fixes
  :name "McClim Fixes"
  :maintainer "Howie Shrobe"
  :description "Various fixes and additions not mature enough to add to my branch"
  :pathname "."
  :components ((:file "accept-values-pane")
               ;; (:file "subset-completion")  Merged into my-version branch of McClim
               (:file "centering-output")
               (:File "graph-formatting")
               (:file "howie-patches")
               (:File "window-inside-functions")
               (:File "more-additions")
               ))
               

;; (load "~/quicklisp/local-projects/my-mcclim/font.lisp")  this is merged
;; or-type still needs fixing
;; missing presentation-methods for subset-completion in text-mode
