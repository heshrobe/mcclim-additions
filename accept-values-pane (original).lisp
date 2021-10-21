;;; -*- Mode: Common-lisp; Package: Clim-internals -*-

(in-package :clim-internals)

;;; from panes

(defclass accept-values-pane (clim-stream-pane)
  (;; it's not clear that we need these since
   ;; stream panes all mixin in accept-values-stream
   ;; which has fields for these purposes
   (accept-values-record :accessor accept-values-record :initform nil :initarg :accept-values-record)
   (accept-values-stream :accessor accept-values-stream :initform nil :initarg :accept-values-stream)
   )
  (:default-initargs :default-view +textual-dialog-view+ :redisplay-needed :no-clear))

(defmethod stream-default-view ((pane accept-values-pane)) +textual-dialog-view+)

(defun make-clim-accept-values-pane (&rest options)
  (apply #'make-clim-stream-pane :type 'accept-values-pane options))

(defun generate-pane-creation-form (name form)
  (destructuring-bind (pane &rest options) form
    (cond ((and (null options) (listp pane)) ; Single form which is a function call
           `(coerce-pane-name ,pane ',name))
          ((eq pane :application) ; Standard pane denoted by a keyword (i.e `:application')
           `(make-clim-application-pane :name ',name ,@options))
          ((eq pane :interactor)
           `(make-clim-interactor-pane :name ',name ,@options))
          ((eq pane :pointer-documentation)
           `(make-clim-pointer-documentation-pane :name ',name ,@options))
          ((eq pane :command-menu)
           `(make-clim-command-menu-pane :name ',name ,@options))
          ((eq pane :accept-values)
           `(make-clim-accept-values-pane :name ',name ,@options))
          (t ; Non-standard pane designator fed to the `make-pane'
           `(make-pane ',pane :name ',name ,@options)))))

;;; from accept-values

(define-command-table accept-values-pane)

;;; Notes: Letf-globally -> letf
;;; frame-manager-dialog-view doesn't exist
;;; display-view-background doesn't exist

(defun accept-values-pane-displayer (frame pane
                                     &key displayer
                                       resynchronize-every-pass
                                       (check-overlapping t)
                                       view
                                       align-prompts
                                       max-height max-width)
  (declare (ignore max-height max-width))
  (setq align-prompts (ecase align-prompts
                        ((t :right) :right)
                        ((:left) :left)
                        ((nil) nil)))
  (let* ((record (accept-values-record pane))
         (*accepting-values-stream* (accept-values-stream pane)))
    (cond ((and record *accepting-values-stream*)
           (letf (((stream-default-view pane) (or view (stream-default-view pane))))
             (redisplay record pane :check-overlapping check-overlapping)
             (when resynchronize-every-pass
               (redisplay record pane :check-overlapping check-overlapping))))
          (t
           (accept-values-pane-displayer-1 frame pane displayer
                                           align-prompts view)))))

(defun accept-values-pane-displayer-1 (frame pane displayer align-prompts view)
  (let ((*accepting-values-stream* (make-instance 'accepting-values-stream
                                                :stream pane
                                                :align-prompts align-prompts))
        (record nil)
        (view (or view (stream-default-view pane))))
    (letf (((stream-default-view pane) view))
      (setq record
            (updating-output (pane :record-type 'accepting-values-record)
              (if align-prompts
                    (formatting-table (pane)
                      (funcall displayer frame *accepting-values-stream*))
                    (funcall displayer frame *accepting-values-stream*)))))
    (setf (accept-values-stream pane) *accepting-values-stream*
          (accept-values-record pane) record)))

(define-command (com-edit-accepting-values-pane-choice :name nil :command-table accept-values-pane)
    ((query-identifier t)
     (pane 'accept-values-pane))
  (let* ((*accepting-values-stream* (accept-values-stream pane))
         (record (accept-values-record pane))
         (query-object (find query-identifier
                             (queries *accepting-values-stream*)
                             :key #'query-identifier :test #'equal)))
      (select-query *accepting-values-stream* query-object (record query-object))

      ;; Because of the odd way that the McClim implementation
      ;; implements accepting-values unless we force a replay
      ;; here updating the same field twice in row leads to the 2nd
      ;; update not working
      (redisplay record pane :check-overlapping t)
      ))

(define-presentation-to-command-translator edit-accepting-values-pane-choice
    (selectable-query com-edit-accepting-values-pane-choice accept-values-pane
                      :pointer-documentation "Edit this field"
                      :gesture :select
                      :tester ((object)
                               t)
                      :priority 1
                      ;;prefer this to IDENTITY when in COMMAND-OR-FORM context
                      ;; Echoing this is annoying, as is putting it into the command history
                      :echo nil )
    (object window)
  (list object window))

(define-command (com-edit-accepting-values-gadget-choice :name nil :command-table accept-values-pane)
    ((query-identifier t)
     (new-value t)
     (pane 'accept-values-pane))
  (let* ((*accepting-values-stream* (accept-values-stream pane))
         (query (find query-identifier (queries *accepting-values-stream*)
                      :key #'query-identifier :test #'equal)))
    ;; (break "in command ~a" query)
    (when query
      (setf (value query) new-value
            (changedp query) t)))
  (setf (pane-needs-redisplay pane) :no-cLear)
  ;; Because of the odd way that the McClim implementation
  ;; implements accepting-values unless we force a replay
  ;; here updating the same field twice in row leads to the 2nd
  ;; update not working
  (redisplay-frame-pane (pane-frame pane) pane)
  )

(defvar *gadget-clicked-on* nil)
(Define-Presentation-to-command-translator gadget-to-change-value
    (command com-edit-accepting-values-gadget-choice accept-values-pane
             :gesture :select
             :echo nil
             :tester ((object presentation)
                      (let ((ptype (presentation-type presentation))
                            (window (when *gadget-clicked-on*
                                      (sheet-parent *gadget-clicked-on*))))
                        (when (and window
                                   (eql (first object) 'com-change-query)
                                   (eql (first ptype) 'command)
                                   (eql (third ptype) 'accept-values-pane)
                                   (typep window 'accept-values-pane))
                          ;; (prog1 t (break "In tester ~a ~%~a ~%~a" window object ptype))
                          t))))
    (object)
  ;; this should be the query identifier and the new-calue
  (destructuring-bind (query-identifier new-value) (rest object)
    (let ((window (sheet-parent *gadget-clicked-on*)))
      ;; (Break "In translator ~a ~a ~a" query-identifier new-value window)
      (list query-identifier new-value window))))



(export '(accept-values-pane accept-values-pane-displayer) 'clim-internals)
(import '(accept-values-pane accept-values-pane-displayer) 'clim)
(export '(accept-values-pane accept-values-pane-displayer) 'clim)


;;; I need to let the guy who catches the event know what gadget (actually what pane) was clicked
;;; I would be nice if we could use the shhet keyword argument to throw-object-ptype but
;;; that turns out not to work because the event created there doesn't specify graft-x and graft-y
;;; which are used if the sheet argument is passed in.  Trying to set them to 0 didn't help
;;; because then the presentation isn't findable and there's a much greater risk of screwing up
;;; everything.  I can go back and look at that sometime.
;;; So I just resorted to the kludge of special binding a variable to hold the gadget
;;; stuffing it into either the ptype or the object breaks other things as you would expect

;;; from dialog-views

(defun %standard-value-changed-callback (query-identifier &optional value-transform)
  ;; Return a function to be used as value-changed-callback of a
  ;; gadget.  The returned function changes the value of the query
  ;; associated with the gadget inside a dialog. The query is
  ;; identified by QUERY-IDENTIFIER.  If VALUE-TRANSFORM is NIL the
  ;; new value of the query will be the value of the gadget, otherwise
  ;; the VALUE-TRANSFORM function will be called with the value of the
  ;; gadget as argument and the returned value will be the new value
  ;; of the query.
  (lambda (pane value)
    ;; (declare (ignore pane))
    (let ((*gadget-clicked-on* pane))
      (let ((new-value (if value-transform
                           (funcall value-transform value)
                           value)))
        (throw-object-ptype `(com-change-query ,query-identifier ,new-value)
                            '(command :command-table accept-values-pane)
                            )))))


;;; The method for this in dialog.lisp is different from the others
;;; and doesn't seem to work
(define-presentation-method accept-present-default
    ((type completion) stream (view option-pane-view) default default-supplied-p
                       present-p query-identifier)
      (make-output-record-from-view view stream query-identifier
                                    :mode :exclusive
                                    :test test
                                    :value default
                                    :name-key name-key
                                    :value-key value-key
                                    :items sequence
                                    :value-changed-callback (%standard-value-changed-callback query-identifier)))

#|

;;; WAITING till the above works

(define-command (com-modify-accepting-values-pane-choice :command-table accept-values-pane)
    ((choice 'accept-values-choice)
     (pane 't))
  (accept-values-query-edit-value choice (car (gethash pane (get-frame-pane-to-avv-stream-table (pane-frame pane))))
                        :modify t))

(define-presentation-to-command-translator modify-accepting-values-pane-choice
    (accept-values-choice com-modify-accepting-values-pane-choice accept-values-pane
     :documentation "Modify this field"
     :pointer-documentation "Modify this field"
     :gesture :modify-field
     :tester ((object presentation)
              (accept-values-query-valid-p object presentation))
     :priority 1        ;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object window)
  (list object window))

(define-command (com-delete-accepting-values-pane-choice :command-table accept-values-pane)
    ((choice 'accept-values-choice))
  (accept-values-query-delete-value choice))

(define-presentation-to-command-translator delete-accepting-values-pane-choice
    (accept-values-choice com-delete-accepting-values-pane-choice accept-values-pane
     :documentation "Remove this field"
     :pointer-documentation "Remove this field"
     :tester ((object presentation)
              (accept-values-query-valid-p object presentation))
     :gesture :delete-field
     :priority 1        ;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object)
  (list object))

(define-command (com-accepting-values-pane-command-button :command-table accept-values-pane)
    ((button 'accept-values-command-button)
     (pane 't))
  (funcall (slot-value button 'continuation))
  (when (slot-value button 'resynchronize)
    (let* ((stream-and-record (and (not *frame-layout-changing-p*)
                                   (gethash pane (get-frame-pane-to-accepting-values-stream-table (pane-frame pane)))))
           (accepting-values-stream (car stream-and-record))
           (accepting-values-record (cdr stream-and-record)))
      (when accepting-values-stream
        (letf-globally (((stream-default-view accepting-values-stream) +textual-dialog-view+))
          (redisplay accepting-values-record accepting-values-stream))))))

(define-presentation-to-command-translator accepting-values-pane-command-button
    (accept-values-command-button com-accepting-values-pane-command-button accept-values-pane
     :documentation document-command-button
     :pointer-documentation document-command-button
     :gesture :select
     :priority 1        ;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object window)
  (list object window))

(define-command (com-accepting-values-pane-choose-one-of :command-table accept-values-pane)
    ((choice 'accept-values-one-of))
  (accepting-values-choose-one-of-1 choice))

(define-presentation-to-command-translator accepting-values-pane-choose-one-of
    (accept-values-one-of com-accepting-values-pane-choose-one-of accept-values-pane
     :documentation "Select this value"
     :pointer-documentation "Select this value"
     :gesture :select
     :priority 1        ;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object)
  (list object))

(define-command (com-accepting-values-pane-choose-some-of :command-table accept-values-pane)
    ((choice 'accept-values-some-of))
  (accepting-values-choose-some-of-1 choice))

(define-presentation-to-command-translator accepting-values-pane-choose-some-of
    (accept-values-some-of com-accepting-values-pane-choose-some-of accept-values-pane
     :documentation "De/Select this value"
     :pointer-documentation "De/Select this value"
     :gesture :select
     :priority 1        ;prefer this to IDENTITY when in COMMAND-OR-FORM context
     :echo nil :maintain-history nil)
    (object)
  (list object))

|#
