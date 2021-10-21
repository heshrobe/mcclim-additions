

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
