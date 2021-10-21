;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Package: CLIM-INTERNALS; Base: 10; Lowercase: Yes -*-
;; See the file LICENSE for the full license governing this code.
;;

(in-package :clim-internals)

;;; Currently this pops up an error window which offers a proceed type to try again
;;; Actually that's just the top level debugger in the Lisp Listener.
;;; The error is signalled by complete-input and isn't handled within McClim.
;;; I'd guess that complete-input should catch it, issue some sort of message
;;; but leave all the text there for the user to edit.  That's what the Allegro implementation
;;; does.  But that's all outside the scope of this minor fix.

(define-presentation-method accept ((type subset-completion) stream (view textual-view) &key)
  (loop
      with separators = (list separator)
      for remaining-elements = sequence then (if (null element) remaining-elements (remove element remaining-elements))
      for element-type = `(completion ,remaining-elements :test ,test :value-key ,value-key)
      for element = (handler-case
                        (accept element-type ; i.e., the type parameter
                                :stream stream
                                :view view
                                :prompt nil
                                :additional-delimiter-gestures separators)
                      (simple-completion-error () nil))
      unless (null element)
      collect element
      do (progn
           (when (not (eql (peek-char nil stream nil nil) separator))
             (loop-finish))
           (read-char stream)
           (when echo-space
             ;; Make the space a noise string
             (input-editor-format stream " ")))))


;;; The original which I think is more correct

(define-presentation-method accept ((type subset-completion) stream (view textual-view) &key)
  (let ((list nil)
        (element-type `(completion ,sequence :test ,test :value-key ,value-key))
        (separators (list separator))
        (echo-string
          (if (eql separator #\space)
              " "
              (make-array 2 :initial-contents (list separator #\space)))))
    (format *trace-output* "~%Separator ~a ~%" separators)
    (flet ((possibility-printer (possibility type stream)
             (with-output-as-presentation (stream (list (funcall value-key (second possibility))) type)
               (funcall printer
                        (funcall name-key (find (second possibility) sequence
                                                :key value-key :test test))
                        stream))))
      (declare (dynamic-extent #'possibility-printer))
      (loop
        (format *trace-output* "~%At top of loop list = ~a Abort Gestures ~a Accelerator Gestures ~a ~%"
                list *abort-gestures* *accelerator-gestures*)
        ;; (let ((gesture (handler-case (read-gesture :stream stream :peek-p t :timeout 0)
        ;;                  (condition (error)
        ;;                    (format *trace-output* "~%got error from read gesture ~a~%"
        ;;                            error)
        ;;                    t))))
        ;;   (format *trace-output* "~%Got gesture ~a~%" gesture)
        ;;   (when (eql gesture :eof)
        ;;     (return-from accept (nreverse list))))
        ;; (format *trace-output* "~%Looking for element, list so far ~a ~%" list)
        (let ((element
                (with-delimiter-gestures (separators)
                  (completing-from-suggestions
                      (stream :partial-completers partial-completers
                              :possibility-printer #'possibility-printer
                              :help-displays-possibilities (<= (length sequence) 10))
                    (flet ((suggest-item (item)
                             (suggest (funcall name-key item)
                                      (funcall value-key item))))
                      (declare (dynamic-extent #'suggest-item))
                      (map nil #'suggest-item sequence))))))
          (push element list)
          (format *trace-output* "~%Got element ~a list ~a ~%" element list))
        (block do-it
          (loop
            (multiple-value-bind (more-to-come object)
                (accept-comma stream element-type view
                              :delimiter-character separator
                              :echo-space echo-space :echo-string-before-blip echo-string)
              (ecase more-to-come
                ((nil)
                 (return-from accept
                   (nreverse list)))
                ((t)
                 (format *trace-output* "~%Got t from accept comma ~%" )
                 (return-from do-it)) ;return to outer loop, call completing-from-suggestions again
                ((:accepted)
                 (push object list))))))
        (format *trace-output* "~%After block do-it list = ~a ~%" list)
        ))))

;;; A handy routine for accepting comma-separated lists in textual views.
;;; If the next character is a comma, absorbs it and any following space and returns T.
;;; Otherwise it absorbs no characters and returns NIL.
;;; Or if a presentation of type desired-type is clicked, return :ACCEPTED, object, and type
(defun accept-comma (stream desired-type view
                     &key (delimiter-character #\,)
                          echo-space echo-string-before-blip)
  ;; (declare (values more-to-come object type))
  (with-input-context (desired-type) (object type)
       (loop
         (let ((char (read-gesture :stream stream)))
           (cond ((typep char 'pointer-button-event)
                  ;; These events come through even though they have already been handled
                  ;; just ignore them, as read-token does
                  )
                 ((eql char delimiter-character)
                  (when (and echo-space (not (eql delimiter-character #\space)))
                    (if (stream-rescanning-p stream)
                        (let ((space (read-gesture :stream stream)))
                          (unless (whitespacep space)
                            ;; It wasn't a space so put it back
                            (unread-gesture space :stream stream)))
                        ;; Add a space if the comma was just typed in for the first time
                        ;; In McClim I think this replaces from the beginning of the buffer
                        ;; so need to grab current-position first
                        (progn
                          (format *trace-output* "~%About to replace input scan pointer ~a input-position ~a~%"
                                  (stream-scan-pointer stream)
                                  (drei::input-position stream))
                          ;; (replace-input stream " " :buffer-start (stream-scan-pointer stream))
                          (format *trace-output* "~%After replace input scan pointer ~a input-position ~a~%"
                                  (stream-scan-pointer stream)
                                  (drei::input-position stream)))
                        ))
                  (return-from accept-comma t))
                 (t
                  ;; It wasn't a comma so put it back
                  (unread-gesture char :stream stream)
                  (return-from accept-comma nil)))))
     (t
       (when echo-string-before-blip
         (replace-input stream echo-string-before-blip))
       (presentation-replace-input stream object type view)
       (values :accepted object type))))
