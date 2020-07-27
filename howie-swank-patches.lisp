;;; -*- Mode: Common-lisp; Package: Swank -*-

;;;; swank.lisp --- Server for SLIME commands.
;;; add an option for write-string-to-buffer

(in-package :swank)

;;; This isn't needed.  The slime patch looks for the to-buffer extension on write string
;;; as suggested below, so we don't need a :write-string-to-buffer message.

;;; In SBCL there is an encapsulate facility similar to ACL's fwrapper facility
;;; (sbclimpl::encapsulate '<function-name> '<type> encapsulating-function)
;;; encapsulating function gets called with the original function and its arguments
;;; it can do whatever it wants including calling the original function

;;; Design note: It's probably possible to do this without a new message type.
;;; Instead in the :write-string message follow the header with :to-buffer <buffer-name> and the
;;; message.  Handle the rest in the slime-event-hook on the slime side.
;;; That makes this file irrelevant.

#+sbcl
(sb-ext::encapsulate 'dispatch-event 'for-write-to-buffer
		     #'(lambda (original connection event)
			 (dcase event
			   ((:write-string-to-buffer &rest _)
			   (declare (ignore _))
			    (encode-message event (current-socket-io)))
			   (t (funcall original connection event)))))

(sb-ext::unencapsulate 'dispatch-event 'for-write-to-buffer)

#|
(defun dispatch-event (connection event)
  "Handle an event triggered either by Emacs or within Lisp."
  (log-event "Swank dispatch-event: ~s~%" event)
  (dcase event
    ((:emacs-rex form package thread-id id)
     (let ((thread (thread-for-evaluation connection thread-id)))
       (cond (thread
              (add-active-thread connection thread)
              (send-event thread `(:emacs-rex ,form ,package ,id)))
             (t
              (encode-message
               (list :invalid-rpc id
                     (format nil "Thread not found: ~s" thread-id))
               (current-socket-io))))))
    ((:return thread &rest args)
     (remove-active-thread connection thread)
     (encode-message `(:return ,@args) (current-socket-io)))
    ((:emacs-interrupt thread-id)
     (interrupt-worker-thread connection thread-id))
    (((:write-string  :write-string-to-buffer
       :debug :debug-condition :debug-activate :debug-return :channel-send
       :presentation-start :presentation-end
       :new-package :new-features :ed :indentation-update
       :eval :eval-no-wait :background-message :inspect :ping
       :y-or-n-p :read-from-minibuffer :read-string :read-aborted :test-delay
       :write-image)
      &rest _)
     (declare (ignore _))
     (encode-message event (current-socket-io)))
    (((:emacs-pong :emacs-return :emacs-return-string) thread-id &rest args)
     (send-event (find-thread thread-id) (cons (car event) args)))
    ((:emacs-channel-send channel-id msg)
     (let ((ch (find-channel channel-id)))
       (send-event (channel-thread ch) `(:emacs-channel-send ,ch ,msg))))
    ((:reader-error packet condition)
     (encode-message `(:reader-error ,packet
                                     ,(safe-condition-message condition))
                     (current-socket-io)))))

|#
