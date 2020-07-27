;;; -*- Mode: Lisp; Package: CLIM-INTERNALS -*-


(in-package :clim-internals)

;;; From CLIM.  More intuitive description of all the completion subtypes.
(define-presentation-method describe-presentation-type ((type completion) stream plural-count)
  (when (= (length sequence) 1)
    (return-from describe-presentation-type
      (default-describe-presentation-type (funcall name-key (elt sequence 0))
                                          stream
                                          (unless (eq plural-count 1)        ;suppress a/an
                                            plural-count))))
  (etypecase plural-count
    ((member nil 1)
     (write-string "one of " stream))
    ((member t)
     (write-string "one or more of " stream))
    ((integer 2 *)
     (format stream "~R of " plural-count)))
  (let ((position 0)
        (last (1- (length sequence))))
    (map nil #'(lambda (item)
                 (funcall printer (funcall name-key item) stream)
                 (unless (= position last)
                   (write-string (if (= last 1) " " ", ") stream)
                   (when (= (incf position) last)
                     (write-string "or " stream))))
         sequence)))

;;; From McClim
(define-presentation-type or (&rest types)
  :inherit-from t
  :parameters-are-types t)

;;; frrom McCilm
(define-presentation-method presentation-typep (object (type or))
  (loop for type in types
        for real-type = (expand-presentation-type-abbreviation type)
        do (when (presentation-typep object real-type)
             (return-from presentation-typep t)))
  nil)

;;; From CLIM
;;; This shows a bug in definition of this presentation method
;;; the parameter types isn't accessible
(define-presentation-method presentation-type-specifier-p ((type or))
  ;; must have at least one parameter
  (and types
       (every #'presentation-type-specifier-p types)))

;;; This also doesn't compile
(define-presentation-method presentation-subtypep ((subtype or) supertype)
  (with-presentation-type-decoded (or subtypes) subtype
    (with-presentation-type-decoded (or supertypes) supertype
      (if (every #'(lambda (subtype)
                     (some #'(lambda (supertype) (presentation-subtypep-1 subtype supertype))
                           supertypes))
                 subtypes)
          (values t t)
          (values nil nil)))))

(define-presentation-method describe-presentation-type ((type or) stream plural-count)
  (let ((length (length types)))
    (dotimes (i length)
      (describe-presentation-type (elt types i) stream plural-count)
      (cond ((= i (1- length)))
            ((= length 2) (write-string " or " stream))
            ((= i (- length 2)) (write-string ", or " stream))
            (t (write-string ", " stream))))))

(define-presentation-method present (object (type or) stream view &rest options)
  (declare (dynamic-extent options))
  (dolist (type types)
    (when (presentation-typep object type)
      (return-from present
        ;; This must call the real present function, rather than using
        ;; CALL-PRESENTATION-GENERIC-FUNCTION, so that a nested presentation
        ;; of the correct type will be created
        ;;--- Maybe.  See RWK comment in the DW version.
        (apply #'present object type :stream stream :view view options))))
  (error "The object ~S is not of type ~S" object type))


;;; From CLIM
;;; I chopped out all the stuff about histories and defaults from this
;;; to get it to compile.  Still doesn't replace input with
;;; full value for subtype that completes

(define-presentation-method accept ((type or)
                                    (stream input-editing-stream)
                                    (view textual-view)
                                    &key)
  (let* ((location (stream-scan-pointer stream))
         last-error)
    (dolist (type types)
      (block fail
        (handler-bind ((parse-error
                         #'(lambda (anerror)
                             ;; That parser didn't work, try another on the same input
                             (setq last-error anerror)
                             (setf (stream-scan-pointer stream) location)
                             (return-from fail))))
          (multiple-value-bind (object object-type)
              (funcall-presentation-generic-function accept
                    type stream view)
            (return-from accept
              (values object (or object-type type)))))))
    ;; No parser worked.  Resignal the most recent parse-error.
    (simple-parse-error "~A" last-error)))

;;; Modified from McClim
(define-presentation-method accept ((type or)
                                    (stream input-editing-stream)
                                    (view textual-view)
                                    &key)
  (with-input-context (type)
      (object type-var)
      (let ((str (read-token stream)))
	(loop for or-type in types
	   do
	     (handler-case
		 (MULTIPLE-VALUE-BIND (SUB-TYPE-OBJECT SUB-TYPE-TYPE)
                     (accept-from-string or-type str :view view)
                   (PRESENTATION-REPLACE-INPUT STREAM SUB-TYPE-OBJECT SUB-TYPE-TYPE VIEW :RESCAN NIL)
		   (RETURN (VALUES  SUB-TYPE-OBJECT SUB-TYPE-TYPE)))
	       (parse-error ()))
	   finally (simple-parse-error "Input type is not one of ~S" types)))
    (t 
     (presentation-replace-input stream object type-var view :rescan nil)
     (return-from accept (values object type-var)))))



(define-presentation-method presentation-type-history ((type or))
  (with-presentation-type-parameters (or type)
    (presentation-type-history (first types))))
