;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-LISTENER; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Patch from ~/quicklisp/local/McClim/core/clim-core/presentation-defs.
;;;   Control prompting in sequence-enumerated
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clim-internals)


;;; Apparently (10/8/2021) the distributed version works
;;; Except for not giving the option to override the prompt


(define-presentation-type sequence-enumerated (&rest types)
  :options ((separator #\,) (echo-space t)(prompt nil))
  :inherit-from 't
  :parameters-are-types t)

(define-presentation-method accept ((type sequence-enumerated)
                                    stream
                                    (view textual-view)
                                    &key)
  (loop with separators = (list separator)
        for (first-type . rest-types) on types
        for (element element-type)
           = (multiple-value-list
              (accept first-type :stream stream :view view
                                 :prompt prompt :display-default nil
                                 :additional-delimiter-gestures separators))
        collect element into sequence-val
        do (let ((gesture (stream-read-gesture stream :peek-p t)))
             (when (not (eql gesture separator))
               (loop-finish))
             (stream-read-gesture stream)
             (when echo-space
               ;; Make the space a noise string
               (input-editor-format stream " ")))
        finally (if rest-types
                    (simple-parse-error "Input ~S too short for ~S."
                                        sequence-val types)
                    (return sequence-val))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Patch from ~/quicklisp/local-projects/McCLIM/Core/clim-core/
;;;
;;; :documentation is a legitimate keyword in command argument definitions
;;; but not in the calls to accept that they get compiled into
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clim-internals)


#|

apparently this is now identical to the distribution's code
there was an issue about the :documentation keyword not being
allowed somewhere but allowed elsewhere but maybe that's resolved

(defun accept-form-for-argument (stream arg)
  (let ((accept-keys '(:default :default-type :display-default
		       :prompt
		       ;; :documentation
		       :insert-default)))
    (destructuring-bind (name ptype &rest key-args
			 &key (mentioned-default nil mentioned-default-p)
			 &allow-other-keys)
	arg
      (declare (ignore name))
      `(accept ,ptype :stream ,stream
	       ,@(loop for (key val) on key-args by #'cddr
		       when (member key accept-keys)
		       append `(,key ,val) into args
		       finally (return (if mentioned-default-p
					   `(:default ,mentioned-default
					     ,@args)
					   args)))))))

;;; In the partial command reader accepting-values dialog, default
;;; values come either from the input command arguments, if a value
;;; was supplied, or from the default option for the command argument.
;;;
;;; accept for the partial command reader.  Can this be refactored to
;;; share code with accept-form-for-argument? Probably not.
;;;
;;; original-command-arg is value entered by the user, or
;;; *unsupplied-argument-marker*. command-arg is the current value for the
;;; argument, originally bound to original-command-arg and now possibly
;;; changed by the user.
;;; Is :documentation an acceptable keyword?

(defun accept-form-for-argument-partial
    (stream ptype-arg command-arg original-command-arg query-identifier)
  (let ((accept-keys '(:default :default-type :display-default
		       :prompt :documentation :insert-default)))
    (destructuring-bind (name ptype &rest key-args) ptype-arg
      (declare (ignore name))
      (let* ((defaultp nil)
             (args (loop for (key val) on key-args by #'cddr
                         if (eq key :default)
                           do (setf defaultp t)
                           and append `(:default (if (eq ,command-arg
                                                         *unsupplied-argument-marker*)
                                                     ,val
                                                     ,command-arg))
                         else if (member key accept-keys :test #'eq)
                                append `(,key ,val)))
             (args (append args `(:query-identifier ',query-identifier))))
	(if defaultp
	    `(accept ,ptype :stream ,stream ,@args)
	    `(if (eq ,original-command-arg *unsupplied-argument-marker*)
		 (accept ,ptype :stream ,stream ,@args)
		 (accept ,ptype :stream ,stream :default ,command-arg
			 ,@args)))))))

|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; With-stack-list(*)
;;; Taken from Xlib
;;; Per implementation more efficient versions are possible
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-stack-list ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (MAPCAR #'EVAL '(exp1 ... expN)))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list ,@elements)))
     (declare (type cons ,var)
              (dynamic-extent ,var))
     ,@body))

(defmacro with-stack-list* ((var &rest elements) &body body)
  ;; SYNTAX: (WITH-STACK-LIST* (var exp1 ... expN) body)
  ;; Equivalent to (LET ((var (APPLY #'LIST* (MAPCAR #'EVAL '(exp1 ... expN))))) body)
  ;; except that the list produced by MAPCAR resides on the stack and
  ;; therefore DISAPPEARS when WITH-STACK-LIST is exited.
  `(let ((,var (list* ,@elements)))
     (declare (type cons ,var)
              (dynamic-extent ,var))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; With-accept-help
;;; This is part of the clim spec but not in McClim!
;;;
;;; This uses with-stack-list* see above
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; OPTIONS is a list of a help type followed by a help string (or a function
;; of two arguments, a stream and the help string so far) A "help type" is
;; either a single keyword (either :TOP-LEVEL-HELP or :SUBHELP), or a list
;; consisting of the type and a suboption (:OVERRIDE, :APPEND, or
;; :ESTABLISH-UNLESS-OVERRIDDEN).
;; Specifying :SUBHELP means "Append to previous subhelp, unless an outer
;; context has established an :OVERRIDE".
;; Specifying (:SUBHELP :APPEND) means append no matter what.
;; Specifying (:SUBHELP :OVERRIDE) means "This is the subhelp, subject to
;; lower-level explicit :APPENDs, unless someone above has already :OVERRIDden us.
;; Specifying (<type> :ESTABLISH-UNLESS-OVERRIDDEN) means "Establish <type>
;; at this level, unless someone above has already established <type>."  It does
;; not imply :APPENDING.

(defvar *accept-help* nil)

(defmethod frame-manager-display-help
    (framem frame (stream standard-input-editing-stream) continuation)
  (declare (dynamic-extent continuation))
  (declare (ignore framem frame))
  ;;-- Yuck but think of a better way
  (with-input-editor-typeout (stream :erase t) ;don't scribble over previous output
    (funcall continuation stream)))

(defmacro with-input-editor-help (stream &body body)
  `(flet ((with-input-editor-help-body (,stream) ,@body))
     (declare (dynamic-extent #'with-input-editor-help-body))
     (let* ((frame (pane-frame stream))
            (framem (and frame (frame-manager frame))))
       (frame-manager-display-help
         framem frame stream #'with-input-editor-help-body))))

;; ACTION is either :HELP, :POSSIBILITIES, or :APROPOS-POSSIBILITIES
(defun display-accept-help (stream action string-so-far)
  (with-input-editor-help stream
    (flet ((find-help-clauses-named (help-name)
             (let ((clauses nil))
               (dolist (clause *accept-help* clauses)
                 (when (eq (caar clause) help-name)
                   (push clause clauses)))))
           (display-help-clauses (help-clauses)
             (dolist (clause help-clauses)
               (let ((type (first clause))
                     (args (rest clause)))
                 (declare (ignore type))
                 (fresh-line stream)
                 (typecase (first args)
                   (string (format stream (first args)))
                   (function
                     (apply (first args) stream action string-so-far (rest args))))))))
      (declare (dynamic-extent #'find-help-clauses-named #'display-help-clauses))
      (let ((top-level-help-clauses
              (find-help-clauses-named :top-level-help))
            (subhelp-clauses
              (find-help-clauses-named :subhelp)))
        (cond ((null top-level-help-clauses)
               (fresh-line stream)
               (write-string "No top-level help specified.  Check the code." stream))
              (t (display-help-clauses top-level-help-clauses)))
        (when subhelp-clauses
          (display-help-clauses subhelp-clauses))))))


(defmacro with-accept-help (options &body body)
  (check-type options list)
  (assert (every #'listp options))
  (dolist (option options)
    (let* ((option-name-spec (if (symbolp (first option))
				 `(,(first option) :normal)
				 (first option)))
           (option-name (first option-name-spec))
           (option-type (second option-name-spec))
           (option-args (rest option)))
      (check-type option-name (member :top-level-help :subhelp))
      (check-type option-type (member :normal :append :override :establish-unless-overridden))
      (setq body
            `((with-stack-list* (*accept-help*
				 (list ',option-name-spec ,@option-args) *accept-help*)
                ,@(cond
		    ((eq option-type :override)
		     `((if (assoc (caar *accept-help*) (rest *accept-help*)
				  :test #'(lambda (a b)
					    (and (eq (first a) (first b))
						 (member :override (rest b)))))
			   (pop *accept-help*)
			   (setq *accept-help*
				 (cons (first *accept-help*)
				       (delete ,option-name (rest *accept-help*)
					       :test #'(lambda (a b)
							 (eq (caar b) a))))))))
		    ((eq option-type :append) nil)
		    ((eq option-type :establish-unless-overridden)
		     `((when (assoc (caaar *accept-help*) (rest *accept-help*)
				    :key #'first)
			 (pop *accept-help*))))
		    (t
		     `((when (assoc (caar *accept-help*) (rest *accept-help*)
				    :test #'(lambda (a b)
					      (and (eq (first a) (first b))
						   (member :override (rest b)))))
			 (pop *accept-help*)))))
                ,@body)))))
  `(progn ,@body))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Editing commands in listener -- Work with Slime/Swank
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clim-listener)

(defun edit-function (function-name)
  (clim-sys:make-process (lambda ()
			   #+swank
			   (swank::with-connection ((swank::default-connection))
			     (swank::ed-in-emacs function-name))
			   #-swank
			   (ed pathname))))

(define-listener-command (com-edit-definition
			  :name "Edit Definition"
			  :menu t
			  :provide-output-destination-keyword nil)
    ((function-name 'function-name :prompt "function name"))
  (edit-function function-name))

(defun edit-file (pathname)
  (clim-sys:make-process (lambda ()
			   #+swank
			   (swank::with-connection ((swank::default-connection))
			     (swank::ed-in-emacs pathname))
			   #-swank
			   (ed pathname))))

(define-listener-command (com-edit-file
			  :name "Edit File"
			  :menu t
			  :provide-output-destination-keyword nil)
    ((pathname 'pathname  :prompt "pathname"))
  (edit-file pathname))

;;; Design note: A :write-string event whose first argument is :to-buffer followed by <buffer-name>
;;; Has the effect of writing the string in to the specified buffer, creating it if necessary
;;; and clearing out the prior contents if not.
;;; This is handled on the slime side by a hook for slime-event-hook by checking
;;; if the write string has a :to-buffer keyword
(defmacro with-output-to-emacs-buffer ((&key buffer-name (stream *standard-output*) string) &body body)
  (let ((string-variable (gensym)))
    `(let ((,string-variable ,@(if (and stream body (null string))
				      `((with-output-to-string (,stream) ,@Body))
				      `(,string))))
	  (swank::with-connection ((swank::default-connection))
	      (swank::send-to-emacs
	       `(:write-string :to-buffer ,,buffer-name ,,string-variable)))
	  (values))))

(export 'with-output-to-emacs-buffer)
(import 'with-output-to-emacs-buffer 'clim-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Edit System Command
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command (com-edit-system :command-table listener :name t)
    ((system 'asdf-system
	     :default-type 'system
	     ;; :provide-default t
             )
     &key
     (include-components 'boolean
			 :default t
			 ;; :documentation "Edit files in component systems"
                         )
     ;; (silent 'boolean
     ;;         :default t :mentioned-default t
     ;;         ;; :documentation "Suppress all terminal output"
     ;;         )
     )
  (let ((system (asdf:find-system system)))
    (assert (and (not (null system)) (not (stringp system))) ()
            "There is no system named ~A" system)
    (let ((files (all-files-in-system system include-components)))
      (setq files (sort files #'string-lessp :key #'pathname-name))
      (setq files (multiple-choose-files files))
      (loop for file in files do (edit-file file)))
    ))

(defun multiple-choose-files (files)
  (let ((ht (make-hash-table :test #'equal)))
    (loop for file in files
	  do (setf (gethash file ht) nil))
    (let ((stream *query-io*))
      (accepting-values (stream :own-window t :resynchronize-every-pass t :align-prompts :left)
        (loop for file in files
	      for pathname = (namestring file)
	      for new-value = (clim:accept '(member-alist (("yes" . t) ("no" . nil)))
                                           :view +radio-box-view+
                                           :prompt (pathname-name pathname)
				           :default (gethash file ht)
				           :stream stream)
	      do (terpri stream)
	      do (setf (gethash file ht) new-value))))
      (loop for file in files
	  when (gethash file ht)
	  collect file)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Buffer Destination
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass buffer-destination (climb::output-destination)
  ((destination-buffer :accessor destination-buffer
                       :initarg :destination-buffer)))

(defmethod climb::invoke-with-standard-output
    (continuation (destination buffer-destination))
  (let ((buffer-name (destination-buffer destination)))
    (with-output-to-emacs-buffer (:buffer-name buffer-name :stream *standard-output*)
      (Funcall continuation))))

(clim:define-presentation-method clim:accept
    ((type buffer-destination) stream (view textual-view)
                             &key (prompt "destination buffer-name"))
  (let ((buffer-name (accept 'string :stream stream :view view :prompt prompt)))
    ;; Give subclasses a shot
    (with-presentation-type-decoded (type-name) type
      (make-instance type-name :destination-buffer buffer-name))))


(clim-internals::register-output-destination-type "Buffer" 'buffer-destination)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Copy Output History Command
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-listener-command (com-copy-output-history
			  :name "Copy Output History"
			  :menu t
			  :provide-output-destination-keyword t)
    ()
  (let ((window (clim:get-frame-pane clim:*application-frame* 'interactor)))
    (clim-internals::copy-textual-output-history window *standard-output*)))

;;; Example of use:

;;; (with-output-to-emacs-buffer (:buffer-name "*foo*" :stream bar)
;;;  (print "Howie is a winner, maybe" bar))
;;;   (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Presentation type for package
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clim-listener)

(define-presentation-type package ()
  :inherit-from t
  :history t)

(define-presentation-method accept
    ((type package) stream (view textual-view) &key)
  (values
    (let ((packages (sort (copy-list (list-all-packages)) #'string-lessp
			  :key #'package-name)))
      (completing-from-suggestions (stream :partial-completers '(#\-))
	(map nil #'(lambda (package)
		     (suggest (package-name package) package)
		     (loop for nickname in (package-nicknames package)
			 do (suggest nickname package))
		     )
	     packages)))))

(define-presentation-method present
    (package (type package) stream (view textual-view) &key)
  (write-string (package-name package) stream))

(define-presentation-method presentation-typep (object (type package))
  (typep object 'clim-lisp::package))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Binding Frame's standard output
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Use this to bind STREAM-VAR to something that will be used as *STANDARD-OUTPUT*.
;; Frames with no standard output pane can use a pop-up window, for example.
(defmacro with-frame-standard-output ((stream-var &optional (frame '*application-frame*))
				      &body body)
  `(flet ((with-frame-standard-output-body (,stream-var) ,@body))
     (declare (dynamic-extent #'with-frame-standard-output-body))
     (invoke-with-frame-standard-output ,frame #'with-frame-standard-output-body)))

(defmethod invoke-with-frame-standard-output ((frame standard-application-frame) continuation)
  (declare (dynamic-extent continuation))
  (let ((stream *standard-output*))
    (funcall continuation stream)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Presentation Type for Function-Spec
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-presentation-type function-spec (&key (defined-p nil))
  :history t)

(define-presentation-method accept
    ((type function-spec) stream (view textual-view) &key default)
  (let ((fspec (accept #---ignore 'symbol
		       #+++ignore '((expression) :auto-activate t)
		       :stream stream :view view
		       :default default
		       :prompt nil)))
    ;;--- Extend CLIM's COMPLETE-SYMBOL-NAME to look in the packages
    ;;--- and then make this use it
    (cond (defined-p
	   (if (fboundp fspec)
	     fspec
	     (input-not-of-required-type fspec type)))
	  (t fspec))))

;;--- Use different views to implement support for multiple languages
(define-presentation-method present
    (fspec (type function-spec) stream (view textual-view) &key)
  (prin1 fspec stream))

(define-presentation-method presentation-typep (object (type function-spec))
  (and (or (symbolp object)
	   (and (listp object)
		(or (eql (car object) 'setf)
		    #+Genera (eql (car object) 'sys:locf))))
       (or (not defined-p)
	   (fboundp object))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find string command from Portable Lisp Environment
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This allocates a character an adjustable array
;;; and fills it character by character to avoid
;;; consing a new one each time as read-line would.
;;; I guess this could also be managed by a resource
;;; to avoid creating a new array for each file.


(defun search-file (pathname stream strings conjunction)
  (with-open-file (fs pathname :if-does-not-exist nil)
    (when fs
      (let ((line-number 0)
	    (printed nil))
	(flet ((search-line (line)
		 (when (if (eql conjunction 'and)
			   (every #'(lambda (string) (search string line :test #'char-equal))
				  strings)
			   (some #'(lambda (string) (search string line :test #'char-equal))
				 strings))
		   (unless printed
		     (with-output-as-presentation (stream pathname 'pathname)
		       (format stream "~%In file ~A~%" pathname))
		     (setq printed t))
		   (format stream "#~3D: " line-number)
		   (write-string line stream)
		   (write-char #\Newline stream)
		   (force-output stream))
		 (incf line-number)))
	  (declare (dynamic-extent #'search-line))
	  (loop for line = (read-line fs nil 'eof)
                until (eql line 'eof)
		do (search-line line)))))))


(defmethod all-files-in-system ((system asdf/system:system) &optional (include-components? nil))
  (let ((answer nil) (traversed nil))
    (labels ((do-one (component)
	       (unless (member component traversed)
		 (push component traversed)
		 (typecase component
		  (asdf/lisp-action:cl-source-file
		   (pushnew (translate-logical-pathname (asdf/component:component-pathname component)) answer))
		  ((or asdf/system:system asdf/component:component)
		   (when (or (typep component 'asdf/component:component) include-components?)
		     (let ((children (asdf/component:component-children component)))
		       (loop for thing in children do (do-one thing)))))))))
      (do-one system))
    answer))

(defmethod all-files-in-system ((system asdf/system:system) &optional (include-components? nil))
  (let ((answer nil) (traversed nil))
    (labels ((do-one (component)
	       (unless (member component traversed)
		 (push component traversed)
		 (typecase component
		  (asdf/lisp-action:cl-source-file
		   (pushnew (translate-logical-pathname (asdf/component:component-pathname component)) answer))
		  ((or asdf/system:system asdf/component:component)
		   (when (or (typep component 'asdf/component:component) include-components?)
		     (let ((children (asdf/component:component-children component)))
		       (loop for thing in children do (do-one thing)))))))))
      (do-one system))
    answer))

(define-command (com-find-string :command-table lisp-commands :name t)
    ((strings '(sequence string)
	      :prompt "substring(s)"
	      ;; :documentation "Substrings to search for"
              )
     &key
     (files '(sequence pathname)
	    :default nil
	    :prompt "file(s)"
	    ;; :documentation "Files to search through"
            )
     (systems '(sequence (type-or-string asdf-system))
	      :default nil
	      :prompt "systems(s)"
	      ;; :documentation "Systems to search through"
              )
     (conjunction '(member and or)
		  :default 'and
		  ;; :documentation "AND or OR of the strings"
                  )
     (include-components 'boolean
			 :default t
			 ;;:documentation "Yes or No"
			 ))
  (with-frame-standard-output (stream)
    (when files
      (dolist (pathname files)
	(if (wild-pathname-p pathname)
	  (dolist (p (directory pathname))
	    (search-file p stream strings conjunction))
	  (search-file pathname stream strings conjunction))))
    (when systems
      (flet ((search-system-file (pathname)
               (search-file (typecase pathname
        		      (logical-pathname (translate-logical-pathname pathname))
        		      (t pathname))
        		    stream strings conjunction)))
        (declare (dynamic-extent #'search-system-file))
        (dolist (system systems)
          (loop for pathname in (all-files-in-system system include-components)
              do (search-system-file pathname)))))
    ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Find symbols command from Portable Lisp Environment
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :clim-listener)

(define-command (com-find-symbols :name t
				  :command-table lisp-commands
				  :provide-output-destination-keyword t)
    ((strings '(sequence string) :prompt "substring(s)"
	                         ;; :documentation "Substrings of the symbol to find"
                                 )
     &key
     (packages '(token-or-type (:all) (sequence package))
	       :default :all :prompt "packages to search"
	       ;; :documentation "Packages in which to search"
               )
     (conjunction '(member and or) :default 'and
		  ;; :documentation "AND or OR of the strings"
                  )
     (imported-symbols 'boolean :default nil :mentioned-default t
		       ;; :documentation "Search imported symbols"
                       )
     (used-by 'boolean> :default nil :mentioned-default t
	      ;; :documentation "Search packages used by these packages"
              )
     (using 'boolean :default nil :mentioned-default t
	    ;; :documentation "Search packages that use these packages"
            )
     (types '(sequence (member-alist (("variable" . :variable) ("Function" . :function)
			("Class" . :class) ("Presentation Type" . :presentation-type)
			("Unbound" . :unbound)  ("All" . :all))))
	    :default '(:all) :prompt "symbol types"
	    ;; :documentation "Kinds of symbols to search for"
            )
     (external-symbols 'boolean :default t :mentioned-default t
		       ;; :documentation "Don't show internal symbols of a package"
                       ))
  (with-frame-standard-output (stream)
    (when (null types) (setq types '(:all)))
    (with-text-face (stream :italic)
      (format stream "~&Searching ")
      (if (eql packages :all)
	(write-string "all packages" stream)
	(format-textual-list packages #'(lambda (p s)
					  (present p 'package :stream s))
			     :conjunction "and" :stream stream))
      (when (or using used-by)
	(cond ((and using used-by)
	       (write-string " and packages using and used by" stream))
	      (using
	       (write-string " and packages using" stream))
	      (used-by
	       (write-string " and packages used by" stream)))
	(if (or (eql packages :all)
		(and (listp packages) (> (length packages) 1)))
	  (write-string " them" stream)
	  (write-string " it" stream)))
      (format stream " for ~:[~A~;all~*~] symbols with names containing "
	(member :all types)
	(with-output-to-string (s)
	  (format-textual-list types #'(lambda (thing stream)
					 (format stream "~(~A~)" thing))
			       :conjunction "and" :stream s)))
      (format-textual-list strings #'prin1
			   :conjunction (if (eql conjunction 'and) "and" "or") :stream stream))
    (when (eql packages :all)
      (setq packages (list-all-packages)))
    (let ((this-package nil)
	  (package-name-printed nil))
      (labels ((find-symbols (symbol package)
		 (when (and (or imported-symbols
				(eql (symbol-package symbol) package))
			    (let ((symbol-name (symbol-name symbol)))
			      (if (eql conjunction 'and)
				(every #'(lambda (string)
					   (search string symbol-name :test #'char-equal))
				       strings)
				(some #'(lambda (string)
					  (search string symbol-name :test #'char-equal))
				      strings))))
		   (unless package-name-printed
		     (with-text-face (stream :italic)
		       (format stream "~&In package ")
		       (present this-package 'package :stream stream))
		     (setq package-name-printed t))
		   (display-symbol-and-definitions symbol types :stream stream)))
	       (search-package (package)
		 (setq package-name-printed nil
		       this-package package)
		 (if external-symbols
		     (do-external-symbols (symbol package)
		       (find-symbols symbol package))
		   (do-symbols (symbol package)
		     (find-symbols symbol package))))
	       (map-over-packages (function)
		 (loop for package in packages doing
		   (funcall function package)
		   (when using
		     (dolist (usee (package-used-by-list package))
		       (unless (eql usee package)
			 (funcall function usee))))
		   (when used-by
		     (dolist (user (cons package (package-use-list package)))
		       (unless (eql user package)
			 (funcall function user)))))))
	(declare (dynamic-extent #'find-symbols #'search-package #'map-over-packages))
	(map-over-packages #'search-package)))
    (with-text-face (stream :italic)
      (format stream "~& ... Done."))))

(defun display-symbol-and-definitions (symbol symbol-types &key (stream *standard-output*))
  (let ((printed-symbol nil)
	(printed-delimiter nil))
    ;; We only want to print the symbol name if at least one of the
    ;; WHEN clauses below is true, and we only want to print the
    ;; post-symbol delimiter when we know that there is at least one
    ;; definition type following.
    (labels ((print-definition-type (symbol type-name)
	       (unless printed-symbol
		 ;; Binding *PACKAGE* to NIL forces the short name of the
		 ;; package to be printed
		 (let ((*package* #+Genera nil #-Genera (find-package :keyword)))
		   (fresh-line stream)
		   (write-string "  " stream)
		   (present symbol (cond ((fboundp symbol) 'function-spec)
					 ((boundp symbol) 'form)
					 ((find-class symbol nil) 'class-specifier)
					 (t 'expression))
			    :stream stream))
		 (setq printed-symbol t))
	       (when type-name
		 (with-text-face (stream :italic)
		   (cond (printed-delimiter
			  (write-string ", " stream))
			 (t (write-string " -- " stream)
			    (setq printed-delimiter t)))
		   (write-string type-name stream))))
	     (type-matches (type-name)
	       (or (member ':all symbol-types)
		   (member type-name symbol-types))))
      (declare (dynamic-extent #'print-definition-type #'type-matches))
      ;; The following WHEN clauses check for the various ways that the
      ;; symbol may be defined.
      (when (type-matches :unbound)
	;; No value or definition, but symbol does exist
	(print-definition-type symbol nil))
      (when (and (type-matches :function)
		 (fboundp symbol))
	(print-definition-type
	 symbol
	 (handler-case
	     (with-output-to-string (s)
	       (write-string "Function " s)
	       (print-lambda-list (function-arglist symbol) s :brief t))
	   (error () "Undecipherable function"))))
      (when (and (type-matches :variable)
		 (boundp symbol))
	(print-definition-type symbol "Bound"))
      (when (and (type-matches :class)
		 (not (null symbol))
		 (find-class symbol nil))
	(print-definition-type symbol "Class"))
      (when (and (type-matches :presentation-type)
		 (not (null symbol))
		 (find-presentation-type-class symbol nil))
	(print-definition-type symbol "Presentation Type"))
      (force-output stream))))

(defun function-arglist (function)
  (values (clouseau::function-lambda-list function) t nil))


;; This assumes that it gets called on well-structured lambda lists...
(defparameter *print-lambda-keyword-style* '(nil :italic nil))
(defparameter *print-lambda-keyword-case* :downcase)
(defparameter *print-lambda-type-style* '(nil :italic nil))
(defparameter *print-lambda-type-case* :downcase)

(defmacro with-optional-argument-destructured
	  ((list-cons type arg-var &optional default-var supplied-p-var keyword-var)
	   &body body)
  (let ((object-var '#:object-var))
    `(let ((,arg-var nil)
	   ,@(and keyword-var `((,keyword-var nil)))
	   ,@(and default-var `((,default-var nil)))
	   ,@(and supplied-p-var `((,supplied-p-var nil)))
	   (,object-var (car ,list-cons)))
       (cond ((symbolp ,object-var)
	      (setq ,arg-var ,object-var
		    ,@(and keyword-var
			   `(,keyword-var (intern (symbol-name ,arg-var) :keyword)))))
	     (t
	      (if (eql ,type :optional)
		(setq ,arg-var (first ,object-var))
		(if (consp (first ,object-var))
		  ,(if keyword-var
		     `(setq ,keyword-var (first (first ,object-var))
			    ,arg-var (second (first ,object-var)))
		     `(setq ,arg-var (second (first ,object-var))))
		  (setq ,arg-var (first ,object-var)
			,@(and keyword-var
			       `(,keyword-var (intern (symbol-name ,arg-var) :keyword))))))
	      ,@(when default-var
		  `((when (>= (length ,object-var) 2)
		      (setq ,default-var (second ,object-var)))))
	      ,@(when supplied-p-var
		  `((when (>= (length ,object-var) 3)
		      (setq ,supplied-p-var (third ,object-var)))))))
       ,@body)))


(defun print-lambda-list (lambda-list stream
			  &key brief types (parenthesis t))
  (let (#+Genera (scl:*print-abbreviate-quote* 'si:backquote)
	(*print-pretty* t)
	(print-space nil)
	(state 'required))
    (labels ((print-or-recurse (item &optional no-space)
	       (unless no-space (space))
	       (if (symbolp item)
		 (print-symbol item)
		 (print-lambda-list item stream :brief brief :types types)))
	     (print-type (item)
	       (let ((type (cadr (assoc item types)))
		     (*print-case* *print-lambda-type-case*))
		 (when type
		   (with-text-style (stream *print-lambda-type-style*)
		     (format stream "<~A>" type)))))
	     (print-symbol (item)
	       (if (special-variable-p item)
		 (prin1 item stream)
		 (princ item stream))
	       (print-type item))
	     (print-lambda-keyword (keyword)
	       (space)
	       (with-text-style (stream *print-lambda-keyword-style*)
		 (let ((*print-case* *print-lambda-keyword-case*))
		   (prin1 keyword stream))))
	     (space ()
	       (if print-space
		 (write-char #\space stream)
		 (setq print-space t)))
	     (special-variable-p (var)
	       (and (symbolp var)
		    (boundp var))))
      (when parenthesis
	(write-char #\( stream))
      (map-over-lambda-list
	lambda-list
	#'(lambda (list-point type)
	    (case type
	      (:required (print-or-recurse (car list-point)))
	      (:body
		(print-lambda-keyword '&body)
		(print-or-recurse (car list-point)))
	      (:rest
		(print-lambda-keyword '&rest)
		(print-or-recurse (car list-point)))
	      (:whole
		(print-lambda-keyword '&whole)
		(print-or-recurse (car list-point)))
	      (:environment
		(print-lambda-keyword '&environment)
		(print-or-recurse (car list-point)))
	      (:allow-other-keys
		(print-lambda-keyword '&allow-other-keys))
	      ((:optional :key)
	       (if (eql type ':optional)
		 (when (not (eql state 'optional))
		   (setq state 'optional)
		   (print-lambda-keyword '&optional))
		 (when (not (eql state 'key))
		   (setq state 'key)
		   (print-lambda-keyword '&key)))
	       (with-optional-argument-destructured
		   (list-point type arg default supplied key)
		 (space)
		 (if (or default (and (not brief) supplied))
		   (progn
		     (write-char #\( stream)
		     (if (eql type ':optional)
		       (print-or-recurse arg t)
		       ;; key
		       (cond ((or brief (string= (symbol-name arg)
						 (symbol-name key)))
			      (prin1 key stream)
			      (print-type arg))
			     (t
			      (write-char #\( stream)
			      (prin1 key stream)
			      (write-char #\space stream)
			      (print-or-recurse arg t)
			      (write-char #\) stream))))
		     (write-char #\space stream)
		     (prin1 default stream)
		     (when (and (not brief) supplied)
		       (write-char #\space stream)
		       (print-or-recurse supplied t))
		     (write-char #\) stream))
		   (if (eql type ':optional)
		     (print-or-recurse arg t)
		     (progn
		       (prin1 key stream)
		       (print-type arg))))))
	      (:aux
		(unless brief
		  (unless (eql state 'aux)
		    (print-lambda-keyword '&aux)
		    (setq state 'aux))
		  (let ((whatsit (car list-point)))
		    (if (symbolp whatsit)
		      (print-or-recurse whatsit)
		      (progn
			(write-char #\( stream)
			(prin1 (car whatsit) stream)
			(write-char #\space stream)
			(prin1 (second whatsit))
			(write-char #\) stream))))))
	      (:dotted-tail
		(print-lambda-keyword '&rest)
		(space)
		(print-symbol list-point))
	      (:&-key
		(print-lambda-keyword (car list-point)))	;the &thing itself
	      ))
	:handle-macros t)
      ;; MAP-OVER-LAMBDA-LIST didn't show trailing lambda-list-keywords
      ;; so print the ones that are meaningful in the trailing position,
      ;; but watch out for dotted lists
      (loop for i from 1
	    as list = (last lambda-list i)
	    as item = (car list)
	    while (member item '(&key &allow-other-keys))
	    do (print-lambda-keyword item)
	    until (eql list lambda-list))
      (when parenthesis
	(write-char #\) stream)))))

;; This does very little error checking...
(defun map-over-lambda-list (ll function &key handle-macros)
  (declare (dynamic-extent function))
  (flet ((check-macro-keyword (word)
	   (unless handle-macros
	     (error "~S is not permitted in the lambda list ~S" word ll))))
    (declare (dynamic-extent #'check-macro-keyword))
    (loop with state = 'required
       with special-saved-state = nil
       with key-required = nil
       for arg-point = ll then (cdr arg-point)
       for arg = (and (consp arg-point) (car arg-point))
       as lambda&-p = (and (symbolp arg) (eql (aref (symbol-name arg) 0) #\&))
       until (null arg-point)
       doing
	 (cond ((symbolp arg-point)
		(unless handle-macros
		  (error "The lambda list ~S is a dotted list" ll))
		(funcall function arg-point :dotted-tail)
		(return))
	       (lambda&-p
		(setq key-required nil)
		(case arg
		  (&optional
		   (setq state 'optional))
		  (&rest
		   (case state
		     ((required optional))
		     ((key allow-other-keys)
		      ;; &REST follows &KEY, but who cares
		      )
		     (otherwise
		      (error "&REST must not follow &AUX or &KEY in ~S" ll)))
		   (setq state 'rest))
		  (&key
		   (unless (member state '(required optional rest))
		     (error "&KEY in invalid location in ~S" ll))
		   (setq state 'key))
		  (&aux
		   (setq state 'aux))
		  (&allow-other-keys
		   (unless (eql state 'key)
		     (error "&ALLOW-OTHER-KEYS must follow &KEY in ~S" ll))
		   (setq state 'allow-other-keys)
		   (setq key-required t))
		  (&whole
		   (setq special-saved-state state)
		   (check-macro-keyword '&whole)
		   (unless (member state '(required optional key allow-other-keys))
		     (error "&WHOLE in invalid location in ~S" ll))
		   (setq state 'whole))
		  (&environment
		   (setq special-saved-state state)
		   (check-macro-keyword '&environment)
		   (unless (member state '(required optional key allow-other-keys))
		     (error "&ENVIRONMENT in invalid location in ~S" ll))
		   (setq state 'environment))
		  (&body
		   (setq special-saved-state state)
		   (check-macro-keyword '&body)
		   (unless (member state '(required optional key allow-other-keys))
		     (error "&BODY in invalid location in ~S" ll))
		   (setq state 'body))
		  (otherwise (funcall function arg-point :&-key))))
	       ((not (or (symbolp arg) (consp arg)))
		(error "The lambda lists ~S may contain only symbols and lists, not ~S" ll arg))
	       (t
		(when key-required
		  (error "Non-lambda-keyword ~S where lambda keyword required in ~S" arg ll))
		(case state
		  (required
		   (funcall function arg-point :required))
		  (optional
		   (funcall function arg-point :optional))
		  (rest
		   (setq key-required t)
		   (funcall function arg-point :rest))
		  (aux
		   (funcall function arg-point :aux))
		  (key
		   (funcall function arg-point :key))
		  (whole
		   (setq state special-saved-state)
		   (funcall function arg-point :whole))
		  (environment
		   (setq state special-saved-state)
		   (funcall function arg-point :environment))
		  (body
		   (setq key-required t)
		   (setq state special-saved-state)
		   (funcall function arg-point :body)))))))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Class Presentation Type missing things
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun find-all-classes ()
  (let ((classes nil))
    (labels ((walk-classes (c)
               (pushnew c classes)
	       (mapc #'walk-classes (c2mop::class-direct-subclasses c))))
      (walk-classes (find-class t)))
    classes))

(define-presentation-method accept
    ((type class) stream (view textual-view) &key)
  (let* ((all-classes (find-all-classes)))
    (multiple-value-bind (class success string)
        (completing-from-suggestions (stream :partial-completers '(#\- #\space)
					     :allow-any-input t)
          (map nil #'(lambda (class)
		       (suggest (symbol-name (clim-lisp:class-name  class)) class))
	       all-classes))
      (declare (ignore success))
      (unless class
        (ignore-errors
         (setq class (find-class (read-from-string string) :errorp nil)))
        (unless class
	  (simple-parse-error "~A is not the name of a class" string)))
      class)))

(define-presentation-method accept
    ((type class) stream (view textual-view) &key default)
  (let* ((class-name (accept 'symbol :stream stream :view view
				     :default (and default (class-name default))
				     :prompt nil))
	 (class (find-class class-name nil)))
    (unless class
      (input-not-of-required-type class-name type))
    class))

(define-presentation-method present
    (class (type class) stream (view textual-view) &key)
  (if (typep class 'class)
    (prin1 (class-name class) stream)
    (prin1 class stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Function Accept Method
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-all-generic-functions ()
  (let ((gfs nil))
    (labels ((walk-classes (c)
	       (dolist (m (c2mop:specializer-direct-methods c))
		 (pushnew (c2mop:method-generic-function m) gfs))
	       (mapc #'walk-classes (c2mop:class-direct-subclasses c))))
      (walk-classes (find-class t))
      gfs)))

;;; in contrast to the method in McCLim (listener)
;;; this one actually does completion.  The drawback
;;; is that it has to compute the entire set of generic functions
;;; each time.  There might be a way to cache that and notice when a change
;;; has happened in order to recompute.
(define-presentation-method accept
    ((type generic-function) stream (view textual-view) &key)
  (multiple-value-bind (gf success string)
      (completing-from-suggestions (stream :partial-completers '(#\-)
					   :allow-any-input t)
        (loop for gf in (find-all-generic-functions)
              for name = (c2mop:generic-function-name gf)
              unless (consp name)
                do (suggest (symbol-name name) gf)))
    (declare (ignore success))
    (unless gf
      (ignore-errors
        (setq gf (and (fboundp (read-from-string string))
		      (fdefinition (read-from-string string)))))
      (unless gf
	(simple-parse-error "~A is not the name of a generic function" string)))
    gf))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Generic Function Specializers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-presentation-type generic-function-specializer (generic-function parameter-name)
  :options ((display-as-specializer nil))
  :history nil)

(define-presentation-method accept
    ((type generic-function-specializer) stream (view textual-view) &key)
  (let* ((what (with-accept-help
		   ((:subhelp #+++ignore (:subhelp :append)
		     #'(lambda (stream action string-so-far)
			 (declare (ignore action string-so-far))
			 (format stream "How do you wish to specialize the ~A argument to ~A (~{~:(~A~)~^, ~})?"
			   parameter-name
			   (clouseau::function-name generic-function)
			   '(:object :class)))))
		 (accept '(member :object :class)
			 :stream stream
			 :prompt (format nil "argument ~A" (symbol-name parameter-name))
			 :provide-default nil
			 :additional-delimiter-gestures '(#\space))))
	 thing)
    (flet ((eat-delimiter ()
	     (unless (eql (stream-read-gesture stream) #\space)
	       (simple-parse-error "You must type a space after ~A" what))))
      (declare (dynamic-extent #'eat-delimiter))
      (setq thing
	    (case what
	      (:object
		(eat-delimiter)
		(with-accept-help
		    ((:subhelp #+++ignore (:subhelp :append)
		      #'(lambda (stream action string-so-far)
			  (declare (ignore action string-so-far))
			  (format stream
			      "An object on which to specialize the ~A argument of ~A"
			    parameter-name
			    (c2mop:generic-function-name generic-function)))))
		  (multiple-value-bind (exp type)
		      (accept 'expression
			  :stream stream
			  :prompt nil
			  :provide-default nil)
		    (values (if (symbolp exp) exp (eval exp))
			    type))))
	      (:class
		(eat-delimiter)
		(let* ((subprompt (format nil "the ~A argument to ~A"
				    parameter-name
				    (c2mop:generic-function-name generic-function)))
		       (class (with-accept-help
				  ((:subhelp #+++ignore (:subhelp :append)
				    #'(lambda (stream action string-so-far)
					(declare (ignore action string-so-far))
					(format stream "A class on which ~A is specialized" subprompt))))
				(accept 'class
					:stream stream
					:prompt nil))))
		  class)))))
    (list what thing)))

(define-presentation-method present
    (object (type generic-function-specializer) stream (view textual-view) &key)
  (if display-as-specializer
    (case (car object)
      (:object (format stream "(~S ~S)" 'eql (second object)))
      (:class (present (second object) 'class :stream stream)))
    (progn
      (format stream "~@(~A~)" (car object))
      (case (car object)
	(:object
	 (write-char #\space stream)
	 (prin1 (second object) stream))
	(:class
	 (write-char #\space stream)
	 (present (second object) 'class :stream stream))))))

(define-presentation-method presentation-typep
    (object (type generic-function-specializer))
  (and (listp object)
       (null (cddr object))
       (case (first object)
	 (:class (typep (second object) 'class))
	 (:object t)
	 (otherwise nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Accept a sequence of specializes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(eval-when (:compile-toplevel :load-toplevel :execute)
(define-presentation-type generic-function-specializers (generic-function)
  :inherit-from t
  :history nil
  :options ((prompt nil)
	    (display-as-specializer nil))))

(define-presentation-method accept
    ((type generic-function-specializers) stream (view textual-view) &key)
  ;;--- It would sure be nice if we could flush the trailing "Class T" stuff

  (let* ((required (let ((required nil))
		     (map-over-lambda-list
		      (c2mop:generic-function-lambda-list generic-function)
		      #'(lambda (args type)
			  (when (eq type :required)
			    (push (car args) required))))
		     (nreverse required))
	   )
         (ptypes (mapcar #'(lambda (arg)
			     `(generic-function-specializer ,generic-function ,arg))
			  required)))
    (accept `((sequence-enumerated ,@ptypes) :separator #\,)
            :stream stream
            :prompt nil)))

(define-presentation-method present
    (object (type generic-function-specializers) stream (view textual-view) &key)
  (format-textual-list
    object #'(lambda (spec stream)
	       (present spec `((generic-function-specializer ,generic-function #:any)
			       :display-as-specializer ,display-as-specializer)
			:stream stream))
    :stream stream :separator ", "))

(define-presentation-method presentation-typep
    (object (type generic-function-specializers))
  (with-presentation-type-parameters (generic-function-specializers type)
    (and (listp object)
	 (every #'(lambda (x) (presentation-typep x `(generic-function-specializer ,generic-function))) object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Undefine Method Command
;;; Still need to provide find-applicable-methods
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-command (com-undefine-method
		 :name t
		 :command-table lisp-commands
		 :provide-output-destination-keyword nil)
    ((generic-function 'generic-function)
     (specializers `((generic-function-specializers ,generic-function))
		   :prompt "specializers")
     &key
     (qualifiers '(clim:sequence symbol)
		 :default nil))
  (let ((methods (c2mop:find-applicable-methods generic-function specializers qualifiers)))
    (loop for method in methods
	do (remove-method generic-function method))))


(in-package :c2mop)

;;; This should be in the same package as remove method or maybe in c2mop
(defun find-applicable-methods (generic-function arg-specs qualifiers)
  (let ((answers nil))
    (loop for method in (c2mop:generic-function-methods generic-function)
       when (loop for specializer in (c2mop:method-specializers method)
	       for (arg-type arg) in arg-specs
	       always (or (and (typep specializer 'c2mop:eql-specializer)
			       (eql arg-type :object)
			       (eql arg (c2mop:eql-specializer-object specializer)))
			  (and (eq arg-type :class)
			       (eql arg specializer))))
         do (unless (set-exclusive-or (method-qualifiers method) qualifiers)
	      (push method answers)))
    answers))

(export 'c2mop::find-applicable-methods)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Swank Support for General Purpose undefining command
;;;
;;; The basic structure is a top level dispatcher that is given the whole defining
;;; form (which SLIME has found and called SWANK with).  Curretly SLIME just
;;; looks backward for a line that starts with a defun and the goes forware one sexp.
;;;
;;; SWANK's assumption is that the first token in that form is the definition type.
;;; So it calls a method whose first argument is discriminated by that type
;;; and whose second argument is the whole form.
;;;
;;; The first dispatch just extracts all the relevant information
;;; and returns 2 things:
;;;  1) A keyword that is used to find the right handler
;;;  2) All the information that handler will need to do its job
;;;
;;; Then it does another eql type dispatch to find the code
;;; that actually does the undefining.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; To do: Switch the 2nd level handlers to actually do the work


(in-package :swank)

(export '(swank::undefine-definition))

(defgeneric form-parser (key form))
(defgeneric definition-undefining-form (type form  do-it?))

(defslimefun undefine-definition (definition-string)
  (let ((form (if (stringp definition-string) (from-string definition-string) definition-string)))
    (multiple-value-bind (fspec type) (form-parser (first form) form)
      (let ((answer (definition-undefining-form type fspec t)))
        ;; this will just the undefning form into the minibuffer
        answer))))


;;; The parsers

(defmethod form-parser ((key t) form) (values (second form) :operator))

(defmethod form-parser ((key (eql 'defun)) form) (values (second form) :operator))

(defmethod form-parser ((key (eql 'defgeneric)) form) (values (second form) :operator))

;;; Need to sync this up with the code in the lisp-listener command undefine-method
(defmethod form-parser ((key (eql 'defmethod)) form)
  (let* ((non-body (rest form))
         (name (pop non-body))
         qualifiers
         specializers)
    (loop for thing = (pop non-body)
          if (symbolp thing)
            do (push thing qualifiers)
          else do (setq specializers thing)
                  (return))
    (values (list name                  ;generic function name
                  specializers          ;specializers
                  (reverse qualifiers)  ;qualifiers if any
                  )
            :defmethod)))


(defmethod form-parser ((key (eql 'defclass)) form)
  (values (second form) :class))





;;; The handlers: do the dirty work and the return a form to be echo'd in the mini buffer

(defmethod definition-undefining-form (type fspec do-it?)
  (declare (ignore do-it?))
  (format nil  "Do not know how to kill definition ~S of type ~S." fspec type))

(defmethod definition-undefining-form ((type (eql :operator)) fspec do-it?)
  (when do-it? (fmakunbound fspec))
  `(fmakunbound ',fspec))



(defmethod definition-undefining-form ((type (eql :class)) fspec do-it?)
  (when do-it? (setf (find-class fspec) nil))
  `(setf (find-class ',fspec) nil))



;;; This needs to get fixed
(defmethod definition-undefining-form ((type (eql :defmethod)) fspec do-it?)
  (destructuring-bind (generic-function-name specializers qualifiers) fspec
    (when do-it?
      (let* ((generic-function (fdefinition generic-function-name))
             (methods (c2mop:find-applicable-methods generic-function
                                                     (fix-up-specializers specializers)
                                                     qualifiers)))
        (loop for method in methods do (remove-method generic-function method)))
      `(undefmethod ,generic-function-name ,@qualifiers ,specializers))))

;;; Code to put this into the same form as the clim-listener command?
(defun fix-up-specializers (specializers)
  (loop for specializer in specializers
        collect (cond
                  ((symbolp specializer) (list :class (find-class t)))
                  ((eql (first (second specializer)) 'eql) (list :object (second (second specializer))))
                  (t (find-class (second specializer))))))

;;; Don't need this since the form parser macroexpands it and then treats it like a
;;; normal defmethod.  The only reason to have this is if you want to feed back
;;; a more specific undefining form to the editor.
;;;
;;; (defmethod definition-undefining-form (fspec (type (eql :joshua-define-predicate-method)))
;;;   (let* ((method (third (macroexpand `(joshua::define-predicate-method ,(second fspec) ,(third fspec)))))
;;;  	 (real-fspec (excl::defmethod-parser method))
;;;  	 (signature (fixed-defmethod-fspec real-fspec)))
;;;     `(fmakunbound ',signature)
;;;     ))






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Patch of :around method for accept of sequence
;;;  In MccLim/apps/clim-listener/listener.lisp
;;;  This method is bogus and should be removed
;;;
;;; Actually on further consideration 10/9/2021, it's specialized on view = listener-view
;;; which isn't the case while accepting command arguments, so this doesn't
;;; break undefine method as I previously thought.
;;;
;;; The interactor/listener pane in the listener uses this view and only
;;; to change the behavior of how sequence is accepted and presented.
;;;
;;; Here's the discussion about it:
;;;
;;; run-command doesn't use it because it doesn't supply the listener-view. This method is used to read a sequence if accept is typed in the listener directly, i.e.:
;;; clim-user> (accept '(sequence integer))
;;; Enter sequence: 1, 2, 3 ;; --> error Input 1 is not of type (sequence integer)
;;;
;;; clim-user> (accept '(sequence integer))
;;; Enter sequence: (1 2 3)
;;; 0 (1 2 3)
;;; 1 (sequence integer)
;;;
;;; This abstraction has been introduced in the commit 6769f25 that replaces a hack hackish-present with a proper view specialization. The hacked function was:
;;;
;;; (defun hackish-present (object)
;;;   "Hack of the day.. let McCLIM determine presentation type to use, except for lists, because the list presentation method is inappropriate for lisp return values."
;;;   (typecase object
;;;     (sequence (present object 'expression))
;;;     (t (present object))))
;;; And hackish-present was introduced itself in da4a320 that in display-values instead of presenting the value as expression
;;; takes the detour to present lists as expressions and everything else for their presentation type. This is to ensure that
;;; (present #p"/tmp")
;;;  will output a pathname (and not the expression), so we can use it as a presentation of type "pathname" with translators and such.
;;; Does this code impact listener in some way that would be a downside?
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
