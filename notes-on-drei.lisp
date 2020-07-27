;;; -*- Mode: Lisp; Package: DREI-LISP-SYNTAX -*-

;;;  (c) copyright 2005, 2014 by
;;;           Robert Strandh (robert.strandh@gmail.com)
;;;  (c) copyright 2006-2007 by
;;;           Troels Henriksen (athas@sigkill.dk)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; A syntax module for analysing Common Lisp using an LR based
;;; parser.

;; In ~/quicklisp/local-projects/McCLIM/Libraries/Drei/base.lisp:

;; Modify constituentp so that #\[ and #\] aren't in the list

(in-package :drei-base)

(defun constituentp (obj)
  "A predicate to ensure that an object is a constituent character."
  (and (characterp obj)
       ;; #+sbcl (sb-impl::constituentp obj)
       (or (alphanumericp obj)
           (member obj '(#\! #\$ #\% #\& #\* #\+ #\- #\. #\/
                         #\: #\< #\= #\> #\@ #\^ #\~ #\_
                         #\{ #\} #\#)))))



(in-package :drei-lisp-syntax)


;;In ~/quicklisp/local-projects/McCLIM/Libraries/Drei/lisp-syntax.lisp

;; Add lexeme type for #\[ and #\]

(defclass left-bracket-lexeme (parenthesis-lexeme) ())
(defclass right-bracket-lexeme (parenthesis-lexeme) ())
(defclass unmatched-right-bracket-lexeme (lisp-lexeme) ())

(defclass logic-variable-lexeme (parenthesis-lexeme) ())
(defclass logic-variable-termination-lexeme (parenthesis-lexeme) ())

;; modify top-level lex method with a case for #\[ that produces left-bracket-lexeme

;; lex method for lexr-list-state

;; when get #\] add right-bracket-lexeme

(defmethod lex ((syntax lisp-syntax) (state lexer-toplevel-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
        (#\( (fo) (make-instance 'left-parenthesis-lexeme))
        (#\) (fo) (make-instance 'unmatched-right-parenthesis-lexeme))
        (#\[ (fo) (make-instance 'left-bracket-lexeme))
        (#\] (fo) (make-instance 'unmatched-right-bracket-lexeme))
        (#\' (fo) (make-instance 'quote-lexeme))
        (#\? (fo) (make-instance 'logic-variable-lexeme))
        (#\; (fo)
             (loop until (or (end-of-buffer-p scan)
                             (end-of-line-p scan)
                             (not (eql (object-after scan) #\;)))
                   do (fo))
             (make-instance 'line-comment-start-lexeme))
        (#\" (fo) (make-instance 'string-start-lexeme))
        (#\` (fo) (make-instance 'backquote-lexeme))
        (#\, (fo)
             (cond ((end-of-buffer-p scan)
                    (make-instance 'incomplete-lexeme))
                   (t
                    (case (object-after scan)
                      (#\@ (fo) (make-instance 'comma-at-lexeme))
                      (#\. (fo) (make-instance 'comma-dot-lexeme))
                      (t (make-instance 'comma-lexeme))))))
        (#\# (fo)
             (cond ((end-of-buffer-p scan)
                    (make-instance 'incomplete-lexeme))
                   (t
                    (let ((prefix 0))
                      (loop until (end-of-buffer-p scan)
                            while (and (characterp (object-after scan))
                                       (digit-char-p (object-after scan)))
                            do (setf prefix
                                     (+ (* 10 prefix)
                                        (digit-char-p (object-after scan))))
                               (fo))
                    (if (or (end-of-buffer-p scan)
                            (not (characterp (object-after scan))))
                        (make-instance 'incomplete-lexeme)
                        (case (object-after scan)
                          ((#\Backspace #\Tab #\Newline #\Linefeed
                                        #\Page #\Return #\Space #\) #\])
                           (fo)
                           (make-instance 'error-lexeme))
                          (#\\ (fo)
                               (cond ((or (end-of-buffer-p scan)
                                          (not (characterp (object-after scan))))
                                      (make-instance 'incomplete-character-lexeme))
                                     ((not (constituentp (object-after scan)))
                                      (fo) (make-instance 'complete-character-lexeme))
                                     (t (loop until (end-of-buffer-p scan)
                                           while (constituentp (object-after scan))
                                           do (fo))
                                        (make-instance 'complete-character-lexeme))))
                          (#\' (fo)
                               (make-instance 'function-lexeme))
                          (#\( (fo)
                               (make-instance 'simple-vector-start-lexeme))
                          (#\* (fo)
                               (loop until (end-of-buffer-p scan)
                                  while (or (eql (object-after scan) #\1)
                                            (eql (object-after scan) #\0))
                                  do (fo))
                               (if (and (not (end-of-buffer-p scan))
                                        (constituentp (object-after scan)))
                                   (make-instance 'error-lexeme)
                                   (make-instance 'bit-vector-form)))
                          (#\: (fo)
                               (make-instance 'uninterned-symbol-lexeme))
                          (#\. (fo)
                               (make-instance 'readtime-evaluation-lexeme))
                          ((#\B #\b #\O #\o #\X #\x)
                           (let ((radix
                                  (ecase (object-after scan)
                                    ((#\B #\b) 2)
                                    ((#\O #\o) 8)
                                    ((#\X #\x) 16))))
                             (fo)
                             (when (and (not (end-of-buffer-p scan))
                                        (char= (object-after scan)
                                               #\-))
                               (fo))
                             (loop until (end-of-buffer-p scan)
                                while (digit-char-p (object-after scan) radix)
                                do (fo)))
                           (if (and (not (end-of-buffer-p scan))
                                    (constituentp (object-after scan)))
                               (make-instance 'error-lexeme)
                               (make-instance 'number-lexeme)))
                          ((#\R #\r)
                           (fo)
                           (cond
                             ((<= 2 prefix 36)
                              (loop until (end-of-buffer-p scan)
                                 while (and (characterp (object-after scan))
                                            (digit-char-p (object-after scan) prefix))
                                 do (fo))
                              (if (and (not (end-of-buffer-p scan))
                                       (constituentp (object-after scan)))
                                  (make-instance 'error-lexeme)
                                  (make-instance 'number-lexeme)))
                             (t (make-instance 'error-lexeme))))
                                        ;((#\C #\c) )
                          ((#\A #\a) (fo)
                           (make-instance 'array-start-lexeme))
                          ((#\S #\s) (fo)
                           (cond ((and (not (end-of-buffer-p scan))
                                       (eql (object-after scan) #\())
                                  (fo)
                                  (make-instance 'structure-start-lexeme))
                                 ((end-of-buffer-p scan)
                                  (make-instance 'incomplete-lexeme))
                                 (t (make-instance 'error-lexeme))))
                          ((#\P #\p) (fo)
                           (make-instance 'pathname-start-lexeme))
                          (#\= (fo)
                               (make-instance 'sharpsign-equals-lexeme))
                          (#\# (fo)
                               (make-instance 'sharpsign-sharpsign-form))
                          (#\+ (fo)
                               (make-instance 'reader-conditional-positive-lexeme))
                          (#\- (fo)
                               (make-instance 'reader-conditional-negative-lexeme))
                          (#\| (fo)
                               (make-instance 'long-comment-start-lexeme))
                          (#\< (fo)
                               (make-instance 'error-lexeme))
                          (t (fo) (make-instance 'undefined-reader-macro-lexeme))))))))
        (#\| (fo) (make-instance 'multiple-escape-start-lexeme))
        (t (cond ((or (constituentp object)
                      (eql object #\\))
                  (lex-token syntax scan))
                 (t (fo) (make-instance 'literal-object-form))))))))

(defmethod lex ((syntax lisp-syntax) (state lexer-list-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (case object
        (#\) (fo) (make-instance 'right-parenthesis-lexeme))
        (#\] (fo) (make-instance 'right-bracket-lexeme))
        (t (call-next-method))))))

#| for refernce


;;; parse trees
(defclass list-form (form) ())
(defclass complete-list-form (list-form complete-form-mixin) ())
(defclass incomplete-list-form (list-form incomplete-form-mixin) ())

(define-parser-state |( form* | (lexer-list-state form-may-follow) ())
(define-parser-state |( form* ) | (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow left-parenthesis-lexeme) |( form* |)
(define-new-lisp-state (|( form* | form) |( form* |)
(define-new-lisp-state (|( form* | comment) |( form* |)
(define-new-lisp-state (|( form* | right-parenthesis-lexeme) |( form* ) |)

;;; reduce according to the rule form -> ( form* )
(define-lisp-action (|( form* ) | t)
  (reduce-until-type complete-list-form left-parenthesis-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (|( form* | (eql nil))
  (reduce-until-type incomplete-list-form left-parenthesis-lexeme t))

|#



;;; parse tree for [ ... ]

(defclass pred-form (form) ())
(defclass complete-pred-form (pred-form complete-form-mixin) ())
(defclass incomplete-pred-form (pred-form incomplete-form-mixin) ())

(define-parser-state open-pred-state (lexer-list-state form-may-follow) ())
(define-parser-state closed-pred-state (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow left-bracket-lexeme)  open-pred-state)
;;; Note: this has to be FORM for the second term (i.e. once you're accepting a pred
;;; and form advances, not just pred forms
(define-new-lisp-state (open-pred-state form) open-pred-state)
(define-new-lisp-state (open-pred-state comment) open-pred-state)
(define-new-lisp-state (open-pred-state right-bracket-lexeme) closed-pred-state)

;;; reduce according to the rule pred -> ( pred* )
(define-lisp-action (closed-pred-state t)
  (reduce-until-type complete-pred-form left-bracket-lexeme))

;;; reduce at the end of the buffer
(define-lisp-action (open-pred-state (eql nil))
  (reduce-until-type incomplete-pred-form left-bracket-lexeme t))




;;; logic variable
(defclass logic-variable-form (form) ())
(defclass complete-logic-variable-form (logic-variable-form complete-form-mixin) ())
(defclass incomplete-logic-variable-form (logic-variable-form incomplete-form-mixin) ())

(define-parser-state open-lv-state (form-may-follow) ())
(define-parser-state open-lv-with-complete-form (lexer-toplevel-state parser-state) ())
(define-parser-state open-lv-with-incomplete-form (lexer-toplevel-state parser-state) ())

(define-new-lisp-state (form-may-follow logic-variable-lexeme) open-lv-state)
(define-new-lisp-state (open-lv-state complete-form-mixin) open-lv-with-complete-form)
(define-new-lisp-state (open-lv-state incomplete-form-mixin) open-lv-with-incomplete-form)
(define-new-lisp-state (open-lv-state comment) open-lv-state)
(define-new-lisp-state (open-lv-state unmatched-right-parenthesis-lexeme) |( form* ) |)

;;; reduce according to the rule form -> ? form
(define-lisp-action (open-lv-with-complete-form t)
  (reduce-until-type complete-logic-variable-form logic-variable-lexeme))

(define-lisp-action (open-lv-with-incomplete-form t)
  (reduce-until-type incomplete-logic-variable-form logic-variable-lexeme))

(define-lisp-action (open-lv-state right-parenthesis-lexeme)
  (reduce-until-type incomplete-logic-variable-form logic-variable-lexeme))

(define-lisp-action (open-lv-state unmatched-right-parenthesis-lexeme)
  (reduce-until-type incomplete-logic-variable-form logic-variable-lexeme))

(define-lisp-action (open-lv-state (eql nil))
  (reduce-until-type incomplete-logic-variable-form logic-variable-lexeme t))

(define-lisp-action (open-lv-state logic-variable-termination-lexeme)
  (reduce-until-type complete-logic-variable-form logic-variable-lexeme))

;;; This Is here so that we don't ignore whitespace but treate
;;; it as a delimiter if seen right after a ?
(defmethod skip-inter ((syntax lisp-syntax) (state open-lv-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (if (end-of-buffer-p scan)
        nil
        t)))

(defmethod lex ((syntax lisp-syntax) (state open-lv-state) scan)
  (macrolet ((fo () `(forward-object scan)))
    (let ((object (object-after scan)))
      (if (or (member object '(#\] #\)))
              (whitespacep syntax object))
          (make-instance 'logic-variable-termination-lexeme)
          (call-next-method)))))


#|Notes on anonymous LV's

1) Need to update the transition rules or perhaps a lex method (or both) so that whn you're in the open
   lv state you peek ahead for whitespace or right paren or right bracket or return and then
   transition to complete lv state
2) The form-to-object method needs to check if there's a token and if not make an anonymous lv

This is done but ? followed by return doesn't work (who cares?)
? space followed by return does work

|#



(defmethod form-to-object ((syntax lisp-syntax) (form pred-form) &rest args
                           &key &allow-other-keys)
  (labels ((recurse (elements)
             (unless (null elements)
               (handler-case
                   (nconc (multiple-value-list
                           (apply #'form-to-object syntax (first elements) args))
                          (recurse (rest elements)))
                 (reader-invoked (c)
                   (let ((remaining-elements (remove (offset (end-mark c)) elements
                                                     :key #'start-offset :test #'>)))
                     (if (and (not (null (rest elements)))
                              (null remaining-elements))
                         (signal c)
                         `(cons (object c) (recurse remaining-elements)))))))))
    (let ((inner-form (recurse (remove-if-not #'formp (children form))))
          (backquoted (typep (second (children form)) 'complete-backquote-form)))
      (if backquoted
          `(ji::predication-maker ,@inner-form)
          `(ji::predication-maker '(,@inner-form))))))

(defmethod form-to-object ((syntax lisp-syntax) (form complete-logic-variable-form) &rest args
                           &key &allow-other-keys)
  (let ((provided-name (second (children form))))
    (cond
      ((null provided-name)
       `(ji::logic-variable-maker
         ,(intern (string (gentemp ji::*anonymous-prefix*)))))
      (t `(ji::logic-variable-maker
           ,(intern (concatenate 'string
                                 "?"
                                 (string (apply #'form-to-object syntax provided-name args)))))))))


#|

The next major hurdle is backquoted predications

Joshua does a few weird things:
1) Provides it's own reader that produces a joshua oriented backquote form with its own magic tokens.
2) Reverses the backquote and the make-predication so that `[ turns into  (predication-maker (ji:backquote (... statement with ji:*backquote-comma-flag* etc)))
3) Predication-maker is a macro that walks the structure of this and turns into a make-predication form wrapped in a let that creates the internal logic-variables

In terms of dealing with DREI it would be wrong to do anything with the way the lexer works, that would mess up input editing very badly
So everything must be done while processing form-to-object
Two options:
1) An around method that reverses the lexemes for backquote and predication-maker, calls the existing method and then walks over that
   changind the drei:backquote and relateds symbols to their Joshua equivalents
2) Do all the processing on the parse-tree ourselves

Option 2 seems daunting, so I'll try option 1 first.

the things that need to be recognized when walking are:


(defconstant +quote-marker+ 'quote
  "The marker used for identifying quote forms in backquoted
forms.")
(defconstant +quote-nil-marker+ (list +quote-marker+ nil))
backquote-list
backquote-append
backquote-list*
backquote-nconc
readackquote-vector



Another issue: Should I create a joshua syntax inheriting from lisp syntax


|#

;;; The complete-backquote-form will have 2 children:
;;;   a backquote-lexeme
;;;   a complete-pred-form

;;; The complete-pred-from has n children, structured as
;;;   a left-backet-lexeme
;;;   stuff*
;;;   a right-bracket-lexeme

(defmethod form-to-object :around ((syntax lisp-syntax) (form backquote-form)
                                   &key &allow-other-keys)
  (destructuring-bind (backquote-lexeme internal-form)  (children form)
    (declare (ignore backquote-lexeme))
    (if (typep internal-form 'complete-pred-form)
        (let* ((outer-backquote-form (call-next-method))
               (code (convert-to-joshua-backquote-form outer-backquote-form)))
          code
          )
        (call-next-method))))

#|

The problem with this approach is that apparently including the original objects as children
in the structure that's built-up causes side effects to their pointers into the buffer (I think)
and therefore you'd need to do the minimal deep copying necessary to avoid that.

see: defmethod initialize-instance :after ((parser-symbol nonterminal) &rest args) in lr-syntax

(defun reverse-backquote-pred-form (backqoute-form)
  (destructuring-bind (backquote-lexeme pred-form)  (children backqoute-form)
    (destructuring-bind (left-bracket-lexeme . stuff) (children pred-form)
      (let* ((body (butlast stuff))
             (right-bracket-lexeme (first (last stuff)))
             (backquote-type (type-of backqoute-form))
             (Pred-type (Type-of pred-form))
             (backquote-form (make-instance backquote-type
                                            :children (list backquote-lexeme
                                                            (make-instance 'complete-list-form
                                                                           :children (concatenate 'list
                                                                                                  (list (make-instance 'left-parenthesis-lexeme))
                                                                                                  ;; I think that this screws up the internal back pointers
                                                                                                  ;; of these objects which are still the children of the original form
                                                                                                  body
                                                                                                  (list (make-instance 'right-parenthesis-lexeme)))))))
             (answer (make-instance pred-type
                                    :children (list left-bracket-lexeme
                                                    backquote-form
                                                    right-bracket-lexeme))))
        answer))))

|#

(defun convert-to-joshua-backquote-form (form)
  (destructuring-bind (backquote-token make-pred-token outer-drei-backquoted nconc-token) form
    (declare (ignore backquote-token nconc-token))
    ;; make-pred-token is actually quoted so need to take second of it
    (destructuring-bind (backquote-token quote-token drei-backquoted nconc-token) outer-drei-backquoted
      (declare (ignore backquote-token quote-token nconc-token))
      (setq ju::*baz* drei-backquoted)
      (labels ((rebuild-body (Form &optional (top-level nil))
                 (cond
                   ((symbolp form) form)
                   ((eql (first form) 'quote) form)
                   ((and (eql (first form) 'backquote-list*)
                         (eql (first (first (last form))) 'backquote-nconc))
                    (if top-level
                        (list 'ji::backquote (rebuild-body (butlast (rest form))))
                        (rebuild-body (butlast (rest form)))))
                   (t
                    (loop for token in form
                          if (and (listp token) (eql (first token) 'quote))
                            collect (second token)
                          else collect (rebuild-body token))))))
        (list (second make-pred-token)
               (rebuild-body drei-backquoted t))))))
