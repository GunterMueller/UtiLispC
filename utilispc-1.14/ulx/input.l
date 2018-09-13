;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; This file contains definitions for the DISPLAY object for Common-Lisp X windows version 11

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 2909
;;;			       AUSTIN, TEXAS 78769
;;;
;;; Copyright (C) 1987 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

;;;
;;; Change history:
;;;
;;;  Date	Author	Description
;;; -------------------------------------------------------------------------------------
;;; 12/10/87	LGO	Created

(in-package 'xlib :use '(lisp))

(export '(
	  event-listen
	  queue-event
	  process-event
	  make-event-handlers
	  event-handler
	  event-case
	  event-cond
	  discard-current-event
	  request-error
	  value-error
	  window-error
	  pixmap-error
	  atom-error
	  cursor-error
	  font-error
	  match-error
	  drawable-error
	  access-error
	  alloc-error
	  colormap-error
	  gcontext-error
	  id-choice-error
	  name-error
	  length-error
	  implementation-error
	  request-error
	  resource-error
	  unknown-error
	  access-error
	  alloc-error
	  atom-error
	  colormap-error
	  cursor-error
	  drawable-error
	  font-error
	  gcontext-error
	  id-choice-error
	  illegal-request-error
	  length-error
	  match-error
	  name-error
	  pixmap-error
	  value-error
	  window-error
	  implementation-error
	  type-error
	  closed-display
	  lookup-error
	  connection-failure
	  reply-length-error
	  reply-timeout
	  sequence-error
	  unexpected-reply
	  missing-parameter
	  invalid-font
	  device-busy
	  get-external-event-code
	  define-extension
	  extension-opcode
	  define-error
	  decode-core-error
	  declare-event
	  ))

;; Event Resource
(defvar *event-free-list* nil) ;; List of unused (processed) events

(eval-when (eval compile load)
(defconstant *max-events* 64) ;; Maximum number of events supported (the X11 alpha release only has 34)
(defvar *event-key-vector* (make-array *max-events* :initial-element nil)
  "Vector of event keys - See define-event")
)
(defvar *event-macro-vector* (make-array *max-events* :initial-element nil)
  "Vector of event handler functions - See declare-event")
(defvar *event-handler-vector* (make-array *max-events* :initial-element nil)
  "Vector of event handler functions - See declare-event")
(defvar *event-send-vector* (make-array *max-events* :initial-element nil)
  "Vector of event sending functions - See declare-event")

(defun allocate-event ()
  (or (threaded-atomic-pop *event-free-list* reply-next reply-buffer)
      (make-reply-buffer *replysize*)))

(defun deallocate-event (reply-buffer)
  (comment declare (type reply-buffer reply-buffer))
  (setf (reply-size reply-buffer) *replysize*)
  (threaded-atomic-push reply-buffer *event-free-list* reply-next reply-buffer))

;; Extensions are handled as follows:
;; DEFINITION:	Use DEFINE-EXTENSION
;;
;; CODE:	Use EXTENSION-CODE to get the X11 opcode for an extension.
;;		This looks up the code on the display-extension-alist.
;;
;; EVENTS:	Use DECLARE-EVENT to define events. This calls ALLOCATE-EXTENSION-EVENT-CODE
;;		at LOAD time to define an internal event-code number
;;		(stored in the 'event-code property of the event-name)
;;		used to index the following vectors:
;;		*event-key-vector* 	Used for getting the event-key
;;		*event-macro-vector*	Used for getting the event-parameter getting macros
;;
;;		The GET-INTERNAL-EVENT-CODE function can be called at runtime to convert
;;		a server event-code into an internal event-code used to index the following
;;		vectors:
;;		*event-handler-vector*	Used for getting the event-handler function
;;		*event-send-vector*	Used for getting the event-sending function
;;
;;		The GET-EXTERNAL-EVENT-CODE function can be called at runtime to convert
;;		internal event-codes to external (server) codes.
;;
;; ERRORS:	Use DEFINE-ERROR to define new error decodings.
;;


;; Any event-code greater than 34 is for an extension
(defparameter *first-extension-event-code* 35)

(defvar *extensions* nil) ;; alist of (extension-name-symbol events errors)

;(defmacro define-extension (name &key events errors)
;  ;; Define extension NAME with EVENTS and ERRORS.
;  ;; Note: The case of NAME is important.
;  ;; To define the request, Use:
;  ;;     (with-buffer-request (display (extension-opcode ,name)) ,@body)
;  ;;     See the REQUESTS file for lots of examples.
;  ;; To define event handlers, use declare-event.
;  ;; To define error handlers, use declare-error and define-condition.
;  (comment declare (type stringable name)
;	   (type list events errors))
;  (let ((name-symbol (kintern name)) ;; Intern name in the keyword package
;	(event-list (cl:mapcar #'canonicalize-event-name events)))
;    `(eval-when (compile load eval)
;       (setq *extensions* (cons (list ',name-symbol ',event-list ',errors)
;				(delete ',name-symbol *extensions* :key #'car))))))
(defmacro define-extension (name . keys)
  (lets ((events (memq ':events keys))
	 (errors (memq ':errors keys)))
    (and events (setq events (car events)))
    (and errors (setq errors (car errors)))
    (let ((name-symbol (kintern name))
	  (event-list (mapcar events (function canonicalize-event-name))))
    `(eval-when (compile load eval)
	(setq *extensions* (cons (list ',name-symbol ',event-list ',errors)
					 (delete ',name-symbol *extensions* :key #'car)))))))
	  
	  
(eval-when (compile eval load)
(defun canonicalize-event-name (event)
  ;; Returns the event name keyword given an event name stringable
  (comment declare (type stringable event))
  (comment declare (values event-key))
  (kintern event))
) ;; end eval-when

(eval-when (compile eval load)

(defun allocate-extension-event-code (name)
  ;; Allocate an event-code for an extension
  ;; This is executed at COMPILE and LOAD time from DECLARE-EVENT.
  ;; The event-code is used at compile-time by macros to index the following vectors:
  ;; *event-key-vector* *event-macro-vector* *event-handler-vector* *event-send-vector*
  (let ((event-code (get name 'event-code)))
    (comment declare (type (or null card8) event-code))
    (unless event-code
      ;; First ensure the name is for a declared extension
      (unless (catch 'found
		(dolist (extension *extensions*)
			(when (member name (second extension))
			  (throw 'found t))))
	(x-type-error name 'event-key))
      (setq event-code (position nil *event-key-vector*
				 :start *first-extension-event-code*))
      (setf (svref *event-key-vector* event-code) name)
      (setf (get name 'event-code) event-code))
    event-code))
) ;; end eval-when

(defun get-internal-event-code (display code)
  ;; Given an X11 event-code, return the internal event-code.
  ;; The internal event-code is used for indexing into the following vectors:
  ;; *event-key-vector* *event-handler-vector* *event-send-vector*
  ;; Returns NIL when the event-code is for an extension that isn't handled.
  (comment declare (type display display)
	   (type card8 code))
  (comment declare (values (or nil card8)))
  (setq code (logand #x7f code))
  (if (< code *first-extension-event-code*)
      code
    (let* ((code-offset (- code *first-extension-event-code*))
	   (event-extensions (display-event-extensions display))
	   (code (if (< code-offset (length event-extensions))
		     (aref event-extensions code-offset)
		   0)))
      (comment declare (type card8 code-offset code))
      (when (zerop code)
	(x-cerror "Ignore the event"
		  'unimplemented-event :event-code code :display display))
      code)))

(defun get-external-event-code (display event)
  ;; Given an X11 event name, return the event-code
  (comment declare (type display display)
	   (type event-key event))
  (comment declare (values card8))
  (let ((code (get-event-code event)))
    (comment declare (type (or null card8) code))
    (when (>= code *first-extension-event-code*)
      (setq code (+ *first-extension-event-code*
		    (or (position code (display-event-extensions display))
			(x-error 'undefined-event :display display :event-name event)))))
    code))

(defmacro extension-opcode (display name)
  ;; Returns the major opcode for extension NAME.
  ;; This is a macro to enable NAME to be interned for fast run-time
  ;; retrieval. 
  ;; Note: The case of NAME is important.
  (comment declare (type display display)
	   (type stringable name))
  (comment declare (values card8))
  (let ((name-symbol (kintern name))) ;; Intern name in the keyword package
    `(or (second (assoc ',name-symbol (display-extension-alist ,display)))
	 (x-error 'absent-extension :name ',name-symbol :display ,display))))

(defun initialize-extensions (display)
  ;; Initialize extensions for DISPLAY
  (let ((event-extensions (make-array 16 :element-type 'card8 :initial-element 0))
	(extension-alist nil))
    (dolist (extension *extensions*)
      (let ((name (first extension))
	    (events (second extension)))
	(multiple-value-bind (major-opcode first-event first-error)
	    (query-extension display name)
	  (when (and major-opcode (plusp major-opcode))
	    (push (list name major-opcode first-event first-error)
		  extension-alist)
	    (when (plusp first-event) ;; When there are extension events
	      ;; Grow extension vector when needed
	      (let ((max-event (- (+ first-event (length events))
				  *first-extension-event-code*)))
		(when (>= max-event (length event-extensions))
		  (let ((new-extensions (make-array (+ max-event 16) :element-type 'card8
						    :initial-element 0)))
		    ;;(replace new-extensions event-extensions)
		    (fill-vector new-extensions event-extensions)
		    (setq event-extensions new-extensions))))
	      (dolist (event events)
		(setf (aref event-extensions (- first-event *first-extension-event-code*))
		      (get-event-code event))
		(incf first-event)))))))
    (setf (display-event-extensions display) event-extensions)
    (setf (display-extension-alist display) extension-alist)))

;;
;; Reply handlers
;;

(defvar *pending-command-free-list* nil)

(defun start-pending-command (display)
  (declare (*pending-command-free-list*) special)
  (let ((pending-command (or (threaded-atomic-pop *pending-command-free-list*
						  pending-command-next pending-command)
			     (make-pending-command))))
    (setf (pending-command-reply-buffer pending-command) nil)
    ;;(setf (pending-command-process pending-command) (current-process))
    (setf (pending-command-process pending-command) nil)
    (setf (pending-command-sequence pending-command)
	  (logand 65535 (1+ (buffer-request-number display))))
    ;; Add the pending command to the end of the threaded list of pending
    ;; commands for the display.
    (with-event-queue-internal (display)
      (threaded-nconc pending-command (display-pending-commands display)
		      pending-command-next pending-command))
    pending-command))

(defun stop-pending-command (display pending-command)
  (with-event-queue-internal (display)
    ;; Remove the pending command from the threaded list of pending commands
    ;; for the display.
    (threaded-delete pending-command (display-pending-commands display)
		     pending-command-next pending-command)
    ;; Deallocate any reply buffers in this pending command
    (comment threaded-dolist (x (pending-command-reply-buffer pending-command)
			reply-next reply-buffer)
		     (pr x t))
    (loop
     (let ((reply-buffer
	    (threaded-pop (pending-command-reply-buffer pending-command)
			  reply-next reply-buffer)))
       (if reply-buffer
	   (deallocate-reply-buffer reply-buffer)
	 (exit nil)))))
  ;; Deallocate this pending-command
  (threaded-atomic-push pending-command *pending-command-free-list*
			pending-command-next pending-command)
  nil)

;;;

;(defvar *reply-buffer-free-lists* (make-array 32 :initial-element nil))
;; utilisp's fixnum is 28bit.
(defvar *reply-buffer-free-lists* (make-array 28 :initial-element nil))

(defun allocate-reply-buffer (size)
  (declare (*reply-buffer-free-lists*) special)
  (if (index<= size *replysize*)
      (allocate-event)
    (let ((index (integer-length (index1- size))))
      (or (threaded-pop (vref *reply-buffer-free-lists* index)
			reply-next reply-buffer)
	  (make-reply-buffer (logshift 1 index))))))

(defun deallocate-reply-buffer (reply-buffer)
  (declare (*reply-buffer-free-lists* *replysize*) special)
  (let ((size (reply-size reply-buffer)))
    (if (index<= size *replysize*)
	(deallocate-event reply-buffer)
      (let ((index (integer-length (index1- size))))
	(threaded-push reply-buffer (vref *reply-buffer-free-lists* index)
		       reply-next reply-buffer)))))
;;;

;(defun read-error-input (display sequence reply-buffer)
;  (comment declare (type display display)
;	   (type reply-buffer reply-buffer)
;	   (type card16 sequence))
;  (prog nil
;	start
;	(with-event-queue-internal (display)
;	  (let ((command 
;		 ;; Find any pending command with this sequence number.
;		 (threaded-dolist (pending-command (display-pending-commands display)
;						   pending-command-next pending-command)
;				  (when (= (pending-command-sequence pending-command) sequence)
;				    (exit pending-command)))))
;	    (comment declare (type (or null pending-command) command))
;	    (cond ((not (null command))
;		   ;; Give this reply to the pending command
;		   (threaded-nconc reply-buffer (pending-command-reply-buffer command)
;				   reply-next reply-buffer)
;		   (comment process-wakeup (pending-command-process command)))
;		  ((memq ':immediately (display-report-asynchronous-errors display))
;		   ;; No pending command and we should report the error immediately
;		   (go report-error))
;		  (t
;		   ;; No pending command found, count this as an asynchronous error
;		   (threaded-nconc reply-buffer (display-asynchronous-errors display)
;				   reply-next reply-buffer)))))
;	;;(return-from read-error-input nil)
;	(return nil)
;	report-error
;	;;(note-input-complete display token)
;	(apply #'report-error display
;	       (prog1 (make-error display reply-buffer t)
;		 (deallocate-event reply-buffer)))))

;;; utilisp
;(defun read-error-input (display sequence reply-buffer)
;  (prog nil
;	start
;	(with-event-queue-internal (display)
;	  (let ((command 
;		 ;; Find any pending command with this sequence number.
;		 (threaded-dolist (pending-command (display-pending-commands display)
;						   pending-command-next pending-command)
;				  (when (= (pending-command-sequence pending-command) sequence)
;				    (exit pending-command)))))
;	    (cond ((not (null command))
;		   ;; Give this reply to the pending command
;		   (threaded-nconc reply-buffer (pending-command-reply-buffer command)
;				   reply-next reply-buffer))
;		  ((memq ':immediately (display-report-asynchronous-errors display))
;		   ;; No pending command and we should report the error immediately
;		   (go report-error))
;		  (t
;		   ;; No pending command found, count this as an asynchronous error
;		   (threaded-nconc reply-buffer (display-asynchronous-errors display)
;				   reply-next reply-buffer)))))
;	;;(return-from read-error-input nil)
;	(return nil)
;	report-error
;	;;(note-input-complete display token)
;	(report-error display (prog1 (make-error display reply-buffer t)
;				(deallocate-event reply-buffer)))))

;; tuned up.
(defun read-error-input (display sequence reply-buffer)
  (prog nil
	start
	(let ((command 
	       ;; Find any pending command with this sequence number.
	       (threaded-dolist
		(pending-command (display-pending-commands display)
				 pending-command-next pending-command)
		(when (= (pending-command-sequence pending-command) sequence)
		  (exit pending-command)))))
	  (cond ((not (null command))
		 ;; Give this reply to the pending command
		 (threaded-nconc reply-buffer (pending-command-reply-buffer command)
				 reply-next reply-buffer))
		((memq ':immediately (display-report-asynchronous-errors display))
		 ;; No pending command and we should report the error immediately
		 (go report-error))
		(t
		 ;; No pending command found, count this as an asynchronous error
		 (threaded-nconc reply-buffer (display-asynchronous-errors display)
				 reply-next reply-buffer))))
	;;(return-from read-error-input nil)
	(return nil)
	report-error
	;;(note-input-complete display token)
	(report-error display (prog1 (make-error display reply-buffer t)
				(deallocate-event reply-buffer)))))

(defun read-reply-input (display sequence length reply-buffer)
  (prog nil
   (unwind-protect
       (progn
	(when (index< *replysize* length)
	  (let ((repbuf nil))
	    (unwind-protect
		(progn
		  (setq repbuf (allocate-reply-buffer length))
		  (buffer-replace (reply-ibuf8 repbuf) (reply-ibuf8 reply-buffer)
				  0 *replysize*)
		  (deallocate-event (prog1 reply-buffer (setq reply-buffer repbuf) (setq repbuf nil))))
	      (when repbuf
		(deallocate-reply-buffer repbuf))))
	  (when (buffer-input display (reply-ibuf8 reply-buffer) *replysize* length)
	    (return t))
	  (setf (reply-data-size reply-buffer) length))
	(with-event-queue-internal (display)
	  (let ((command
		 (threaded-dolist (pending-command (display-pending-commands display)
						   pending-command-next pending-command)
				  (when (= (pending-command-sequence pending-command) sequence)
				    (exit pending-command)))))
	    (comment declare (type (or null pending-command) command))
	    (when command
	      ;; Give this reply to the pending command
	      (threaded-nconc (prog1 reply-buffer (setq reply-buffer nil))
			    (pending-command-reply-buffer command)
			    reply-next reply-buffer)
	      (comment process-wakeup (pending-command-process command)))))
	nil)
     (when reply-buffer
       (deallocate-reply-buffer reply-buffer)))))

(defun read-event-input (display code reply-buffer)
  (comment declare (type display display)
	   (type card8 code)
	   (type reply-buffer reply-buffer))
  ;; Push the event in the input buffer on the display's event queue
  (setf (event-code reply-buffer)
	(get-internal-event-code display code))
  (enqueue-event reply-buffer display)
  nil)

;;; uti
;;; CLX to chigatte, predicate args ha hitotudake toiu kotoni site simatta.
;(defun read-input (display timeout force-output-p predicate predicate-arg)
;  (let ((reply-buffer nil))
;    (prog1
;	(prog nil
;	 loop
;	 (when (display-dead display)
;	   (x-error 'closed-display :display display))
;	 (when (funcall predicate predicate-arg)
;	   (return nil))
;	 ;; Check and see if we have to force output
;	 (when (and force-output-p
;		    (null (buffer-listen display)))
;	   (go force-output))
;	 ;; Now start gobbling.
;	 (setq reply-buffer (allocate-event))
;	 (with-buffer-input (reply-buffer :sizes 99 16 32)
;	   (let ((type 0))
;	     (unless (eq timeout 0)
;	       (let ((eof-p (buffer-input-wait display timeout)))
;		 (when eof-p (return eof-p))))
;	     (let ((eof-p (buffer-input display buffer-bbuf 0 *replysize*
;					(if force-output-p 0 timeout))))
;	       (when eof-p
;		 (when (eq eof-p ':timeout)
;		   (if force-output-p
;		       ;;(go force-output) ;; compiler ga okoru.
;		       nil
;		     (return ':timeout)))
;		 (setf (display-dead display) t)
;		 (return eof-p)))
;	     (setf (reply-data-size reply-buffer) *replysize*)
;	     (selectq (setq type (read-card8 0))
;	       (0 (read-error-input
;		   display
;		   (read-card16 2)
;		   (prog1 reply-buffer (setq reply-buffer nil))))
;	       (1 (let ((value
;			 (read-reply-input
;			  display
;			  (read-card16 2)
;			  (index+ *replysize* (index* (card32->big (read-card32 4)) 4))
;			  (prog1 reply-buffer (setq reply-buffer nil)))))
;		    ;;(when value (return value))
;		    (return value)
;		    ))
;	       (t (read-event-input
;		   display
;		   (read-card8 0)
;		   (prog1 reply-buffer (setq reply-buffer nil)))))))
;	 (go loop)
;
;	 force-output
;	 (display-force-output display)
;	 (setq force-output-p nil)
;	 (go loop))
;      (when (not (null reply-buffer))
;	(deallocate-reply-buffer reply-buffer))
;      )))
;;; read-input

;; prog wo tukawanai youni sita mono.
(defun read-input (display timeout force-output-p predicate predicate-arg)
  (let ((reply-buffer nil))
    (prog1
	 (loop
	  (when (display-dead display)
	    (x-error 'closed-display :display display))
	  (when (funcall predicate predicate-arg)
	    (exit nil))
	  ;; Check and see if we have to force output
	  (cond
	   ((and force-output-p (null (buffer-listen display)))
	    (display-force-output display)
	    (setq force-output-p nil))
	   (t 
	    ;; Now start gobbling.
	    (setq reply-buffer (allocate-event))
	    (with-buffer-input (reply-buffer :sizes 99 16 32)
	      (let ((type 0))
		(unless (eq timeout 0)
		  (let ((eof-p (buffer-input-wait display timeout)))
		    (when eof-p (exit eof-p))))
		(let ((eof-p (buffer-input display buffer-bbuf 0 *replysize*
					   (if force-output-p 0 timeout))))
		  (when eof-p
		    (when (eq eof-p ':timeout)
		      (if force-output-p
			  ;;(go force-output) ;; compiler ga okoru.
			  nil
			(exit ':timeout)))
		    (setf (display-dead display) t)
		    (exit eof-p)))
	     (setf (reply-data-size reply-buffer) *replysize*)
	     (selectq (setq type (read-card8 0))
	       (0 (read-error-input
		   display
		   (read-card16 2)
		   (prog1 reply-buffer (setq reply-buffer nil))))
	       (1 (let ((value
			 (read-reply-input
			  display
			  (read-card16 2)
			  (index+ *replysize* (index* (card32->big (read-card32 4)) 4))
			  (prog1 reply-buffer (setq reply-buffer nil)))))
		    ;;(when value (exit value))
		    (exit value)
		    ))
	       (t (read-event-input
		   display
		   (read-card8 0)
		   (prog1 reply-buffer (setq reply-buffer nil)))))))
	    )))
      (when (not (null reply-buffer))
	(deallocate-reply-buffer reply-buffer))
      )))

(defun report-asynchronous-errors (display mode)
  (when (and (display-asynchronous-errors display)
	     (member mode (display-report-asynchronous-errors display)))
    (let ((aborted t))
      (unwind-protect 
	  (loop
	    (let ((error
		    (with-event-queue-internal (display)
		      (threaded-pop (display-asynchronous-errors display)
				    reply-next reply-buffer))))
	      (comment declare (type (or null reply-buffer) error))
	      (if error
		  (cl:apply #'report-error display
			 (prog1 (make-error display error t)
				(deallocate-event error)))
		(return (setq aborted nil)))))
	;; If we get aborted out of this, deallocate all outstanding asynchronous
	;; errors.
	(when aborted 
	  (with-event-queue-internal (display)
	    (loop
	      (let ((reply-buffer
		      (threaded-pop (display-asynchronous-errors display)
				    reply-next reply-buffer)))
		(comment declare (type (or null reply-buffer) reply-buffer))
		(if reply-buffer
		    (deallocate-event reply-buffer)
		  (return nil))))))))))

;(defun wait-for-event (display timeout force-output-p)
;  (comment declare (type display display)
;	   (type (or null number) timeout)
;	   (type boolean force-output-p))
;  (let ((event-process-p (not (eq timeout 0))))
;    (comment declare (type boolean event-process-p))
;    (unwind-protect
;	(loop
;	  (comment when event-process-p
;	    (conditional-store (display-event-process display) nil (current-process)))
;	  (let ((eof (read-input
;		       display timeout force-output-p 
;		       #'(lambda (display)
;			   (comment declare (type display display))
;			   (or (not (null (display-new-events display)))
;			       (and (display-asynchronous-errors display)
;				    (member :before-event-handling
;					    (display-report-asynchronous-errors display))
;				    t)))
;		       display)))
;	    (when eof (exit eof)))
;	  ;; Report asynchronous errors here if the user wants us to.
;	  (when event-process-p
;	    (report-asynchronous-errors display :before-event-handling))
;	  (when (not (null (display-new-events display)))
;	    (return nil)))
;      (comment when (and event-process-p
;		 (eq (display-event-process display) (current-process)))
;	(setf (display-event-process display) nil)))))

;;; utilisp
(defun wait-for-event (display timeout force-output-p)
  (let ((event-process-p (not (eq timeout 0))))
    (unwind-protect
	(loop
	 (let ((eof (read-input
		     display timeout force-output-p
		     #'(lambda (display)
			 (or (not (null (display-new-events display)))
			     (and (display-asynchronous-errors display)
				  (memq ':before-event-handling
					  (display-report-asynchronous-errors display))
				  t)))
		     display)))
	   (when eof (exit eof)))
	 ;; Report asynchronous errors here if the user wants us to.
	 (when event-process-p
	   (report-asynchronous-errors display ':before-event-handling))
	 (when (not (null (display-new-events display)))
	   (exit nil))))))

;;; utilisp
(defun read-reply (display pending-command)
  (loop
   ;; timeout == nil, force-output-p == nil
   (when (read-input display nil nil
		     #'(lambda (p-c)
			 (not (null (pending-command-reply-buffer p-c))))
		     pending-command)
     (x-error 'closed-display :display display))
   (let ((reply-buffer
	  (threaded-pop (pending-command-reply-buffer pending-command)
			reply-next reply-buffer)))
     (with-buffer-input (reply-buffer)
       (selectq (read-card8 0)
	      ;; report-error ha CLX no mono to hikisuu no siyouga chigau.
	      (0 (report-error display
			       (prog1 (make-error display reply-buffer nil)
				 (deallocate-reply-buffer reply-buffer))))
	      (1 (exit reply-buffer))
	      (t (funcall 'err:argument-type 'read-reply)))))))
;;;

(defun event-listen (display (timeout 0))
  (comment declare (type display display)
	   (type (or null number) timeout)
	   (values number-of-events-queued eof-or-timeout))
  ;; Returns the number of events queued locally, if any, else nil.  Hangs
  ;; waiting for events, forever if timeout is nil, else for the specified
  ;; number of seconds.
  (let* ((current-event-symbol (car (display-current-event-symbol display)))
	 (current-event (and (boundp current-event-symbol)
			     (eval current-event-symbol)))
	 (queue (or current-event (display-event-queue-head display))))
    (comment declare (type symbol current-event-symbol)
	     (type (or null reply-buffer) current-event queue))
    (if queue
	(values
	  (with-event-queue-internal (display :timeout timeout)
	    (threaded-length (or current-event (display-event-queue-head display))
			     reply-next reply-buffer))
	  nil)
      (with-event-queue (display :timeout timeout :inline t)
	(let ((eof-or-timeout (wait-for-event display timeout nil)))
	  (if eof-or-timeout
	      (values nil eof-or-timeout)
	    (values 
	      (with-event-queue-internal (display :timeout timeout)
		(threaded-length (display-event-queue-head display)
				 reply-next reply-buffer))
	      nil)))))))

(defun queue-event (display event-key &rest args &key append-p send-event-p &allow-other-keys)
  ;; The event is put at the head of the queue if append-p is nil, else the tail.
  ;; Additional arguments depend on event-key, and are as specified above with
  ;; declare-event, except that both resource-ids and resource objects are accepted
  ;; in the event components.
  (comment declare (type display display)
	   (type event-key event-key)
	   (type boolean append-p send-event-p)
	   (dynamic-extent args))
  (unless (get event-key 'event-code)
    (x-type-error event-key 'event-key))
  (let* ((event (allocate-event))
	 (buffer (reply-ibuf8 event))
	 (event-code (get event-key 'event-code)))
    (comment declare (type reply-buffer event)
	     (type buffer-bytes buffer)
	     (type (or null card8) event-code))
    (unless event-code (x-type-error event-key 'event-key))
    (setf (event-code event) event-code)
    (with-display (display)
      (cl:apply (svref *event-send-vector* event-code) display args)
      (buffer-replace buffer
		      (display-obuf8 display)
		      0
		      *replysize*
		      (index+ 12 (buffer-boffset display)))
      ;;(setf (aref buffer 0) (if send-event-p (logior event-code #x80) event-code)
      ;;(aref buffer 2) 0
      ;;(aref buffer 3) 0)
      (sset buffer 0 (if send-event-p (logor event-code #x80) event-code))
      (sset buffer 2 0)
      (sset buffer 3 0)
      )
    (with-event-queue (display)
      (if append-p
	  (enqueue-event event display)
	(with-event-queue-internal (display)
	  (threaded-requeue event
			    (display-event-queue-head display)
			    (display-event-queue-tail display)
			    reply-next reply-buffer))))))

;;;; utilisp
(defun enqueue-event (new-event display)
  (comment declare (type reply-buffer new-event)
	   (type display display))
  ;; Place EVENT at the end of the event queue for DISPLAY
  (let* ((event-code (event-code new-event))
	 (event-key (and (index< event-code (vector-length *event-key-vector*))
			 (svref *event-key-vector* event-code))))
    (cond ((null event-key)
	   (unwind-protect
	    ;; ichijiteki.
	    ;;(cerror "Ignore this event" "No handler for ~s event" event-key)
	    (x-cerror "Ignore this event" "No handler for ~s event" event-key)
	    (deallocate-event new-event)))
	  (t 
	   (with-event-queue-internal (display)
	     (threaded-enqueue new-event
			       (display-event-queue-head display)
			       (display-event-queue-tail display)
			       reply-next reply-buffer)
	     (unless (display-new-events display)
	       (setf (display-new-events display) new-event)))))))

;(defmacro define-event (name code)
;  `(eval-when (eval compile load)
;     (setf (svref *event-key-vector* ,code) ',name)
;     (setf (get ',name 'event-code) ,code)))

(defmacro define-event (name code)
  `(progn
     (setf (svref *event-key-vector* ,code) ',name)
     (setf (get ',name 'event-code) ,code)))

;; Event names.  Used in "type" field in XEvent structures.  Not to be
;; confused with event masks above.  They start from 2 because 0 and 1
;; are reserved in the protocol for errors and replies. */

(define-event :key-press 2)
(define-event :key-release 3)
(define-event :button-press 4)
(define-event :button-release 5)
(define-event :motion-notify 6)
(define-event :enter-notify 7)
(define-event :leave-notify 8)
(define-event :focus-in 9)
(define-event :focus-out 10)
(define-event :keymap-notify 11)
(define-event :exposure 12)
(define-event :graphics-exposure 13)
(define-event :no-exposure 14)
(define-event :visibility-notify 15)
(define-event :create-notify 16)
(define-event :destroy-notify 17)
(define-event :unmap-notify 18)
(define-event :map-notify 19)
(define-event :map-request 20)
(define-event :reparent-notify 21)
(define-event :configure-notify 22)
(define-event :configure-request 23)
(define-event :gravity-notify 24)
(define-event :resize-request 25)
(define-event :circulate-notify 26)
(define-event :circulate-request 27)
(define-event :property-notify 28)
(define-event :selection-clear 29)
(define-event :selection-request 30)
(define-event :selection-notify 31)
(define-event :colormap-notify 32)
(define-event :client-message 33)
(define-event :mapping-notify 34)


(defmacro declare-event (event-codes . declares)
  ;; Used to indicate the keyword arguments for handler functions in
  ;; process-event and event-case.
  ;; Generates the functions used in SEND-EVENT.
  ;; A compiler warning is printed when all of EVENT-CODES are not
  ;; defined by a preceding DEFINE-EXTENSION.
  ;; The body is a list of declarations, each of which has the form:
  ;; (type . items)  Where type is a data-type, and items is a list of
  ;; symbol names.  The item order corresponds to the order of fields
  ;; in the event sent by the server.  An item may be a list of items.
  ;; In this case, each item is aliased to the same event field.
  ;; This is used to give all events an EVENT-WINDOW item.
  ;; See the INPUT file for lots of examples.
  (when (atom event-codes) (setq event-codes (list event-codes)))
  (setq event-codes (cl:mapcar #'canonicalize-event-name event-codes))
  (let* ((keywords nil)
	 (name (first event-codes))
	 (get-macro (xintern name '-event-get-macro))
	 (get-function (xintern name '-event-get))
	 (put-function (xintern name '-event-put)))
    (multiple-value-bind (get-code get-index get-sizes)
	(get-put-items
	  2 declares nil
	  #'(lambda (type index item args)
	      (let ((event-get #'(lambda (type index item args)
		       (unless (member type '(pad8 pad16))
			 `(,(kintern item)
			   (,(getify type) ,index ,@args))))))
		(if (atom item)
		    (funcall event-get type index item args)
		  (cl:mapcan #'(lambda (item)
			      (funcall event-get type index item args))
			  item)))))
      (multiple-value-bind (put-code put-index put-sizes)
	  (get-put-items
	    2 declares t
	    #'(lambda (type index item args)
		(unless (member type '(pad8 pad16))
		  (if (atom item)
		      (progn
			(push item keywords)
			`((,(putify type) ,index ,item ,@args)))
		    (let ((names (cl:mapcar #'(lambda (name) (kintern name))
					 item)))
		      (setq keywords (append item keywords))
		      `((,(putify type) ,index
			 (check-consistency ',names ,@item) ,@args)))))))
	`(within-definition (,name declare-event)
	   (defun ,get-macro (display event-key variable)
	     ;; Note: we take pains to macroexpand the get-code here to enable application
	     ;; code to be compiled without having the CLX macros file loaded.
	     (subst display '%buffer
		    (getf `(:display (the display ,display)
			    :event-key (the keyword ,event-key)
			    :event-code (the card8 (logand #x7f (read-card8 0)))
			    :send-event-p (the boolean (logbitp 7 (read-card8 0)))
			    ,@',(cl:mapcar #'macroexpand get-code))
			  variable)))

	   (defun ,get-function (display event handler)
	     (reading-event (event :display display :sizes (8 16 ,@get-sizes))
	       (funcall handler
			:display display
			:event-key (svref *event-key-vector* (event-code event))
			:event-code (logand #x7f (card8-get 0))
			:send-event-p (logbitp 7 (card8-get 0))
			,@get-code)))

	   (defun ,put-function (display &key ,@(setq keywords (nreverse keywords))
				 &allow-other-keys)
	     ,(when (member 'sequence keywords)
		`(unless sequence (setq sequence (display-request-number display))))
	     (with-buffer-output (display :sizes ,put-sizes
					  :index (index+ (buffer-boffset display) 12))
	       ,@put-code))
       
	   ,@(cl:mapcar #'(lambda (name)
			 (allocate-extension-event-code name)
			 `(let ((event-code (or (get ',name 'event-code)
						(allocate-extension-event-code ',name))))
			    (setf (svref *event-macro-vector* event-code)
				  (function ,get-macro))
			    (setf (svref *event-handler-vector* event-code)
				  (function ,get-function))
			    (setf (svref *event-send-vector* event-code)
				  (function ,put-function))))
		     event-codes)
	   ',name)))))

(defmacro check-consistency (names . args)
  `(_check-consistency ',names (list . ,args)))
(defun _check-consistency (names  args)
  ;; Ensure all args are nil or have the same value.
  ;; Returns the consistent non-nil value.
  (let ((value (car args)))
    (dolist (arg (cdr args))
      (if value
	  (when (and arg (not (eq arg value)))
	    (x-error 'inconsistent-parameters
		     :parameters (cl:mapcan #'list names args)))
	(setq value arg)))
    value))

;;;(macro declare-event (x) ''declare-event)

(declare-event (:key-press :key-release :button-press :button-release)
  ;; for key-press and key-release, code is the keycode
  ;; for button-press and button-release, code is the button number
  (data code)
  (card16 sequence)
  (card32 time)
  (window root (window event-window))
  ((or null window) child)
  (int16 root-x root-y x y)
  (card16 state)
  (boolean same-screen-p)
  )

(declare-event :motion-notify
  ((data boolean) hint-p)
  (card16 sequence)
  (card32 time)
  (window root (window event-window))
  ((or null window) child)
  (int16 root-x root-y x y)
  (card16 state)
  (boolean same-screen-p))

(declare-event (:enter-notify :leave-notify)
  ((data (member8 :ancestor :virtual :inferior :nonlinear :nonlinear-virtual)) kind)
  (card16 sequence)
  (card32 time)
  (window root (window event-window))
  ((or null window) child)
  (int16 root-x root-y x y)
  (card16 state)
  ((member8 :normal :grab :ungrab) mode)
  ((bit 0) focus-p)
  ((bit 1) same-screen-p))

(declare-event (:focus-in :focus-out)
  ((data (member8 :ancestor :virtual :inferior :nonlinear :nonlinear-virtual
		  :pointer :pointer-root :none))
   kind)
  (card16 sequence)
  (window (window event-window))
  ((member8 :normal :while-grabbed :grab :ungrab) mode))

(declare-event :keymap-notify
  ((bit-vector256 0) keymap))

(declare-event :exposure
  (card16 sequence)
  (window (window event-window))
  (card16 x y width height count))

(declare-event :graphics-exposure
  (card16 sequence)
  (drawable (drawable event-window))
  (card16 x y width height)
  (card16 minor)  ;; Minor opcode
  (card16 count)
  (card8 major))

(declare-event :no-exposure
  (card16 sequence)
  (drawable (drawable event-window))
  (card16 minor)
  (card8  major))

(declare-event :visibility-notify
  (card16 sequence)
  (window (window event-window))
  ((member8 :unobscured :partially-obscured :fully-obscured) state))

(declare-event :create-notify
  (card16 sequence)
  (window (parent event-window) window)
  (int16 x y)
  (card16 width height border-width)
  (boolean override-redirect-p))

(declare-event :destroy-notify
  (card16 sequence)
  (window event-window window))

(declare-event :unmap-notify
  (card16 sequence)
  (window event-window window)
  (boolean configure-p))

(declare-event :map-notify
  (card16 sequence)
  (window event-window window)
  (boolean override-redirect-p))

(declare-event :map-request
  (card16 sequence)
  (window (parent event-window) window))

(declare-event :reparent-notify
  (card16 sequence)
  (window event-window window parent)
  (int16 x y)
  (boolean override-redirect-p))

(declare-event :configure-notify
  (card16 sequence)
  (window event-window window)
  ((or null window) above-sibling)
  (int16 x y)
  (card16 width height border-width)
  (boolean override-redirect-p))

(declare-event :configure-request
  ((data (member :above :below :top-if :bottom-if :opposite)) stack-mode)
  (card16 sequence)
  (window (parent event-window) window)
  ((or null window) above-sibling)
  (int16 x y)
  (card16 width height border-width value-mask))

(declare-event :gravity-notify
  (card16 sequence)
  (window event-window window)
  (int16 x y))

(declare-event :resize-request
  (card16 sequence)
  (window (window event-window))
  (card16 width height))

(declare-event :circulate-notify
  (card16 sequence)
  (window event-window window parent)
  ((member16 :top :bottom) place))

(declare-event :circulate-request
  (card16 sequence)
  (window (parent event-window) window)
  (pad16 1 2)
  ((member16 :top :bottom) place))

(declare-event :property-notify
  (card16 sequence)
  (window (window event-window))
  (keyword atom) ;; keyword
  (card32 time)
  ((member16 :new-value :deleted) state))

(declare-event :selection-clear
  (card16 sequence)
  (card32 time)
  (window (window event-window)) 
  (keyword selection) ;; keyword
  )

(declare-event :selection-request
  (card16 sequence)
  (card32 time)
  (window (window event-window) requestor)
  (keyword selection target)
  ((or null keyword) property)
  )

(declare-event :selection-notify
  (card16 sequence)
  (card32 time)
  (window (window event-window))
  (keyword selection target)
  ((or null keyword) property)
  )

(declare-event :colormap-notify
  (card16 sequence)
  (window (window event-window))
  ((or null colormap) colormap)
  (boolean new-p installed-p))

(declare-event :client-message
  (data format)
  (card16 sequence)
  (window (window event-window))
  (keyword type)
  ((client-message-sequence format) data))

(declare-event :mapping-notify
  (card16 sequence)
  ((member8 :modifier :keyboard :pointer) request)
  (card8 start) ;; first key-code
  (card8 count))

;;
;; EVENT-LOOP
;;

(defun event-loop-setup (display)
  (comment declare (type display display)
	   (values progv-vars progv-vals
		   current-event-symbol current-event-discarded-p-symbol))
  (lets ((progv-vars (display-current-event-symbol display))
	 (current-event-symbol (first progv-vars))
	 (current-event-discarded-p-symbol (second progv-vars)))
    (comment declare (type list progv-vars)
	     (type symbol current-event-symbol current-event-discarded-p-symbol))
    (values
      progv-vars 
      (list (if (boundp current-event-symbol)
		;; The current event is already bound, so bind it to
		;; the next event.
		(let ((event (eval current-event-symbol)))
		  ;; If we have already discarded the current event, then we have
		  ;; already advanced the event pointer.
		  (if (eval current-event-discarded-p-symbol)
		      event
		    (and event (reply-next (the reply-buffer event)))))
	      ;; The current event isn't bound, so bind it to the head of the
	      ;; event queue.
	      (display-event-queue-head display))
	    nil)
      current-event-symbol
      current-event-discarded-p-symbol)))

;(defun event-loop-step-before (display timeout force-output-p current-event-symbol)
;  (prog nil
;	(unless (eval current-event-symbol)
;	  (let ((eof-or-timeout (wait-for-event display timeout force-output-p)))
;	    (when eof-or-timeout
;	      (return (values nil eof-or-timeout))))
;	  (set current-event-symbol (display-new-events display)))
;	(let ((event (eval current-event-symbol)))
;	  (comment declare (type reply-buffer event))
;	  (with-event-queue-internal (display)
;	    (when (eq event (display-new-events display))
;	      (setf (display-new-events display) (reply-next event))))
;	  (return (values event nil)))))

(defun event-loop-step-before (display timeout force-output-p current-event-symbol)
  (prog nil
	(unless (eval current-event-symbol)
	  (let ((eof-or-timeout (wait-for-event display timeout force-output-p)))
	    (when eof-or-timeout
	      (return (values nil eof-or-timeout))))
	  (set current-event-symbol (display-new-events display)))
	(let ((event (eval current-event-symbol)))
	  (with-event-queue-internal (display)
	    (when (eq event (display-new-events display))
	      (setf (display-new-events display) (reply-next event))))
	  (return (values event nil)))))

(defun event-loop-step-after
       (display event discard-p current-event-symbol current-event-discarded-p-symbol)
  (cond ((eval current-event-discarded-p-symbol)
	 ;; We have already done a discard, so we don't have to do
	 ;; anything.
	 )
	(discard-p
	 ;; We haven't discarded it yet, so discard it.
	 (discard-current-event display))
	(t
	 ;; Just go on to the next event.
	 (set current-event-symbol (reply-next event))))
  (set current-event-discarded-p-symbol nil))

;(defmacro event-loop ((display event timeout force-output-p discard-p) . body)
;  ;; Bind EVENT to the events for DISPLAY.
;  ;; This is the "GUTS" of process-event and event-case.
;  `(let ((%display% ,display)
;	 (%timeout% ,timeout)
;	 (%force-output-p% ,force-output-p)
;	 (%discard-p% ,discard-p))
;     (comment declare (type display %display%)
;	      (type (or null number) %timeout%)
;	      (type boolean %force-output-p% %discard-p%))
;     (with-event-queue (%display% . ,(and timeout '(:timeout %timeout%)))
;       (multiple-value-bind (%progv-vars% %progv-vals%
;			     %current-event-symbol% %current-event-discarded-p-symbol%)
;	   (event-loop-setup %display%)
;	 (progv %progv-vars% %progv-vals%
;	   (loop
;	     (multiple-value-bind (%event% %eof-or-timeout%)
;		 (event-loop-step-before
;		   %display% %timeout% %force-output-p%
;		   %current-event-symbol%)
;	       (when (null %event%) (exit (values nil %eof-or-timeout%)))
;	       ;;(print "eventloop: got event")(pr %event%)
;	       (let ((,event %event%))
;		 . ,body)
;	       (event-loop-step-after
;		 %display% %event% %discard-p%
;		 %current-event-symbol% %current-event-discarded-p-symbol%))))))))


(defmacro event-loop ((display event timeout force-output-p discard-p) . body)
  `(let ((%display% ,display)
	 (%timeout% ,timeout)
	 (%force-output-p% ,force-output-p)
	 (%discard-p% ,discard-p))
     (declare (**event** **discarded-p**) special)
     (let ((**event** (cond
		       ((boundp '**event**)
			(cond
			 (**discarded-p** **event**)
			 (t (and **event** (reply-next **event**)))))
		       (t (display-event-queue-head display))))
	   (**discarded-p** nil))
       (loop
	(lets ((%m% (event-loop-step-before %display%
					    %timeout%
					    %force-output-p%
					    '**event**))
	       (%event% (first %m%))
	       (%eof-or-timeout% (second %m%)))
	  (and (null %event%) (exit (values nil %eof-or-timeout%)))
	  (let ((,event %event%)) . ,body)
	  (cond (**discarded-p**)
		(%discard-p% (discard-current-event display))
		(t (setq **event** (reply-next %event%))))
	  (setq **discarded-p** nil))))))

;(defun discard-current-event (display)
;  ;; Discard the current event for DISPLAY.
;  ;; Returns NIL when the event queue is empty, else T.
;  ;; To ensure events aren't ignored, application code should only call
;  ;; this when throwing out of event-case or process-next-event, or from
;  ;; inside even-case, event-cond or process-event when :peek-p is T and
;  ;; :discard-p is NIL.
;  (comment declare (type display display))
;  (comment declare (values boolean))
;  (lets ((current-event-symbol (first (display-current-event-symbol display)))
;	 (current-event (and (boundp current-event-symbol)
;			     (eval current-event-symbol)))
;	 (current-event-discarded-p-symbol (second (display-current-event-symbol display))))
;    (when current-event
;      (with-event-queue-internal (display)
;	(let ((next (reply-next (the reply-buffer current-event))))
;	  (when (eq current-event (display-new-events display))
;	    (setf (display-new-events display) next))
;	  ;; Indicate we have done a discard.
;	  ;;(setf (symbol-value current-event-discarded-p-symbol) t)
;	  (set current-event-discarded-p-symbol t)
;	  (cond ((eq current-event (display-event-queue-head display))
;		 (threaded-dequeue (display-event-queue-head display)
;				   (display-event-queue-tail display)
;				   reply-next reply-buffer)
;		 (deallocate-event (prog1 (eval current-event-symbol)
;				     (set current-event-symbol next)))
;		 (not (null next)))
;		((null (display-event-queue-head display))
;		 ;; If there's no events in the event queue, then how can we
;		 ;; be discarding an event?  Don't deallocate the current
;		 ;; event, because for all we know it could be already
;		 ;; deallocated.  Do set the current event to nil, since the
;		 ;; event queue is empty.
;		 (set current-event-symbol nil)
;		 nil)
;		(t
;		 (do* ((previous (display-event-queue-head display) current)
;		       (current (reply-next previous) (reply-next previous)))
;		      ((or (null current) (eq current-event current))
;		       (when (eq current-event current)
;			 (when (eq current (display-event-queue-tail display))
;			   (setf (display-event-queue-tail display) previous))
;			 (setf (reply-next previous) next))
;		       (deallocate-event (prog1 (eval current-event-symbol)
;					   (set current-event-symbol next)))
;		       t)
;		      ))))))))

;;;
(defun discard-current-event (display)
  (lets ((current-event-symbol '**event**)
	 (current-event (and (boundp current-event-symbol)
			     (eval current-event-symbol)))
	 (current-event-discarded-p-symbol '**discarded-p**))
    (when current-event
      (with-event-queue-internal (display)
	(let ((next (reply-next (the reply-buffer current-event))))
	  (when (eq current-event (display-new-events display))
	    (setf (display-new-events display) next))
	  ;; Indicate we have done a discard.
	  ;;(setf (symbol-value current-event-discarded-p-symbol) t)
	  (set current-event-discarded-p-symbol t)
	  (cond ((eq current-event (display-event-queue-head display))
		 (threaded-dequeue (display-event-queue-head display)
				   (display-event-queue-tail display)
				   reply-next reply-buffer)
		 (deallocate-event (prog1 (eval current-event-symbol)
				     (set current-event-symbol next)))
		 (not (null next)))
		((null (display-event-queue-head display))
		 ;; If there's no events in the event queue, then how can we
		 ;; be discarding an event?  Don't deallocate the current
		 ;; event, because for all we know it could be already
		 ;; deallocated.  Do set the current event to nil, since the
		 ;; event queue is empty.
		 (set current-event-symbol nil)
		 nil)
		(t
		 (do* ((previous (display-event-queue-head display) current)
		       (current (reply-next previous) (reply-next previous)))
		      ((or (null current) (eq current-event current))
		       (when (eq current-event current)
			 (when (eq current (display-event-queue-tail display))
			   (setf (display-event-queue-tail display) previous))
			 (setf (reply-next previous) next))
		       (deallocate-event (prog1 (eval current-event-symbol)
					   (set current-event-symbol next)))
		       t)
		      ))))))))

;;
;; PROCESS-EVENT
;;
(defmacro process-event (display . keys)
  (lets ((handler (memq ':handler keys))
	 (timeout (memq ':timeout keys))
	 (peek-p (memq ':peek-p keys))
	 (discard-p (memq ':discard-p))
	 (force-output-p (memq ':force-output-p)))
    `(_process-event ,display
		     ,(and handler (second handler))
		     ,(and timeout (second timeout))
		     ,(and peek-p (second peek-p))
		     ,(and discard-p (second discard-p))
		     . ,(and force-output-p (ncons (second force-output-p))))))

;(defun process-event (display &key handler timeout peek-p discard-p (force-output-p t))
;  ;; If force-output-p is true, first invokes display-force-output.  Invokes handler
;  ;; on each queued event until handler returns non-nil, and that returned object is
;  ;; then returned by process-event.  If peek-p is true, then the event is not
;  ;; removed from the queue.  If discard-p is true, then events for which handler
;  ;; returns nil are removed from the queue, otherwise they are left in place.  Hangs
;  ;; until non-nil is generated for some event, or for the specified timeout (in
;  ;; seconds, if given); however, it is acceptable for an implementation to wait only
;  ;; once on network data, and therefore timeout prematurely.  Returns nil on
;  ;; timeout.  If handler is a sequence, it is expected to contain handler functions
;  ;; specific to each event class; the event code is used to index the sequence,
;  ;; fetching the appropriate handler.  Handler is called with raw resource-ids, not
;  ;; with resource objects.  The arguments to the handler are described using declare-event.
;  ;;
;  ;; T for peek-p means the event (for which the handler returns non-nil) is not removed
;  ;; from the queue (it is left in place), NIL means the event is removed.
;  
;  (event-loop (display event timeout force-output-p discard-p)
;    (lets ((event-code (event-code event)) ;; Event decoder defined by DECLARE-EVENT
;	   (event-decoder (and (index< event-code (vector-length *event-handler-vector*))
;			       (vref *event-handler-vector* event-code))))
;      (if event-decoder
;	  (let ((event-handler (if (functionp handler)
;				   handler
;				 (and (vectorp handler)
;				      (< event-code (vector-length handler))
;				      (vref handler event-code)))))
;	    (if event-handler
;		(let ((result (funcall event-decoder display event event-handler)))
;		  (when result
;		    (unless peek-p
;		      (discard-current-event display))
;		    (exit result)))
;	      (cerror "Ignore this event"
;		      "No handler for ~s event"
;		      (vref *event-key-vector* event-code))))
;	(cerror "Ignore this event"
;		"Server Error: event with unknown event code ~d received."
;		event-code)))))

(defun _process-event (display handler timeout peek-p discard-p (force-output-p t))
  (event-loop (display event timeout force-output-p discard-p)
    (lets ((event-code (event-code event)) ;; Event decoder defined by DECLARE-EVENT
	   (event-decoder (and (index< event-code (vector-length *event-handler-vector*))
			       (vref *event-handler-vector* event-code))))
      (if event-decoder
	  (let ((event-handler (if (functionp handler)
				   handler
				 (and (vectorp handler)
				      (< event-code (vector-length handler))
				      (vref handler event-code)))))
	    (if event-handler
		(let ((result (funcall event-decoder display event event-handler)))
		  (when result
		    (unless peek-p
		      (discard-current-event display))
		    (exit result)))
	      (cerror "Ignore this event"
		      "No handler for ~s event"
		      (vref *event-key-vector* event-code))))
	(cerror "Ignore this event"
		"Server Error: event with unknown event code ~d received."
		event-code)))))

;(defun make-event-handlers (&key (type 'array) default)
;  (comment declare (type t type)			;Sequence type specifier
;	   (type function default)
;	   (values sequence))			;Default handler for initial content
;  ;; Makes a handler sequence suitable for process-event
;  (make-sequence type *max-events* :initial-element default))

(defun make-event-handlers (&key (type 'vector) default)
  (make-sequence type *max-events* :initial-element default))

(defun event-handler (handlers event-key)
  (comment declare (type sequence handlers)
	   (type event-key event-key)
	   (values function))
  ;; Accessor for a handler sequence
  (elt handlers (position event-key *event-key-vector* :test #'eq)))

(defun set-event-handler (handlers event-key handler)
  (comment declare (type sequence handlers)
	   (type event-key event-key)
	   (type function handler)
	   (values handler))
  (setf (elt handlers (position event-key *event-key-vector* :test #'eq)) handler))

(defsetf event-handler set-event-handler)

;;
;; EVENT-CASE
;; 

(defmacro event-case (args . clauses)
  ;; If force-output-p is true, first invokes display-force-output.  Executes the
  ;; matching clause for each queued event until a clause returns non-nil, and that
  ;; returned object is then returned by event-case.  If peek-p is true, then the
  ;; event is not removed from the queue.  If discard-p is true, then events for
  ;; which the clause returns nil are removed from the queue, otherwise they are left
  ;; in place.  Hangs until non-nil is generated for some event, or for the specified
  ;; timeout (in seconds, if given); however, it is acceptable for an implementation
  ;; to wait only once on network data, and therefore timeout prematurely.  Returns
  ;; nil on timeout.  In each clause, event-or-events is an event-key or a list of
  ;; event-keys (but they need not be typed as keywords) or the symbol t or otherwise
  ;; (but only in the last clause).  The keys are not evaluated, and it is an error
  ;; for the same key to appear in more than one clause.  Args is the list of event
  ;; components of interest; corresponding values (if any) are bound to variables
  ;; with these names (i.e., the args are variable names, not keywords, the keywords
  ;; are derived from the variable names).  An arg can also be a (keyword var) form,
  ;; as for keyword args in a lambda lists.  If no t/otherwise clause appears, it is
  ;; equivalent to having one that returns nil.
  (or (null args) (consp args) (funcall 'err:argument-type args 'event-case))

  (comment declare (arglist (display &key timeout peek-p discard-p (force-output-p t))
		   (event-or-events ((&rest args) |...|) &body body) |...|))
  ;; Event-case is just event-cond with the whole body in the test-form

  `(event-cond ,args
	       . ,(mapcar
		   clauses
		   (function (lambda (clause)
			       `(,(car clause) ,(cadr clause) (progn . ,(cddr clause))))))))
;;
;; EVENT-COND
;; 

;; arguments: display &key timeout peek-p discard-p (force-output-p t)

(defmacro event-cond ((display . keys) . clauses)
  ;; The clauses of event-cond are of the form:
  ;; (event-or-events binding-list test-form . body-forms)
  ;;
  ;; EVENT-OR-EVENTS	event-key or a list of event-keys (but they
  ;;			need not be typed as keywords) or the symbol t
  ;;			or otherwise (but only in the last clause).  If
  ;;			no t/otherwise clause appears, it is equivalent
  ;;			to having one that returns nil.  The keys are
  ;;			not evaluated, and it is an error for the same
  ;;			key to appear in more than one clause.
  ;;
  ;; BINDING-LIST	The list of event components of interest.
  ;;			corresponding values (if any) are bound to
  ;;			variables with these names (i.e., the binding-list
  ;;			has variable names, not keywords, the keywords are
  ;;			derived from the variable names).  An arg can also
  ;;			be a (keyword var) form, as for keyword args in a
  ;;			lambda list.
  ;;
  ;; The matching TEST-FORM for each queued event is executed until a
  ;; clause's test-form returns non-nil.  Then the BODY-FORMS are
  ;; evaluated, returning the (possibly multiple) values of the last
  ;; form from event-cond.  If there are no body-forms then, if the
  ;; test-form is non-nil, the value of the test-form is returned as a
  ;; single value.
  ;;
  ;; Options:
  ;; FORCE-OUTPUT-P	When true, first invoke display-force-output if no
  ;;		  	input is pending.
  ;;
  ;; PEEK-P		When true, then the event is not removed from the queue.
  ;;
  ;; DISCARD-P		When true, then events for which the clause returns nil
  ;; 			are removed from the queue, otherwise they are left in place.
  ;;
  ;; TIMEOUT		If NIL, hang until non-nil is generated for some event's
  ;;			test-form. Otherwise return NIL after TIMEOUT seconds have
  ;;			elapsed.
  ;;
  (lets ((x (gensym)))
    (setplist x keys)
    (lets ((timeout (get x ':timeout))	;binding key arguments.
	   (peek-p (get x ':peek-p))
	   (discard-p (get x ':discard-p))
	   (force-output-p (or (get x ':force-output-p) t))
	   
	   (event (gensym))
	   (disp (gensym))
	   (peek (gensym)))
      `(let ((,disp ,display)
	     (,peek ,peek-p))
	 (event-loop (,disp ,event ,timeout ,force-output-p ,discard-p)
		     (event-dispatch (,disp ,event ,peek) . ,clauses))))))

(defun get-event-code (event)
  ;; Returns the event code given an event-key
  (or (get event 'event-code)
      (x-type-error event 'event-key)))

(defun universal-event-get-macro (display event-key variable)
  (getf
   `(:display ,display :event-key ,event-key :event-code
	      (logand 127 (read-card8 0)) :send-event-p
	      (logbitp 7 (read-card8 0)))
   variable))

;(defmacro event-dispatch ((display event peek-p) . clauses)
;  ;; Helper macro for event-case
;  ;; CLAUSES are of the form:
;  ;; (event-or-events binding-list test-form . body-forms)
;  (let ((event-key (gensym))
;	;;(all-events (make-array *max-events* :element-type 'bit :initial-element 0))
;	(all-events (vector *max-events* 0)))
;    `(reading-event (,event)
;       (let ((,event-key (svref *event-key-vector* (event-code ,event))))
;	 (case ,event-key
;	   . ,(mapcar
;	       clauses
;	       #'(lambda (clause)		; Translate event-cond clause to case clause
;		   (lets ((events (first clause))
;			  (arglist (second clause))
;			  (test-form (third clause))
;			  (body-forms (cdddr clause)))
;		     (let ((event-clause
;			    #'(lambda (display peek-p first-form rest-of-forms)
;				(if rest-of-forms
;				    `(when ,first-form
;				       (unless ,peek-p (discard-current-event ,display))
;				       (exit (progn . ,rest-of-forms)))
;				  ;; No body forms, return the result of the test form
;				  (let ((result (gensym)))
;				    `(let ((,result ,first-form))
;				       (when ,result
;					 (unless ,peek-p (discard-current-event ,display))
;					 (exit ,result))))))))
;		       
;		       (if (memq events '(otherwise t))
;			   ;; code for OTHERWISE clause.
;			   ;; Find all events NOT used by other clauses
;			   (let ((keys (do ((i 0 (1+ i))
;					    (key nil)
;					    (result nil))
;					   ((>= i *max-events*) result)
;					   (setq key (svref *event-key-vector* i))
;					   (when (and key (zerop (aref all-events i)))
;					     (push key result)))))
;			     `(otherwise
;			       (binding-event-values
;				(,display ,event-key ,(or keys :universal) . ,arglist)
;				,(funcall event-clause display peek-p test-form body-forms))))
;			 
;			 ;; Code for normal clauses
;			 (let ((true-events nil)) ;; canonicalize event-names
;			   (if (consp events)
;			       (progn
;				 (setq true-events (mapcar evnets #'canonicalize-event-name))
;				 (dolist (event true-events)
;				   (setf (aref all-events (get-event-code event)) 1)))
;			     (setf true-events (canonicalize-event-name events)
;				   (aref all-events (get-event-code true-events)) 1))
;			   `(,true-events
;			     (binding-event-values
;			      (,display ,event-key ,true-events . ,arglist)
;			      ,(funcall event-clause display peek-p test-form body-forms)))))
;		       )))
;	       ))))
;    ))

;; tuned for utilisp (there's yet to be done).
(defmacro event-dispatch ((display event peek-p) . clauses)
  ;; Helper macro for event-case
  ;; CLAUSES are of the form:
  ;; (event-or-events binding-list test-form . body-forms)
  (let ((event-key (gensym))
	;;(all-events (make-array *max-events* :element-type 'bit :initial-element 0))
	(all-events (vector *max-events* 0)))
    `(reading-event (,event)
       (let ((,event-key (vref *event-key-vector* (event-code ,event))))
	 (selectq ,event-key
	   . ,(mapcar
	       clauses
	       #'(lambda (clause)		; Translate event-cond clause to case clause
		   (lets ((events (first clause))
			  (arglist (second clause))
			  (test-form (third clause))
			  (body-forms (cdddr clause)))
		     (let ((event-clause
			    #'(lambda (display peek-p first-form rest-of-forms)
				(cond (rest-of-forms
				       `(when ,first-form
					  (or ,peek-p (discard-current-event ,display))
					  (exit (progn . ,rest-of-forms))))
				      (t
				       ;; No body forms, return the result of the test form
				       (let ((result (gensym)))
					 `(let ((,result ,first-form))
					    (cond (,result
						   (or ,peek-p (discard-current-event ,display))
						   (exit ,result)))
						  )))))))
		       (if (memq events '(otherwise t))
			   ;; code for OTHERWISE clause.
			   ;; Find all events NOT used by other clauses
			   (let ((keys (do ((i 0 (1+ i))
					    (key nil)
					    (result nil))
					   ((>= i *max-events*) result)
					   (setq key (svref *event-key-vector* i))
					   (and key (zerop (vref all-events i))
						(push key result)))))
			     `(t
			       (binding-event-values
				(,display ,event-key ,(or keys ':universal) . ,arglist)
				,(funcall event-clause display peek-p test-form body-forms))))
			 
			 ;; Code for normal clauses
			 (let ((true-events nil)) ;; canonicalize event-names
			   (cond ((consp events)
				  (setq true-events (mapcar events #'canonicalize-event-name))
				  (dolist (event true-events)
					  (setf (aref all-events (get-event-code event)) 1)))
				 (t
				  (setf true-events (canonicalize-event-name events)
					(vref all-events (get-event-code true-events)) 1)))
			   `(,true-events
			     (binding-event-values
			      (,display ,event-key ,true-events . ,arglist)
			      ,(funcall event-clause display peek-p test-form body-forms)))))
		       )))
	       ))))
    ))

;(defmacro binding-event-values ((display event-key event-keys . value-list) . body)
;  ;; Execute BODY with the variables in VALUE-LIST bound to components of the
;  ;; EVENT-KEYS events.
;  (unless (consp event-keys) (setq event-keys (list event-keys)))
;
;  (let ((var-key #'(lambda (var) (kintern (if (consp var) (first var) var))))
;	(var-symbol #'(lambda (var) (if (consp var) (second var) var))))
;    ;; VARS is an alist of:
;    ;;  (component-key ((event-key event-key ...) . extraction-code)
;    ;;		       ((event-key event-key ...) . extraction-code) ...)
;    ;; There should probably be accessor macros for this, instead of things like cdadr.
;    (let ((vars (mapcar value-list #'(lambda (var) (list var))))
;	  (multiple-p nil))
;      ;; Fill in the VARS alist with event-keys and extraction-code
;      (do ((keys event-keys (cdr keys))
;	   (temp nil))
;	  ((atom keys))
;	(lets ((key (car keys))
;	       (binder (selectq key
;			 (:universal #'universal-event-get-macro)
;			 (t (svref *event-macro-vector* (get-event-code key))))))
;	  (dolist (var vars)
;	    (let ((code (funcall binder display event-key (funcall var-key (car var)))))
;	      (comment unless code (warn "~a isn't a component of the ~s event"
;				 (funcall var-key (car var)) key))
;	      (unless code (format "/s isn't a component of the /s event/n"
;				   (funcall var-key (car var)) key))
;	      (if (setq temp (cl:member code (cdr var) :key #'cdr :test #'equal))
;		  (setl (caar temp) (cons key (caar temp)))
;		(setl (cdr var) (cons `((,key) . ,code) (cdr var))))))))
;      ;; Bind all the values
;      `(let ,(mapcar vars
;		     #'(lambda (var)
;			 (if (cddr var);; if more than one binding form
;			     (progn (setq multiple-p t)
;				    (funcall var-symbol (car var)))
;			   (list (funcall var-symbol (car var)) (cdadr var)))))
;	 ;; When some values come from different places, generate code to set them
;	 ,(when multiple-p
;	    `(case ,event-key
;	       . ,(do ((keys event-keys (cdr keys))
;		      (clauses nil) ;; alist of (event-keys bindings)
;		      (clause nil nil)
;		      (temp))
;		     ((atom keys)
;		      (dolist (clause clauses)
;			(unless (cdar clause) ;; Atomize single element lists
;			  (setf (car clause) (caar clause))))
;		      clauses)
;		   ;; Gather up all the bindings associated with (car keys)
;		   (dolist (var vars)
;		     (when (cddr var) ;; when more than one binding form
;		       (dolist (events (cdr var))
;			 (when (member (car keys) (car events))
;			   ;; Optimize for event-window being the same as some other binding
;			   (if (setq temp (cl:member (cdr events) clause
;						  :key #'caddr
;						  :test #'equal))
;			       (setq clause
;				     (nconc clause `((setq ,(car var) ,(second (car temp))))))
;			     (push `(setq ,(car var) ,(cdr events)) clause))))))
;		   ;; Merge bindings for (car keys) with other bindings
;		   (when clause
;		     (if (setq temp (cl:member clause clauses :key #'cdr :test #'equal))
;			 (setl (caar temp) (cons (car keys) (caar temp)))
;		       (push `((,(car keys)) . ,clause) clauses))))))
;	 . ,body))))

;; tuned up for utilisp
(defmacro binding-event-values ((display event-key event-keys . value-list) . body)
  (or (consp event-keys) (setq event-keys (list event-keys)))

  (let ((var-key #'(lambda (var) (kintern (if (consp var) (first var) var))))
	(var-symbol #'(lambda (var) (if (consp var) (second var) var))))
    (let ((vars (mapcar value-list #'(lambda (var) (list var))))
	  (multiple-p nil))
      (do ((keys event-keys (cdr keys))
	   (temp nil))
	  ((atom keys))
	(lets ((key (car keys))
	       (binder (selectq key
			 (:universal #'universal-event-get-macro)
			 (t (vref *event-macro-vector* (get-event-code key))))))
	  (do ((v vars (cdr v)))
	      ((atom v))
	      (lets ((var (car v))
		     (code (funcall binder display event-key (funcall var-key (car var)))))
		(or code (format "/c isn't a component of the /c event/n"
				 (funcall var-key (car var)) key))
		(cond ((setq temp (mem (function (lambda (k v) (equal k (cdr v))))
				       code (cdr var)))
		       (rplaca (car temp) (cons key (caar temp))))
		      (t (rplacd var (cons `((,key) . ,code) (cdr var)))))))))
      ;; Bind all the values
      `(let ,(mapcar vars #'(lambda (var)
			      (cond ((cddr var)
				     (setq multiple-p t)
				     (funcall var-symbol (car var)))
				    (t (list (funcall var-symbol (car var))
					     (cdadr var))))))
	 ;; When some values come from different places, generate code to set them
	 ,(and multiple-p
	       `(case ,event-key
		  . ,(do ((keys event-keys (cdr keys))
			  (clauses nil);; alist of (event-keys bindings)
			  (clause nil nil)
			  (temp))
			 ((atom keys)
			  (do ((c clauses (cdr c)))
			      ((atom c))
			      (let ((clause (car c)))
				(or (cdar clause)
				    (rplaca clause (caar clause)))))
			  clauses)
			 ;; Gather up all the bindings associated with (car keys)
			 (do ((v vars (cdr v)))
			     ((atom v))
			     (and
			      (cddar v)
			      (do ((events (cdar v) (cdr events)))
				  ((atom events))
				  (and
				   (member (car keys) (caar events))
				   (cond ((setq temp
						(mem #'(lambda (k v) (equal k (caddr v)))
						     (cdar events) clause))
					  (setq clause
						(nconc clause
						       `((setq ,(caar v)
							       ,(second (car temp)))))))
					 (t (push `(setq ,(caar v) ,(cdar events))
						  clause)))))))
			 ;; Merge bindings for (car keys) with other bindings
			 (and clause
			      (cond ((setq temp (mem #'(lambda (k v) (equal (cdr v) k))
						     clause clauses))
				     (rplaca (car temp) (cons (car keys) (caar temp))))
				    (t (push `((,(car keys)) . ,clause) clauses)))))))
	 . ,body))))

;;;-----------------------------------------------------------------------------
;;; Error Handling
;;;-----------------------------------------------------------------------------

(eval-when (eval compile load)
(defparameter
  *xerror-vector*
  '#(unknown-error
     request-error				; 1  bad request code
     value-error				; 2  integer parameter out of range
     window-error				; 3  parameter not a Window
     pixmap-error				; 4  parameter not a Pixmap
     atom-error					; 5  parameter not an Atom
     cursor-error				; 6  parameter not a Cursor
     font-error					; 7  parameter not a Font
     match-error				; 8  parameter mismatch
     drawable-error				; 9  parameter not a Pixmap or Window
     access-error				; 10 attempt to access private resource"
     alloc-error				; 11 insufficient resources
     colormap-error				; 12 no such colormap
     gcontext-error				; 13 parameter not a GContext
     id-choice-error				; 14 invalid resource ID for this connection
     name-error					; 15 font or color name does not exist
     length-error				; 16 request length incorrect;
						;    internal Xlib error
     implementation-error			; 17 server is defective
     ))
)

;(defun make-error (display event asynchronous)
;  (comment declare (type display display)
;	   (type reply-buffer event)
;	   (type boolean asynchronous))
;  (reading-event (event)
;    (let* ((error-code (read-card8 1))
;	   (error-key (get-error-key display error-code))
;	   (error-decode-function (get error-key 'error-decode-function))
;	   (params (funcall error-decode-function display event)))
;      (list* error-code error-key
;	     :asynchronous asynchronous :current-sequence (display-request-number display)
;	     params))))

(defun make-error (display event asynchronous)
  (reading-event (event)
    (let* ((error-code (read-card8 1))
	   (error-key (get-error-key display error-code))
	   (error-decode-function (get error-key 'error-decode-function))
	   (params (funcall error-decode-function display event)))
      (append
       (list error-code error-key
	     :asynchronous asynchronous :current-sequence (display-request-number display))
       params))))

;(defun report-error (display error-code error-key &rest params)
;  (comment declare (type display display)
;	   (dynamic-extent params))
;  ;; All errors (synchronous and asynchronous) are processed by calling
;  ;; an error handler in the display.  The handler is called with the display
;  ;; as the first argument and the error-key as its second argument. If handler is
;  ;; an array it is expected to contain handler functions specific to
;  ;; each error; the error code is used to index the array, fetching the
;  ;; appropriate handler. Any results returned by the handler are ignored;;
;  ;; it is assumed the handler either takes care of the error completely,
;  ;; or else signals. For all core errors, additional keyword/value argument
;  ;; pairs are:
;  ;;    :major integer
;  ;;    :minor integer
;  ;;    :sequence integer
;  ;;    :current-sequence integer
;  ;;    :asynchronous (member t nil)
;  ;; For :colormap, :cursor, :drawable, :font, :GContext, :id-choice, :pixmap, and :window
;  ;; errors another pair is:
;  ;;    :resource-id integer
;  ;; For :atom errors, another pair is:
;  ;;    :atom-id integer
;  ;; For :value errors, another pair is:
;  ;;    :value integer
;  (let* ((handler (display-error-handler display))
;	 (handler-function
;	   (if (type? handler 'sequence)
;	       (elt handler error-code)
;	     handler)))
;    (apply handler-function display error-key params)))

;;; utilisp you ni siyou wo henkou sita.
;;; params ha (error-code error-key . key-vals)
(defun report-error (display params)
  ;; All errors (synchronous and asynchronous) are processed by calling
  ;; an error handler in the display.  The handler is called with the display
  ;; as the first argument and the error-key as its second argument. If handler is
  ;; an array it is expected to contain handler functions specific to
  ;; each error; the error code is used to index the array, fetching the
  ;; appropriate handler. Any results returned by the handler are ignored;;
  ;; it is assumed the handler either takes care of the error completely,
  ;; or else signals. For all core errors, additional keyword/value argument
  ;; pairs are:
  ;;    :major integer
  ;;    :minor integer
  ;;    :sequence integer
  ;;    :current-sequence integer
  ;;    :asynchronous (member t nil)
  ;; For :colormap, :cursor, :drawable, :font, :GContext, :id-choice, :pixmap, and :window
  ;; errors another pair is:
  ;;    :resource-id integer
  ;; For :atom errors, another pair is:
  ;;    :atom-id integer
  ;; For :value errors, another pair is:
  ;;    :value integer
  (lets ((error-code (first params))
	 (handler (display-error-handler display))
	 (handler-function
	  (cond ((vectorp handler) (vref handler error-code))
		((consp handler) (nth error-code handler))
		(t handler))))
    (funcall handler-function
	     display (second params) (cddr params))))

;;; handler function ha saigono hikisuu ga parameter no haitta list to kangaeru
;;; koto.

;;; dolist ha exit dekinaito ikenai.
(defun request-name (code (display))
  (if (< code (vector-length *request-names*))
      (svref *request-names* code)
    (dolist (extension (and display (display-extension-alist display)) "unknown")
      (when (= code (second extension))
	(exit (first extension))))))

;;; NOTE::::: ato de naosu koto !!!!
;;;
;;;
;;;
"define-condition not implemented yet!!"
(macro define-condition (x) ''define-condition)

;#-clx-cl-error
(define-condition request-error (x-error)
  (display
   error-key
   major
   minor
   sequence
   current-sequence
   asynchronous)
  (:report report-request-error))

(defun report-request-error (condition stream)
  (let ((error-key (request-error-error-key condition))
	(asynchronous (request-error-asynchronous condition))
	(major (request-error-major condition))
	(minor (request-error-minor condition))
	(sequence (request-error-sequence condition))
	(current-sequence (request-error-current-sequence condition)))		   
    (format stream "~:[~;Asynchronous ~]~a in ~:[request ~d (last request was ~d) ~;current request~2* ~] Code ~d.~d [~a]"
	    asynchronous error-key (= sequence current-sequence)
	    sequence current-sequence major minor
	    (request-name major (request-error-display condition)))))

;; Since the :report arg is evaluated as (function report-request-error) the
;; define-condition must come after the function definition.
;#+clx-cl-error
;(define-condition request-error (x-error)
;  (display
;   error-key
;   major
;   minor
;   sequence
;   current-sequence
;   asynchronous)
;  (:report report-request-error))


(define-condition resource-error (request-error)
  (resource-id)
  (:report (lambda (condition stream)
	     (report-request-error condition stream)
	     (format stream " ID #x~x" (resource-error-resource-id condition)))))  

(define-condition unknown-error (request-error)
  (error-code)
  (:report (lambda (condition stream)
	     (report-request-error condition stream)
	     (format stream " Error Code ~d." (unknown-error-error-code condition)))))

(define-condition access-error (request-error))

(define-condition alloc-error (request-error))

(define-condition atom-error (request-error)
  (atom-id)
  (:report (lambda (condition stream)
	     (report-request-error condition stream)
	     (format stream " Atom-ID #x~x" (atom-error-atom-id condition)))))

(define-condition colormap-error (resource-error))

(define-condition cursor-error (resource-error))

(define-condition drawable-error (resource-error))

(define-condition font-error (resource-error))

(define-condition gcontext-error (resource-error))

(define-condition id-choice-error (resource-error))

(define-condition illegal-request-error (request-error))

(define-condition length-error (request-error))

(define-condition match-error (request-error))

(define-condition name-error (request-error))

(define-condition pixmap-error (resource-error))

(define-condition value-error (request-error)
  (value)
  (:report (lambda (condition stream)
	     (report-request-error condition stream)
	     (format stream " Value ~d." (value-error-value condition)))))

(define-condition window-error (resource-error))

(define-condition implementation-error (request-error))

;;-----------------------------------------------------------------------------
;; Internal error conditions signaled by CLX

(define-condition type-error (x-error)
  (object
   type
   type-string)
  (:report (lambda (condition stream)
	     (format stream "~s isn't ~@[a ~] ~s"
		     (type-error-object condition)
		     (type-error-type-string condition)
		     (type-error-type condition)))))

(define-condition closed-display (x-error)
  (display)
  (:report (lambda (condition stream)
	     (format stream "Attempt to use closed display ~s"
		     (closed-display-display condition)))))

(define-condition lookup-error (x-error)
  (id display type object)
  (:report (lambda (condition stream)
	     (format stream "ID ~d from display ~s should have been a ~s, but was ~s"
		     (lookup-error-id condition)
		     (lookup-error-display condition)
		     (lookup-error-type condition)
		     (lookup-error-object condition)))))  

(define-condition connection-failure (x-error)
  (major-version
   minor-version
   host
   display
   reason)
  (:report (lambda (condition stream)
	     (format stream "Connection failure to X~d.~d server ~a display ~d: ~a"
		     (connection-failure-major-version condition)
		     (connection-failure-minor-version condition)
		     (connection-failure-host condition)
		     (connection-failure-display condition)
		     (connection-failure-reason condition)))))
  
(define-condition reply-length-error (x-error)
  (reply-length
   expected-length
   display)
  (:report (lambda (condition stream)
	     (format stream "Reply length was ~d when ~d words were expected for display ~s"
		     (reply-length-error-reply-length condition)
		     (reply-length-error-expected-length condition)
		     (reply-length-error-display condition)))))  

(define-condition reply-timeout (x-error)
  (timeout
   display)
  (:report (lambda (condition stream)
	     (format stream "Timeout after waiting ~d seconds for a reply for display ~s"
		     (reply-timeout-timeout condition)
		     (reply-timeout-display condition)))))  

(define-condition sequence-error (x-error)
  (display
   req-sequence
   msg-sequence)
  (:report (lambda (condition stream)
	     (format stream "Reply out of sequence for display ~s.~%  Expected ~d, Got ~d"
		     (sequence-error-display condition)
		     (sequence-error-req-sequence condition)
		     (sequence-error-msg-sequence condition)))))  

(define-condition unexpected-reply (x-error)
  (display
   msg-sequence
   req-sequence
   length)
  (:report (lambda (condition stream)
	     (format stream "Display ~s received a server reply when none was expected.~@
		             Last request sequence ~d Reply Sequence ~d Reply Length ~d bytes."
		  (unexpected-reply-display condition)
		  (unexpected-reply-req-sequence condition)
		  (unexpected-reply-msg-sequence condition)
		  (unexpected-reply-length condition)))))

(define-condition missing-parameter (x-error)
  (parameter)
  (:report (lambda (condition stream)
	     (let ((parm (missing-parameter-parameter condition)))
	       (if (consp parm)
		   (format stream "One or more of the required parameters ~a is missing."
			   parm)
		 (format stream "Required parameter ~a is missing or null." parm))))))

;; This can be signalled anywhere a pseudo font access fails.
(define-condition invalid-font (x-error)
  (font)
  (:report (lambda (condition stream)
	     (format stream "Can't access font ~s" (invalid-font-font condition)))))

(define-condition device-busy (x-error)
  (display)
  (:report (lambda (condition stream)
	     (format stream "Device busy for display ~s"
		     (device-busy-display condition)))))

(define-condition unimplemented-event (x-error)
  (display
   event-code)
  (:report (lambda (condition stream)
	     (format stream "Event code ~d not implemented for display ~s"
		     (unimplemented-event-event-code condition)
		     (unimplemented-event-display condition)))))

(define-condition undefined-event (x-error)
  (display
   event-name)
  (:report (lambda (condition stream)
	     (format stream "Event code ~d undefined for display ~s"
		     (undefined-event-event-name condition)
		     (undefined-event-display condition)))))

(define-condition absent-extension (x-error)
  (name display)
  (:report (lambda (condition stream)
	     (format stream "Extension ~a isn't defined for display ~s"
		     (absent-extension-name condition)
		     (absent-extension-display condition)))))

(define-condition inconsistent-parameters (x-error)
  (parameters)
  (:report (lambda (condition stream)
	     (format stream "inconsistent-parameters:~{ ~s~}"
		     (inconsistent-parameters-parameters condition)))))

(defun get-error-key (display error-code)
  (comment declare (type display display)
	   (type array-index error-code))
  ;; Return the error-key associated with error-code
  (if (< error-code (vector-length *xerror-vector*))
      (svref *xerror-vector* error-code)
    ;; Search the extensions for the error
    (dolist (entry (display-extension-alist display) 'unknown-error)
      (let* ((event-name (first entry))
	     (first-error (fourth entry))
	     (errors (third (assoc event-name *extensions*))))
	(when (and errors
		   (index<= first-error error-code
			    (index+ first-error (index- (length errors) 1))))
	  (exit (nth (index- error-code first-error) errors)))))))

(defmacro define-error (error-key function)
  ;; Associate a function with ERROR-KEY which will be called with
  ;; parameters DISPLAY and REPLY-BUFFER and
  ;; returns a plist of keyword/value pairs which will be passed on
  ;; to the error handler.  A compiler warning is printed when
  ;; ERROR-KEY is not defined in a preceding DEFINE-EXTENSION.
  ;; Note: REPLY-BUFFER may used with the READING-EVENT and READ-type
  ;;       macros for getting error fields. See DECODE-CORE-ERROR for
  ;;       an example.
  (comment declare (type symbol error-key)
	   (type function function))
  ;; First ensure the name is for a declared extension
  (unless (or (find error-key *xerror-vector*)
	      (dolist (extension *extensions*)
		(when (member error-key (third extension))
		  (return t))))
    (x-type-error error-key 'error-key))
  `(setf (get ',error-key 'error-decode-function) (function ,function)))

;; All core errors use this, so we make it available to extensions.
;;; utilisp
(defun decode-core-error (display event (arg))
  ;; All core errors have the following keyword/argument pairs:
  ;;    :major integer
  ;;    :minor integer
  ;;    :sequence integer
  ;; In addition, many have an additional argument that comes from the
  ;; same place in the event, but is named differently.  When the ARG
  ;; argument is specified, the keyword ARG with card32 value starting
  ;; at byte 4 of the event is returned with the other keyword/argument
  ;; pairs.
  display
  (reading-event (event)
    (lets ((sequence (read-card16 2))
	   (minor-code (read-card16 8))
	   (major-code (read-card8 10))
	   (result (list :major major-code
			 :minor minor-code
			 :sequence sequence)))
      (when arg
	(setq result (append (list arg (read-card32 4)) result)))
      result)))

(defun decode-resource-error (display event)
  (decode-core-error display event :resource-id))

;(define-error unknown-error
;  (lambda (display event)
;    (list* :error-code (aref (reply-ibuf8 event) 1)
;	   (decode-core-error display event))))

(define-error unknown-error
  (lambda (display event)
    (append (list :error-code (sref (reply-ibuf8 event) 1)
		  (decode-core-error display event)))))

(define-error request-error decode-core-error)		; 1  bad request code

(define-error value-error				; 2  integer parameter out of range
  (lambda (display event)
    (decode-core-error display event :value)))

(define-error window-error decode-resource-error)	; 3  parameter not a Window

(define-error pixmap-error decode-resource-error)	; 4  parameter not a Pixmap

(define-error atom-error				; 5  parameter not an Atom
  (lambda (display event)
    (decode-core-error display event :atom-id)))

(define-error cursor-error decode-resource-error)	; 6  parameter not a Cursor

(define-error font-error decode-resource-error)		; 7  parameter not a Font

(define-error match-error decode-core-error)		; 8  parameter mismatch

(define-error drawable-error decode-resource-error)	; 9  parameter not a Pixmap or Window

(define-error access-error decode-core-error)		; 10 attempt to access private resource"

(define-error alloc-error decode-core-error)		; 11 insufficient resources

(define-error colormap-error decode-resource-error)	; 12 no such colormap

(define-error gcontext-error decode-resource-error)	; 13 parameter not a GContext

(define-error id-choice-error decode-resource-error)	; 14 invalid resource ID for this connection

(define-error name-error decode-core-error)		; 15 font or color name does not exist

(define-error length-error decode-core-error)		; 16 request length incorrect;
							;    internal Xlib error

(define-error implementation-error decode-core-error)	; 17 server is defective
