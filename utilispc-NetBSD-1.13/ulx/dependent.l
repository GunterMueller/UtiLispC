;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;; This file contains some of the system dependent code for CLX

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

;;; $Revision: 1.1.1.1 $
;;; $Log: dependent.l,v $
;;; Revision 1.1.1.1  2000/09/26 00:51:08  ktanaka
;;;
;;;
;;; Revision 1.1  1991/06/12  05:24:32  ken
;;; Initial revision
;;;
;;; This file is part of ULX (UtiLisp X library)
;;;

;;; Number of seconds to wait for a reply to a server request
(defparameter *reply-timeout* nil)

;;; utilisp
(defun card8->int8 (x)
  (cond ((0= (logand 128 x)) x)(t (- x 256))))

(defun int8->card8 (x)
  (logand 255 x))

(defun card16->int16 (x)
  (cond ((0= (logand 32768 x)) x)(t (- x 65536))))

(defun int16->card16 (x)
  (logand 65535 x))

;; 32bit integer/cardinal objects are kept in the form (16bit-value . 16bit-value)
(defun card32->int32 (x)
  (cond ((0= (logand 32768 (car x))) x)
	(t (cons (- (car x) 65536)(cdr x)))))

(defun int32->card32 (x)
  (cons (logand 65535 (car x))(logand 65535 (cdr x))))

(defun aref-card8 (a i)			;string ref. (buffer is a string obj.)
  (sref a i))

(defun aset-card8 (v a i)
  (sset a i v))

(defun aref-int8 (a i)
  (card8->int8 (sref a i)))

(defun aset-int8 (v a i)
  (sset a i (int8->card8 v)))

(defun aref-card16 (a i)
  (cutout a i 2))
;  (logior (logshift (sref a (index+ i *word-1*)) 8)
;	  (sref a (index+ i *word-0*)))

(defun aset-card16 (v a i)
  (sset a i (logand 255 (logshift v -8)))
  (sset a (index1+ i) (logand 255 v))
  v)

(defun aref-int16 (a i)
  (card16->int16 (cutout a i 2)))
;(logior (logshift (aref-int8 a (index+ i *word-1*)) 8)
;	  (aref a (index+ i *word-0*))))

(defun aset-int16 (v a i)
  (sset a i (logand 255 (logshift v -8)))
  (sset a (index1+ i) (logand 255 v))
  v)

;; again, 32bits objects are kept in the form (high-16bits . low-16bits)
(defun aref-card32 (a i)
  (cons (cutout a i 2) (cutout a (index+ i 2) 2)))

(defun aset-card32 (v a i)
  (cond ((fixp v)
	 (sset a i 0)
	 (sset a (index1+ i) (logshift v -16))
	 (sset a (index+ i 2) (logand 255 (logshift v -8)))
	 (sset a (index+ i 3) (logand 255 v)))
	((stringp v) (string-amend a v i))
	(t (sset a i (logand 255 (logshift (car v) -8)))
	   (sset a (index1+ i) (logand 255 (car v)))
	   (sset a (index+ i 2) (logand 255 (logshift (cdr v) -8)))
	   (sset a (index+ i 3) (logand 255 (cdr v)))))
  v)

(defun aref-int32 (a i)
  (card32->int32 (aref-card32 a i)))

(defun aset-int32 (v a i)
  (aset-card32 v a i)
  v)

(defun aref-card29 (a i)
  ;; 2047 = #b000111111111111
  (cons (logand 2047 (cutout a i 2))(cutout a (index+ i 2) 2)))

(defun aset-card29 (v a i)
  (aset-card32 v a i)
  v)

;;; Other random conversions

(defun rgb-val->card16 (value)
  ;; Short floats are good enough
;  (declare (type rgb-val value))
;  (declare (values card16))
;  #.(declare-buffun)
  ;; Convert VALUE from float to card16
;  (the card16 (values (round (the rgb-val value) #.(/ 1.0s0 #xffff))))
  (fix (+$ (*$ 65535.0 value) 0.5)))

(defun card16->rgb-val (value) 
  ;; Short floats are good enough
  ;; Convert VALUE from card16 to float
;  (the short-float (* (the card16 value) #.(/ 1.0s0 #xffff)))
  (//$ (float value) 65535.0))

(defun radians->int16 (value)
  (fix (+$ 0.5 (*$ 64.0 (float value)))))
  ;; Short floats are good enough
;  (the int16 (values (round (the angle value) #.(float (/ pi 180.0s0 64.0s0) 0.0s0)))))

(defun int16->radians (value)
  (//$ (float value) 64.0))
  ;; Short floats are good enough
;  (the short-float (* (the int16 value) #.(coerce (/ pi 180.0 64.0) 'short-float))))

;;-----------------------------------------------------------------------------
;; Character transformation
;;-----------------------------------------------------------------------------

;;; This stuff transforms chars to ascii codes in card8's and back.
;;; You might have to hack it a little to get it to work for your machine.

;eval-when (eval compile)
;(defparameter *char-to-ascii-alist*
;  '#.`(#-lispm
;       ;; The normal ascii codes for the control characters.
;       ,@`((#\Return . 13)
;	   (#\Linefeed . 10)
;	   (#\Rubout . 127)
;	   (#\Page . 12)
;	   (#\Tab . 9)
;	   (#\Backspace . 8)
;	   (#\Newline . 10)
;	   (#\Space . 32))
;       ;; One the lispm, #\Newline is #\Return, but we'd really like
;       ;; #\Newline to translate to ascii code 10, so we swap the
;       ;; Ascii codes for #\Return and #\Linefeed. We also provide
;       ;; mappings from the counterparts of these control characters
;       ;; so that the character mapping from the lisp machine
;       ;; character set to ascii is invertible.
;       #+lispm
;       ,@`((#\Return . 10)   (,(code-char  10) . ,(char-code #\Return))
;	   (#\Linefeed . 13) (,(code-char  13) . ,(char-code #\Linefeed))
;	   (#\Rubout . 127)  (,(code-char 127) . ,(char-code #\Rubout))
;	   (#\Page . 12)     (,(code-char  12) . ,(char-code #\Page))
;	   (#\Tab . 9)       (,(code-char   9) . ,(char-code #\Tab))
;	   (#\Backspace . 8) (,(code-char   8) . ,(char-code #\Backspace))
;	   (#\Newline . 10)  (,(code-char  10) . ,(char-code #\Newline))
;	   (#\Space . 32)    (,(code-char  32) . ,(char-code #\Space)))
;       ;; The rest of the common lisp charater set with the normal
;       ;; ascii codes for them.
;       (#\! . 33) (#\" . 34) (#\# . 35) (#\$ . 36)
;       (#\% . 37) (#\& . 38) (#\' . 39) (#\( . 40)
;       (#\) . 41) (#\* . 42) (#\+ . 43) (#\, . 44)
;       (#\- . 45) (#\. . 46) (#\/ . 47) (#\0 . 48)
;       (#\1 . 49) (#\2 . 50) (#\3 . 51) (#\4 . 52)
;       (#\5 . 53) (#\6 . 54) (#\7 . 55) (#\8 . 56)
;       (#\9 . 57) (#\: . 58) (#\; . 59) (#\< . 60)
;       (#\= . 61) (#\> . 62) (#\? . 63) (#\@ . 64)
;       (#\A . 65) (#\B . 66) (#\C . 67) (#\D . 68)
;       (#\E . 69) (#\F . 70) (#\G . 71) (#\H . 72)
;       (#\I . 73) (#\J . 74) (#\K . 75) (#\L . 76)
;       (#\M . 77) (#\N . 78) (#\O . 79) (#\P . 80)
;       (#\Q . 81) (#\R . 82) (#\S . 83) (#\T . 84)
;       (#\U . 85) (#\V . 86) (#\W . 87) (#\X . 88)
;       (#\Y . 89) (#\Z . 90) (#\[ . 91) (#\\ . 92)
;       (#\] . 93) (#\^ . 94) (#\_ . 95) (#\` . 96)
;       (#\a . 97) (#\b . 98) (#\c . 99) (#\d . 100)
;       (#\e . 101) (#\f . 102) (#\g . 103) (#\h . 104)
;       (#\i . 105) (#\j . 106) (#\k . 107) (#\l . 108)
;       (#\m . 109) (#\n . 110) (#\o . 111) (#\p . 112)
;       (#\q . 113) (#\r . 114) (#\s . 115) (#\t . 116)
;       (#\u . 117) (#\v . 118) (#\w . 119) (#\x . 120)
;       (#\y . 121) (#\z . 122) (#\{ . 123) (#\| . 124)
;       (#\} . 125) (#\~ . 126)))


;(pushnew :clx-ascii *features*)
;(dolist (pair *char-to-ascii-alist*)
;  (when (not (= (char-code (car pair)) (cdr pair)))
;    (return (setq *features* (delete :clx-ascii *features*)))))
;
;)

;(proclaim '(inline char->card8 card8->char))

;#-clx-ascii
;(progn
; 
;(defparameter *char-to-card8-translation-table*
;	      '#.(let ((array (make-array
;				(let ((max-char-code 255))
;				  (dolist (pair *char-to-ascii-alist*)
;				    (setq max-char-code
;					  (max max-char-code (char-code (car pair)))))
;				  (1+ max-char-code))
;				:element-type 'card8)))
;		   (dotimes (i (length array))
;		     (setf (aref array i) (mod i 256)))
;		   (dolist (pair *char-to-ascii-alist*)
;		     (setf (aref array (char-code (car pair))) (cdr pair)))
;		   array))

;(defparameter *card8-to-char-translation-table*
;	      '#.(let ((array (make-string 256)))
;		   (dotimes (i (length array))
;		     (setf (aref array i) (code-char (mod i 256))))
;		   (dolist (pair *char-to-ascii-alist*)
;		     (setf (aref array (cdr pair)) (car pair)))
;		   array))

;#-Genera
;(progn
;  
;(defun char->card8 (char)
;  (declare (type string-char char))
;  #.(declare-buffun)
;  (the card8 (aref (the (simple-array card8 (*)) *char-to-card8-translation-table*)
;		   (the array-index (char-code char)))))
;
;(defun card8->char (card8)
;  (declare (type card8 card8))
;  #.(declare-buffun)
;  (the string-char (aref (the simple-string *card8-to-char-translation-table*) card8)))
;
;)

;#+Genera
;(progn
;
;(defun char->card8 (char)
;  (declare lt:(side-effects reader reducible))
;  (aref *char-to-card8-translation-table* (char-code char)))
;
;(defun card8->char (card8)
;  (declare lt:(side-effects reader reducible))
;  (aref *card8-to-char-translation-table* card8))
;
;)

;(defun check-character-mapping-consistency ()
;  (dotimes (i 256)
;    (unless (= i (char->card8 (card8->char i)))
;      (warn "The card8->char mapping is not invertible through char->card8.  Info:~%~S"
;	    (list i (card8->char i) (char->card8 (card8->char i))))
;      (return nil)))
;  (dotimes (i (length *char-to-card8-translation-table*))
;    (let ((char (code-char i)))
;      (unless (eql char (card8->char (char->card8 char)))
;	(warn "The char->card8 mapping is not invertible through card8->char.  Info:~%~S"
;	      (list char (char->card8 char) (card8->char (char->card8 char))))
;	(return nil)))))
;
;(check-character-mapping-consistency)
;
;)

;#+clx-ascii
;(progn
 
(defun char->card8 (char)
;  (declare (type string-char char))
;  #.(declare-buffun)
;  (the card8 (char-code char)))
  char)

(defun card8->char (card8)
;  (declare (type card8 card8))
;  #.(declare-buffun)
;  (the string-char (code-char card8)))
  card8)

;(eval-when (eval compile)
;  (setq *features* (delete :clx-ascii *features*)))

;)

;;; If you can inhibit asynchronous keyboard aborts inside the body of this
;;; macro, then it is a good idea to do this.  This macro is wrapped around
;;; request writing and reply reading to ensure that requests are atomically
;;; written and replies are atomically read from the stream.

;#-(or Genera excl lcl3.0)
;(defmacro without-aborts (&body body)
;  `(progn ,@body))

(macro without-aborts (body)
       `(progn . ,body))

;;; PROCESS-BLOCK: Wait until a given predicate returns a non-NIL value.
;;; Caller guarantees that PROCESS-WAKEUP will be called after the predicate's
;;; value changes.

;;; CONDITIONAL-STORE:

;; This should use GET-SETF-METHOD to avoid evaluating subforms multiple times.
;; It doesn't because CLtL doesn't pass the environment to GET-SETF-METHOD.
(defmacro conditional-store (place old-value new-value)
  `(without-interrupts
     (cond ((eq ,place ,old-value)
	    (setf ,place ,new-value)
	    t))))

;;;----------------------------------------------------------------------------
;;; IO Error Recovery
;;;	All I/O operations are done within a WRAP-BUF-OUTPUT macro.
;;;	It prevents multiple mindless errors when the network craters.
;;;
;;;----------------------------------------------------------------------------

;;; utilisp
(defmacro wrap-buf-output ((buffer) . body)
  `(unless (buffer-dead ,buffer) . ,body))

;;; utilisp
(defmacro wrap-buf-input ((buffer) . body)
  `(progn . ,body))

;;;----------------------------------------------------------------------------
;;; System dependent IO primitives
;;;	Functions for opening, reading writing forcing-output and closing 
;;;	the stream to the server.
;;;----------------------------------------------------------------------------

;;; OPEN-X-STREAM - create a stream for communicating to the appropriate X
;;; server

;;
;; Note that since we don't use the CL i/o facilities to do i/o, the display
;; input and output "stream" is really a file descriptor (fixnum).
;;

;; C funtion connect-to-server returns UNIX file descripter
(defun open-x-stream (host display protocol)
  ;(declare (ignore protocol));; unused
  (let ((fd (connect-to-server (string host) display)))
    (or fd
	(error "failed to connect to server: ~a ~d" host display))
    fd))

;;; buffer-read-default - read data from the x stream

;;; utilisp
;; fd-wait-for-input: written in c.
;; calls fd-wait-for-input with timeout=0 (no wait). retun value 1 means there's 
;; something to be read.
(defun fd-char-avail-p (fd)
  (eq 1 (fd-wait-for-input fd 0)))

;; vector must be a string object.
(defun buffer-read-default (display vector start end timeout)
  (lets ((howmany (- end start))
	 (fd (display-input-stream display)))
	 ;; if chars are available, read them.
    (or (cond ((fd-char-avail-p fd) nil)
	      ((eq timeout 0) ':timeout)
	      ((buffer-input-wait-default display timeout)))
	(progn (fd-read-bytes fd vector start howmany) nil))))

;;; buffer-write-default - write data to the x stream

(defun buffer-write-default (vector display start end)
  ;;(setq last-packet (substring vector start end))
  (fd-write-bytes (display-output-stream display) vector start (- end start)))

;;; buffer-force-output-default - force output to the x stream

(defun buffer-force-output-default (display)
  nil)

;;; buffer-close-default - close the x stream

;;; close-connection: not have written yet.
;(defun buffer-close-default (display (abort nil))
;  (close-connection (display-output-stream display)))
(defun buffer-close-default (display (abort nil) (ignore))
  (fd-close (display-output-stream display)))

;;; buffer-input-wait-default - wait for for input to be available for the
;;; buffer.  this is called in read-input between requests, so that a process
;;; waiting for input is abortable when between requests.  should return
;;; :timeout if it times out, nil otherwise.

;;; the default implementation

;; utilisp
;; timeout == nil cause fd-wait-for-input  waits forever.
(defun buffer-input-wait-default (display (timeout 0))
  (let ((fd (display-input-stream display)))
    (and (>= fd 0)
	 (let ((res 0))
	   (loop
;	    (setq res (fd-wait-for-input fd (if (null timeout) 0
;					      (truncate timeout))))
	    (setq res (fd-wait-for-input fd timeout))
	    (cond ((plusp res)		; success
		   (exit nil))
		  ((eq res 0)		; timeout
		   (exit ':timeout))
		  ((eq res -1)		; error
		   (exit t))
		  ;; otherwise we got an interrupt -- go around again.
		  ))))))

;;; buffer-listen-default - returns t if there is input available for the
;;;;; buffer. this should never block, so it can be called from the scheduler.
;;
;;;;; the default implementation is to just use listen.

(defun buffer-listen-default (display)
  (let ((stream (display-input-stream display)))
    (if (or (null stream) (= stream -1))
	t
      (fd-char-avail-p stream))))

;;;;;----------------------------------------------------------------------------
;;;;; system dependent speed hacks
;;;;;----------------------------------------------------------------------------
;;
;;;;
;;;; with-stack-list is used by with-state as a memory saving feature.
;;;; if your lisp doesn't have stack-lists, and you're worried about
;;;; consing garbage, you may want to re-write this to allocate and
;;;; initialize lists from a resource.
;;;;
;;;#+lispm
;;;(defmacro with-stack-list ((var &rest elements) &body body)
;;;  `(sys:with-stack-list (,var ,@elements) ,@body))
;;;
;;;#+lispm
;;;(defmacro with-stack-list* ((var &rest elements) &body body)
;;;  `(sys:with-stack-list* (,var ,@elements) ,@body))
;;;
;;;#-lispm
;;;(defmacro with-stack-list ((var &rest elements) &body body)
;;;  ;; syntax: (with-stack-list (var exp1 ... expn) body)
;;;  ;; equivalent to (let ((var (mapcar #'eval '(exp1 ... expn)))) body)
;;;  ;; except that the list produced by mapcar resides on the stack and
;;;  ;; therefore disappears when with-stack-list is exited.
;;;  `(let ((,var (list ,@elements))) ,@body))
;;;
;;;#-lispm
;;;(defmacro with-stack-list* ((var &rest elements) &body body)
;;;  ;; syntax: (with-stack-list* (var exp1 ... expn) body)
;;;  ;; equivalent to (let ((var (apply #'list* (mapcar #'eval '(exp1 ... expn))))) body)
;;;  ;; except that the list produced by mapcar resides on the stack and
;;;  ;; therefore disappears when with-stack-list is exited.
;;;  `(let ((,var (list* ,@elements))) ,@body))
;;;
;;;(proclaim '(inline buffer-replace))
;; uti

(defun buffer-replace (buf1 buf2 start1 end1 (start2 0))
  (string-amend buf1
		(substring buf2 start2 (+ start2 (- end1 start1)))
		start1))

;;;#+lispm
;;;(defmacro with-gcontext-bindings ((gc saved-state indexes ts-index temp-mask temp-gc)
;;;				  &body body)
;;;  ;; don't use svref on LHS because Symbolics didn't define locf for it
;;;  (let* ((local-state (gensym))
;;;	 (bindings `(((aref ,local-state ,ts-index) 0))))	; will become zero anyway
;;;    (dolist (index indexes)
;;;      (push `((aref ,local-state ,index) (svref ,saved-state ,index))
;;;	    bindings))
;;;    `(let ((,local-state (gcontext-local-state ,gc)))
;;;       (declare (type gcontext-state ,local-state))
;;;       (unwind-protect
;;;	   (with-location-bindings ,bindings
;;;	     ,@body)
;;;	 (setf (svref ,local-state ,ts-index) 0)
;;;	 (when ,temp-gc
;;;	   (restore-gcontext-temp-state ,gc ,temp-mask ,temp-gc))
;;;	 (deallocate-gcontext-state ,saved-state)))))
;;;
;;;#-lispm
;;;(defmacro with-gcontext-bindings ((gc saved-state indexes ts-index temp-mask temp-gc)
;;;				  &body body)
;;;  (let ((local-state (gensym))
;;;	(resets nil))
;;;    (dolist (index indexes)
;;;      (push `(setf (svref ,local-state ,index) (svref ,saved-state ,index))
;;;	    resets))
;;;    `(unwind-protect
;;;	 (progn
;;;	   ,@body)
;;;       (let ((,local-state (gcontext-local-state ,gc)))
;;;	 (declare (type gcontext-state ,local-state))
;;;	 ,@resets
;;;	 (setf (svref ,local-state ,ts-index) 0))
;;;       (when ,temp-gc
;;;	 (restore-gcontext-temp-state ,gc ,temp-mask ,temp-gc))
;;;       (deallocate-gcontext-state ,saved-state))))
;;
;;;;;----------------------------------------------------------------------------
;;;;; How error detection should CLX do?
;;;;; Several levels are possible:
;;;;;
;;;;; 1. Do the equivalent of check-type on every argument.
;;;;; 
;;;;; 2. Simply report TYPE-ERROR.  This eliminates overhead of all the format
;;;;;    strings generated by check-type.
;;;;; 
;;;;; 3. Do error checking only on arguments that are likely to have errors
;;;;;    (like keyword names)
;;;;; 
;;;;; 4. Do error checking only where not doing so may dammage the envirnment
;;;;;    on a non-tagged machine (i.e. when storing into a structure that has
;;;;;    been passed in)
;;;;; 
;;;;; 5. No extra error detection code.  On lispm's, ASET may barf trying to
;;;;;    store a non-integer into a number array. 
;;;;; 
;;;;; How extensive should the error checking be?  For example, if the server
;;;;; expects a CARD16, is is sufficient for CLX to check for integer, or
;;;;; should it also check for non-negative and less than 65536?
;;;;;----------------------------------------------------------------------------
;; 
;;;; The *TYPE-CHECK?* constant controls how much error checking is done.
;;;; Possible values are:
;;;;    NIL      - Don't do any error checking
;;;;    t        - Do the equivalent of checktype on every argument
;;;;    :minimal - Do error checking only where errors are likely
;;
;;;;; This controls macro expansion, and isn't changable at run-time You will
;;;;; probably want to set this to nil if you want good performance at
;;;;; production time.
;;;(defconstant *type-check?* #+Genera nil #-Genera t)
(defconstant *type-check?* nil)
;;
;;;; TYPE? is used to allow the code to do error checking at a different level from
;;;; the declarations.  It also does some optimizations for systems that don't have
;;;; good compiler support for TYPEP.  The definitions for CARD32, CARD16, INT16, etc.
;;;; include range checks.  You can modify TYPE? to do less extensive checking
;;;; for these types if you desire.
;;
;(defmacro type? (object type)
;  `(and (vectorp ,object)
;	(eql (vref ,object 0) ,(eval type))))

;(defmacro type? (object type)
;  `(,(xintern (eval type) "-p") ,object))

(defmacro type? (object type)
  (match (eval type)
    ('null `(null ,object))
    (('member . m) (cond ((cdr m) `(memq ,object  ',m))
			 (t `(eq ,object ',(car m)))))
    (@ `(,(xintern @ "-p") ,object))))

;;;; X-TYPE-ERROR is the function called for type errors.
;;;; If you want lots of checking, but are concerned about code size,
;;;; this can be made into a macro that ignores some parameters.
;;

;;; utilisp
(defun x-type-error (obj type (error-string))
  (cond ((error-string)
	 (format "TYPE-ERROR: /s /s /s/n" obj type error-string))
	(t (format "TYPE-ERROR: /s /s/n" obj type))))

;;;(defun x-type-error (object type &optional error-string)
;;;  (x-error 'type-error :object object :type type :type-string error-string))
;;
;;
;;;;-----------------------------------------------------------------------------
;;;; Error handlers
;;;;    Hack up KMP error signaling using zetalisp until the real thing comes 
;;;;    along
;;;;-----------------------------------------------------------------------------
;;
;;;#+(or allegro lcl3.0)
;;;(eval-when (eval compile load)
;;;  (pushnew :clx-cl-error *features*))
;;
;;;(defun default-error-handler (display error-key &rest key-vals
;;;			      &key asynchronous &allow-other-keys)
;;;  (declare (type boolean asynchronous)
;;;	   (dynamic-extent key-vals))
;;;  ;; The default display-error-handler.
;;;  ;; It signals the conditions listed in the DISPLAY file.
;;;  (if asynchronous
;;;      (apply #'x-cerror "Ignore" error-key :display display :error-key error-key key-vals)
;;;      (apply #'x-error error-key :display display :error-key error-key key-vals)))

;; utilisp
;; siyou ga kawatte ru. key-vals ha hajime kara list to site watasareru.
(defun default-error-handler (display error-key key-vals)
  (let ((asynchronous (memq ':asynchronous key-vals)))
    (if (and asynchronous (second asynchronous))
	;; kari.
	(format "X-CERROR: /s display=/s, /s/n" error-key display key-vals)
      (format "X-ERROR: /s display=/s, /s/n" error-key display key-vals)
      (break))))

(defmacro x-error (error-key . rest)
  `(progn (format "X-ERROR: /s /s/n" ,error-key ',rest) (funcall break)))

(defmacro x-cerror (msg error-key . rest)
  `(format "X-CERROR: /c:/s /s/n" ,msg ,error-key ',rest))

;;;#+(and lispm (not Genera) (not clx-cl-error))
;;;(defun x-error (condition &rest keyargs)
;;;  (apply #'sys:signal condition keyargs))
;;;
;;;#+(and lispm (not Genera) (not clx-cl-error))
;;;(defun x-cerror (proceed-format-string condition &rest keyargs)
;;;  (sys:signal (apply #'zl:make-condition condition keyargs)
;;;	      :proceed-types proceed-format-string))
;;;
;;;#+(and Genera (not clx-cl-error))
;;;(defun x-error (condition &rest keyargs)
;;;  (declare (dbg:error-reporter))
;;;  (apply #'sys:signal condition keyargs))
;;;
;;;#+(and Genera (not clx-cl-error))
;;;(defun x-cerror (proceed-format-string condition &rest keyargs)
;;;  (declare (dbg:error-reporter))
;;;  (apply #'sys:signal condition :continue-format-string proceed-format-string keyargs))
;;;
;;;#+clx-cl-error
;;;(defun x-error (condition &rest keyargs)
;;;  (declare (dynamic-extent keyargs))
;;;  (apply #'error condition keyargs))
;;;
;;;#+clx-cl-error
;;;(defun x-cerror (proceed-format-string condition &rest keyargs)
;;;  (declare (dynamic-extent keyargs))
;;;  (apply #'cerror proceed-format-string condition keyargs))
;;;
;;;#-(or lispm clx-cl-error)
;;;(defun x-error (condition &rest keyargs)
;;;  (error "X-Error: ~a"
;;;	 (princ-to-string (apply #'make-condition condition keyargs))))
;;;
;;;#-(or lispm clx-cl-error)
;;;(defun x-cerror (proceed-format-string condition &rest keyargs)
;;;  (cerror proceed-format-string "X-Error: ~a"
;;;	 (princ-to-string (apply #'make-condition condition keyargs))))
;;;
;;;;; version 15 of Pitman error handling defines the syntax for define-condition to be:
;;;;; DEFINE-CONDITION name (parent-type) [({slot}*) {option}*]
;;;;; Where option is one of: (:documentation doc-string) (:conc-name symbol-or-string)
;;;;; or (:report exp)
;;;
;;;#+(and lispm (not clx-cl-error))
;;;(defmacro define-condition (name parents &body options)
;;;  (let ((slots (pop options))
;;;	(documentation nil)
;;;	(conc-name (concatenate 'string (string name) "-"))	       
;;;	(reporter nil))
;;;    (dolist (item options)
;;;      (ecase (first item)
;;;	(:documentation (setq documentation (second item)))
;;;	(:conc-name (setq conc-name (string (second item))))
;;;	(:report (setq reporter (second item)))))
;;;    `(within-definition (,name define-condition)
;;;       (zl:defflavor ,name ,slots ,parents
;;;	 :initable-instance-variables
;;;	 #-Genera
;;;	 (:accessor-prefix ,conc-name)
;;;	 #+Genera
;;;	 (:conc-name ,conc-name)
;;;	 #-Genera
;;;	 (:outside-accessible-instance-variables ,@slots)
;;;	 #+Genera
;;;	 (:readable-instance-variables ,@slots))
;;;       ,(when reporter ;; when no reporter, parent's is inherited
;;;	  `(zl:defmethod #-Genera (,name :report)
;;;	                 #+Genera (dbg:report ,name) (stream)
;;;	      ,(if (stringp reporter)
;;;		   `(write-string ,reporter stream)
;;;		 `(,reporter global:self stream))
;;;	      global:self))
;;;       (zl:compile-flavor-methods ,name)
;;;       ,(when documentation
;;;	  `(setf (documentation name 'type) ,documentation))
;;;       ',name)))
;;;
;;;#+(and lispm (not Genera) (not clx-cl-error))
;;;(zl:defflavor x-error () (global:error))
;;;
;;;#+(and Genera (not clx-cl-error))
;;;(scl:defflavor x-error
;;;	((dbg:proceed-types '(:continue))	;
;;;	 continue-format-string)
;;;	(sys:error)
;;;  (:initable-instance-variables continue-format-string))
;;;
;;;#+(and Genera (not clx-cl-error))
;;;(scl:defmethod (scl:make-instance x-error) (&rest ignore)
;;;  (when (not (sys:variable-boundp continue-format-string))
;;;    (setf dbg:proceed-types (remove :continue dbg:proceed-types))))
;;;
;;;#+(and Genera (not clx-cl-error))
;;;(scl:defmethod (dbg:proceed x-error :continue) ()
;;;  :continue)
;;;
;;;#+(and Genera (not clx-cl-error))
;;;(sys:defmethod (dbg:document-proceed-type x-error :continue) (stream)
;;;  (format stream continue-format-string))
;;;
;;;#+clx-cl-error
;;;(define-condition x-error (error))
;;;
;;;#-(or lispm clx-cl-error)
;;;(defstruct x-error
;;;  report-function)
;;;
;;;#-(or lispm clx-cl-error)
;;;(defun reporter-for-condition (name)
;;;  (xintern "." name '-reporter.))
;;;
;;;#-(or lispm clx-cl-error)
;;;(defmacro define-condition (name parents &body options)
;;;  ;; Define a structure that when printed displays an error message
;;;  (let ((slots (pop options))
;;;	(documentation nil)
;;;	(conc-name (concatenate 'string (string name) "-"))	       
;;;	(reporter nil)
;;;	(condition (gensym))
;;;	(stream (gensym))
;;;	(report-function (reporter-for-condition name)))
;;;    (dolist (item options)
;;;      (ecase (first item)
;;;	(:documentation (setq documentation (second item)))
;;;	(:conc-name (setq conc-name (string (second item))))
;;;	(:report (setq reporter (second item)))))
;;;    (unless reporter (setq report-function (reporter-for-condition (car parents))))
;;;    `(within-definition (,name define-condition)
;;;       (defstruct (,name (:conc-name ,(intern conc-name))
;;;		         (:print-function condition-print)
;;;			 (:include ,(car parents) (report-function ',report-function)))
;;;	 ,@slots)
;;;       ,(when documentation
;;;	  `(setf (documentation name 'type) ,documentation))
;;;       ,(when reporter
;;;	  `(defun ,report-function (,condition ,stream)
;;;	     ,(if (stringp reporter)
;;;		  `(write-string ,reporter ,stream)
;;;		`(,reporter ,condition ,stream))
;;;	     ,condition))
;;;       ',name)))
;;;
;;;#-(or lispm clx-cl-error)
;;;(defun condition-print (condition stream depth)
;;;  (declare (type x-error condition)
;;;	   (type stream stream)
;;;	   (ignore depth))
;;;  (if *print-escape*
;;;      (printing-object (condition stream))
;;;    (funcall (x-error-report-function condition) condition stream))
;;;  condition)
;;;  
;;;#-(or lispm clx-cl-error)
;;;(defun make-condition (type &rest slot-initializations)
;;;  (declare (dynamic-extent slot-initializations))
;;;  (let ((make-function (intern (concatenate 'string (string 'make-) (string type))
;;;			       (symbol-package type))))
;;;    (apply make-function slot-initializations)))
;;
;;

;;;;-----------------------------------------------------------------------------
;;;;  HOST hacking
;;;;-----------------------------------------------------------------------------
;;
;;;#-(or explorer Genera)
;;;(defun host-address (host &optional (family :internet))
;;;  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
;;;  ;; and cdr is a list of network address bytes.
;;;  (declare (type (or stringable list) host)
;;;	   (type (or null (member :internet :decnet :chaos) card8) family))
;;;  (declare (values list))
;;;  host family
;;;  (error "HOST-ADDRESS not implemented yet."))
;;;
;;;#+explorer
;;;(defun host-address (host &optional (family :internet))
;;;  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
;;;  ;; and cdr is a list of network address bytes.
;;;  (declare (type (or stringable list) host)
;;;	   (type (or null (member :internet :decnet :chaos) card8) family))
;;;  (declare (values list))
;;;  (ecase family
;;;    (:internet
;;;     (let ((addr (ip:get-ip-address host)))
;;;       (unless addr (error "~s isn't an internet host name" host))
;;;       (list :internet
;;;	     (ldb (byte 8 24) addr)
;;;	     (ldb (byte 8 16) addr)
;;;	     (ldb (byte 8 8) addr)
;;;	     (ldb (byte 8 0) addr))))
;;;    (:chaos
;;;     (let ((addr (first (chaos:chaos-addresses host))))
;;;       (unless addr (error "~s isn't a chaos host name" host))
;;;       (list :chaos
;;;	     (ldb (byte 8 0) addr)
;;;	     (ldb (byte 8 8) addr))))))
;;;
;;;#+Genera
;;;(defun host-address (host &optional (family :internet))
;;;  ;; Return a list whose car is the family keyword (:internet :DECnet :Chaos)
;;;  ;; and cdr is a list of network address bytes.
;;;  (declare (type (or stringable list) host)
;;;	   (type (or null (member :internet :decnet :chaos) card8) family))
;;;  (declare (values list))
;;;  (let ((net-type (if (eq family :DECnet)
;;;		      :DNA
;;;		      family)))
;;;    (dolist (addr
;;;	      (sys:send (net:parse-host host) :network-addresses)
;;;	      (error "~s isn't a valid ~(~A~) host name" host family))
;;;      (let ((network (car addr))
;;;	    (address (cadr addr)))
;;;	(when (sys:send network :network-typep net-type)
;;;	  (return (ecase family
;;;		    (:internet
;;;		      (multiple-value-bind (a b c d) (tcp:explode-internet-address address)
;;;			(list :internet a b c d)))
;;;		    ((:chaos :DECnet)
;;;		     (list family (ldb (byte 8 0) address) (ldb (byte 8 8) address))))))))))
;;;
;;;#+explorer ;; This isn't required, but it helps make sense of the results from access-hosts
;;;(defun get-host (host-object)
;;;  ;; host-object is a list whose car is the family keyword (:internet :DECnet :Chaos)
;;;  ;; and cdr is a list of network address bytes.
;;;  (declare (type list host-object))
;;;  (declare (values string family))
;;;  (let* ((family (first host-object))
;;;	 (address (ecase family
;;;		    (:internet
;;;		     (dpb (second host-object)
;;;			  (byte 8 24)
;;;			  (dpb (third host-object)
;;;			       (byte 8 16)
;;;			       (dpb (fourth host-object)
;;;				    (byte 8 8)
;;;				    (fifth host-object)))))
;;;		    (:chaos
;;;		     (dpb (third host-object) (byte 8 8) (second host-object))))))
;;;    (when (eq family :internet) (setq family :ip))
;;;    (let ((host (si:get-host-from-address address family)))
;;;      (values (and host (funcall host :name)) family))))
;;;
;;;;;; This isn't required, but it helps make sense of the results from access-hosts
;;;#+Genera
;;;(defun get-host (host-object)
;;;  ;; host-object is a list whose car is the family keyword (:internet :DECnet :Chaos)
;;;  ;; and cdr is a list of network address bytes.
;;;  (declare (type list host-object))
;;;  (declare (values string family))
;;;  (let ((family (first host-object)))
;;;    (values (sys:send (net:get-host-from-address 
;;;			(ecase family
;;;			  (:internet
;;;			    (apply #'tcp:build-internet-address (rest host-object)))
;;;			  ((:chaos :DECnet)
;;;			   (dpb (third host-object) (byte 8 8) (second host-object))))
;;;			(net:local-network-of-type (if (eq family :DECnet)
;;;						       :DNA
;;;						       family)))
;;;		      :name)
;;;	    family)))
;;
;;
;;;;-----------------------------------------------------------------------------
;;;; Whether to use closures for requests or not.
;;;;-----------------------------------------------------------------------------
;;
;;;;; If this macro expands to non-NIL, then request and locking code is
;;;;; compiled in a much more compact format, as the common code is shared, and
;;;;; the specific code is built into a closure that is funcalled by the shared
;;;;; code.  If your compiler makes efficient use of closures then you probably
;;;;; want to make this expand to T, as it makes the code more compact.
;;
;;;(defmacro use-closures ()
;;;  #+lispm t #-lispm nil)
;;(defmacro use-closures () nil)
;;
;;;;-----------------------------------------------------------------------------
;;;; Resource stuff
;;;;-----------------------------------------------------------------------------
;;
;;
;;;;; DEFAULT-RESOURCES-PATHNAME - The pathname of the resources file to load if
;;;;; a resource manager isn't running.
;;
;;;(defun default-resources-pathname ()
;;;  (when #+unix t #-unix (search "Unix" (software-type) :test #'char-equal)
;;;    (merge-pathnames (user-homedir-pathname) (pathname ".Xdefaults"))))
;;(defun default-resource-pathname ()
;;  (string-append (getenv "HOME") "//.Xdefaults"))
;;
;;
;;;;; RESOURCES-PATHNAME - The pathname of the resources file to load after the
;;;;; defaults have been loaded.
;;
;;;;(defun resources-pathname ()
;;;;  (when #+unix t #-unix (search "Unix" (software-type) :test #'char-equal)
;;;;    (or #+(or excl lcl3.0)
;;;;	(let ((string (#+excl sys:getenv
;;;;		       #+lcl3.0 lcl:environment-variable
;;;;		       "XENVIRONMENT")))
;;;;	  (when string
;;;;	    (pathname string)))
;;;;	(merge-pathnames
;;;;	  (user-homedir-pathname)
;;;;	  (pathname 
;;;;	    (concatenate 'simple-string ".Xdefaults-"
;;;;			 #+excl (short-site-name)
;;;;			 #-excl (machine-instance)))))))
;;;;
;;;
;;;; utilisp needs gethostname.
;;;;(defun resources-pathname ()
;;;;  (string-append (getenv "HOME") "//.Xdefaults-" ))
;;;
;;;;;-----------------------------------------------------------------------------
;;;;; GC stuff
;;;;;-----------------------------------------------------------------------------
;;;
;;;#+Genera
;;;(si:define-gc-cleanup clx-cleanup ("CLX Cleanup")
;;;  (declare (special *event-free-list*
;;;		    *pending-command-free-list*
;;;		    *reply-buffer-free-lists*
;;;		    *gcontext-local-state-cache*
;;;		    *temp-gcontext-cache*))
;;;  (setq *event-free-list* nil)
;;;  (setq *pending-command-free-list* nil)
;;;  (fill *reply-buffer-free-lists* nil)
;;;  (setq *gcontext-local-state-cache* nil)
;;;  (setq *temp-gcontext-cache* nil))
;;
;;
;;;;-----------------------------------------------------------------------------
;;;; Image stuff
;;;;-----------------------------------------------------------------------------
;;
;;;(deftype pixarray-1-element-type ()
;;;  'bit)
;;;
;;;(deftype pixarray-4-element-type ()
;;;  'card4)
;;;
;;;(deftype pixarray-8-element-type ()
;;;  'card8)
;;
;;;(deftype pixarray-16-element-type ()
;;;  'card16)
;;
;;;(deftype pixarray-24-element-type ()
;;;  #-Genera 'card24 #+Genera 'int32)
;;;
;;;(deftype pixarray-32-element-type ()
;;;  #-Genera 'card32 #+Genera 'int32)
;;;
;;;(deftype pixarray-1  ()
;;;  '(array pixarray-1-element-type (* *)))
;;;
;;;(deftype pixarray-4  ()
;;;  '(array pixarray-4-element-type (* *)))
;;;
;;;(deftype pixarray-8  ()
;;;  '(array pixarray-8-element-type (* *)))
;;;
;;;(deftype pixarray-16 ()
;;;  '(array pixarray-16-element-type (* *)))
;;;
;;;(deftype pixarray-24 ()
;;;  '(array pixarray-24-element-type (* *)))
;;;
;;;(deftype pixarray-32 ()
;;;  '(array pixarray-32-element-type (* *)))
;;;
;;;(deftype pixarray ()
;;;  '(or pixarray-1 pixarray-4 pixarray-8 pixarray-16 pixarray-24 pixarray-32))
;;;
;;;(deftype bitmap ()
;;;  'pixarray-1)
;;
;;
;;;;; These are used to read and write pixels from and to CARD8s.
;;
;;;;; READ-IMAGE-LOAD-BYTE is used to extract 1 and 4 bit pixels from CARD8s.
;;
;;;(defmacro read-image-load-byte (size position integer)
;;;  `(the (unsigned-byte ,size)
;;;	(#-Genera ldb #+Genera sys:%logldb
;;;	 (byte ,size ,(if *image-bit-lsb-first-p* position (- 7 position)))
;;;	 (the card8 ,integer))))
;;
;;;;; READ-IMAGE-ASSEMBLE-BYTES is used to build 16, 24 and 32 bit pixels from
;;;;; the appropriate number of CARD8s.
;;
;;;(defmacro read-image-assemble-bytes (&rest bytes)
;;;  (let* ((bytes (if *image-byte-lsb-first-p* bytes (reverse bytes)))
;;;	 (it (first bytes))
;;;	 (count 0))
;;;    (dolist (byte (rest bytes))
;;;      (setq it
;;;	    `(#-Genera dpb #+Genera sys:%logdpb 
;;;	      (the card8 ,byte)
;;;	      (byte 8 ,(incf count 8))
;;;	      (the (unsigned-byte ,count) ,it))))
;;;    #-Genera `(the (unsigned-byte ,(* (length bytes) 8)) ,it)
;;;    #+Genera it))
;;
;;;;; WRITE-IMAGE-LOAD-BYTE is used to extract a CARD8 from a 16, 24 or 32 bit
;;;;; pixel.
;;
;;;(defmacro write-image-load-byte (position integer integer-size)
;;;  integer-size
;;;  `(the card8
;;;	(#-Genera ldb #+Genera sys:%logldb
;;;	  (byte 8 ,(if *image-byte-lsb-first-p*
;;;		       position
;;;		     (- integer-size 8 position)))
;;;	  #-Genera (the (unsigned-byte ,integer-size) ,integer)
;;;	  #+Genera ,integer
;;;	  )))
;;
;;;;; WRITE-IMAGE-ASSEMBLE-BYTES is used to build a CARD8 from 1 or 4 bit
;;;;; pixels.
;;
;;;(defmacro write-image-assemble-bytes (&rest bytes)
;;;  (let* ((bytes (if *image-bit-lsb-first-p* bytes (reverse bytes)))
;;;	 (size (floor 8 (length bytes)))
;;;	 (it (first bytes))
;;;	 (count 0))
;;;    (dolist (byte (rest bytes))
;;;      (setq it `(#-Genera dpb #+Genera sys:%logdpb
;;;		 (the (unsigned-byte ,size) ,byte)
;;;		 (byte ,size ,(incf count size))
;;;		 (the (unsigned-byte ,count) ,it))))
;;;    `(the card8 ,it)))
;;
;;;;; If you can write fast routines that can read and write pixarrays out of a
;;;;; buffer-bytes, do it!  It makes the image code a lot faster.  The
;;;;; FAST-READ-PIXARRAY, FAST-WRITE-PIXARRAY and FAST-COPY-PIXARRAY routines
;;;;; return T if they can do it, NIL if they can't.
;;
;;;;; FAST-READ-PIXARRAY - fill part of a pixarray from a buffer of card8s
;;
;;;#+(or lcl3.0 excl)
;;;(defun fast-read-pixarray-1 (buffer-bbuf index array x y width height  
;;;			     padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-1 array)
;;;	   (type card16 x y width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((vector (underlying-simple-vector array))
;;;	  (start (index+ index
;;;			 (index* y padded-bytes-per-line)
;;;			 (index-ceiling x 8))
;;;		 (index+ start padded-bytes-per-line))
;;;	  (y 0 (index1+ y))
;;;	  (left-bits (index-mod (index- x) 8))
;;;	  (right-bits (index-mod (index- width left-bits) 8))
;;;	  (middle-bits (index- width left-bits right-bits))
;;;	  (middle-bytes (index-floor middle-bits 8)))
;;;	 ((index>= y height))
;;;      (declare (type (simple-array pixarray-1-element-type (*)) vector)
;;;	       (type array-index start y
;;;		     left-bits right-bits middle-bits middle-bytes))
;;;      (cond ((index< middle-bits 0)
;;;	     (let ((byte (aref buffer-bbuf (index1- start)))
;;;		   (x (array-row-major-index array y left-bits)))
;;;	       (declare (type card8 byte)
;;;			(type array-index x))
;;;	       (when (index> right-bits 6)
;;;		 (setf (aref vector (index- x 1))
;;;		       (read-image-load-byte 1 7 byte)))
;;;	       (when (and (index> left-bits 1)
;;;			  (index> right-bits 5))
;;;		 (setf (aref vector (index- x 2))
;;;		       (read-image-load-byte 1 6 byte)))
;;;	       (when (and (index> left-bits 2)
;;;			  (index> right-bits 4))
;;;		 (setf (aref vector (index- x 3))
;;;		       (read-image-load-byte 1 5 byte)))
;;;	       (when (and (index> left-bits 3)
;;;			  (index> right-bits 3))
;;;		 (setf (aref vector (index- x 4))
;;;		       (read-image-load-byte 1 4 byte)))
;;;	       (when (and (index> left-bits 4)
;;;			  (index> right-bits 2))
;;;		 (setf (aref vector (index- x 5))
;;;		       (read-image-load-byte 1 3 byte)))
;;;	       (when (and (index> left-bits 5)
;;;			  (index> right-bits 1))
;;;		 (setf (aref vector (index- x 6))
;;;		       (read-image-load-byte 1 2 byte)))
;;;	       (when (index> left-bits 6)
;;;		 (setf (aref vector (index- x 7))
;;;		       (read-image-load-byte 1 1 byte)))))
;;;	    (t
;;;	     (unless (index-zerop left-bits)
;;;	       (let ((byte (aref buffer-bbuf (index1- start)))
;;;		     (x (array-row-major-index array y left-bits)))
;;;		 (declare (type card8 byte)
;;;			  (type array-index x))
;;;		 (setf (aref vector (index- x 1))
;;;		       (read-image-load-byte 1 7 byte))
;;;		 (when (index> left-bits 1)
;;;		   (setf (aref vector (index- x 2))
;;;			 (read-image-load-byte 1 6 byte))
;;;		   (when (index> left-bits 2)
;;;		     (setf (aref vector (index- x 3))
;;;			   (read-image-load-byte 1 5 byte))
;;;		     (when (index> left-bits 3)
;;;		       (setf (aref vector (index- x 4))
;;;			     (read-image-load-byte 1 4 byte))
;;;		       (when (index> left-bits 4)
;;;			 (setf (aref vector (index- x 5))
;;;			       (read-image-load-byte 1 3 byte))
;;;			 (when (index> left-bits 5)
;;;			   (setf (aref vector (index- x 6))
;;;				 (read-image-load-byte 1 2 byte))
;;;			   (when (index> left-bits 6)
;;;			     (setf (aref vector (index- x 7))
;;;				   (read-image-load-byte 1 1 byte))
;;;			     ))))))))
;;;	     (do* ((end (index+ start middle-bytes))
;;;		   (i start (index1+ i))
;;;		   (x (array-row-major-index array y left-bits) (index+ x 8)))
;;;		  ((index>= i end)
;;;		   (unless (index-zerop right-bits)
;;;		     (let ((byte (aref buffer-bbuf end))
;;;			   (x (array-row-major-index
;;;				array y (index+ left-bits middle-bits))))
;;;		       (declare (type card8 byte)
;;;				(type array-index x))
;;;		       (setf (aref vector (index+ x 0))
;;;			     (read-image-load-byte 1 0 byte))
;;;		       (when (index> right-bits 1)
;;;			 (setf (aref vector (index+ x 1))
;;;			       (read-image-load-byte 1 1 byte))
;;;			 (when (index> right-bits 2)
;;;			   (setf (aref vector (index+ x 2))
;;;				 (read-image-load-byte 1 2 byte))
;;;			   (when (index> right-bits 3)
;;;			     (setf (aref vector (index+ x 3))
;;;				   (read-image-load-byte 1 3 byte))
;;;			     (when (index> right-bits 4)
;;;			       (setf (aref vector (index+ x 4))
;;;				     (read-image-load-byte 1 4 byte))
;;;			       (when (index> right-bits 5)
;;;				 (setf (aref vector (index+ x 5))
;;;				       (read-image-load-byte 1 5 byte))
;;;				 (when (index> right-bits 6)
;;;				   (setf (aref vector (index+ x 6))
;;;					 (read-image-load-byte 1 6 byte))
;;;				   )))))))))
;;;	       (declare (type array-index end i x))
;;;	       (let ((byte (aref buffer-bbuf i)))
;;;		 (declare (type card8 byte))
;;;		 (setf (aref vector (index+ x 0))
;;;		       (read-image-load-byte 1 0 byte))
;;;		 (setf (aref vector (index+ x 1))
;;;		       (read-image-load-byte 1 1 byte))
;;;		 (setf (aref vector (index+ x 2))
;;;		       (read-image-load-byte 1 2 byte))
;;;		 (setf (aref vector (index+ x 3))
;;;		       (read-image-load-byte 1 3 byte))
;;;		 (setf (aref vector (index+ x 4))
;;;		       (read-image-load-byte 1 4 byte))
;;;		 (setf (aref vector (index+ x 5))
;;;		       (read-image-load-byte 1 5 byte))
;;;		 (setf (aref vector (index+ x 6))
;;;		       (read-image-load-byte 1 6 byte))
;;;		 (setf (aref vector (index+ x 7))
;;;		       (read-image-load-byte 1 7 byte))))
;;;	     ))))
;;  t)
;;
;;;#+(or lcl3.0 excl)
;;;(defun fast-read-pixarray-4 (buffer-bbuf index array x y width height 
;;;			     padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-4 array)
;;;	   (type card16 x y width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((vector (underlying-simple-vector array))
;;;	  (start (index+ index
;;;			 (index* y padded-bytes-per-line)
;;;			 (index-ceiling x 2))
;;;		 (index+ start padded-bytes-per-line))
;;;	  (y 0 (index1+ y))
;;;	  (left-nibbles (index-mod (index- x) 2))
;;;	  (right-nibbles (index-mod (index- width left-nibbles) 2))
;;;	  (middle-nibbles (index- width left-nibbles right-nibbles))
;;;	  (middle-bytes (index-floor middle-nibbles 2)))
;;;	 ((index>= y height))
;;;      (declare (type (simple-array pixarray-4-element-type (*)) vector)
;;;	       (type array-index start y
;;;		     left-nibbles right-nibbles middle-nibbles middle-bytes))
;;;      (unless (index-zerop left-nibbles)
;;;	(setf (aref array y 0)
;;;	      (read-image-load-byte
;;;		4 4 (aref buffer-bbuf (index1- start)))))
;;;      (do* ((end (index+ start middle-bytes))
;;;	    (i start (index1+ i))
;;;	    (x (array-row-major-index array y left-nibbles) (index+ x 2)))
;;;	   ((index>= i end)
;;;	    (unless (index-zerop right-nibbles)
;;;	      (setf (aref array y (index+ left-nibbles middle-nibbles))
;;;		    (read-image-load-byte 4 0 (aref buffer-bbuf end)))))
;;;	(declare (type array-index end i x))
;;;	(let ((byte (aref buffer-bbuf i)))
;;;	  (declare (type card8 byte))
;;;	  (setf (aref vector (index+ x 0))
;;;		(read-image-load-byte 4 0 byte))
;;;	  (setf (aref vector (index+ x 1))
;;;		(read-image-load-byte 4 4 byte))))
;;;      ))
;;;  t)
;;;
;;;#+(or lcl3.0 excl)
;;;(defun fast-read-pixarray-8 (buffer-bbuf index array x y width height 
;;;			     padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-8 array)
;;;	   (type card16 x y width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((vector (underlying-simple-vector array))
;;;	  (start (index+ index
;;;			 (index* y padded-bytes-per-line)
;;;			 x)
;;;		 (index+ start padded-bytes-per-line))
;;;	  (y 0 (index1+ y)))
;;;	 ((index>= y height))
;;;      (declare (type (simple-array pixarray-8-element-type (*)) vector)
;;;	       (type array-index start y))
;;;      (do* ((end (index+ start width))
;;; 	    (i start (index1+ i))
;;; 	    (x (array-row-major-index array y 0) (index1+ x)))
;;; 	   ((index>= i end))
;;; 	(declare (type array-index end i x))
;;; 	(setf (aref vector x)
;;; 	      (the card8 (aref buffer-bbuf i))))))
;;;  t)
;;;
;;;#+(or lcl3.0 excl)
;;;(defun fast-read-pixarray-16 (buffer-bbuf index array x y width height 
;;;			      padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-16 array)
;;;	   (type card16 width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((vector (underlying-simple-vector array))
;;;	  (start (index+ index
;;;			 (index* y padded-bytes-per-line)
;;;			 (index* x 2))
;;;		 (index+ start padded-bytes-per-line))
;;;	  (y 0 (index1+ y)))
;;;	 ((index>= y height))
;;;      (declare (type (simple-array pixarray-16-element-type (*)) vector)
;;;	       (type array-index start y))
;;;      (do* ((end (index+ start (index* width 2)))
;;;	    (i start (index+ i 2))
;;;	    (x (array-row-major-index array y 0) (index1+ x)))
;;;	   ((index>= i end))
;;;	(declare (type array-index end i x))
;;;	(setf (aref vector x)
;;;	      (read-image-assemble-bytes
;;;		(aref buffer-bbuf (index+ i 0))
;;;		(aref buffer-bbuf (index+ i 1)))))))
;;;  t)
;;;
;;;#+Genera
;;;(defun fast-read-pixarray-24 (buffer-bbuf index array x y width height 
;;;			      padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-24 array)
;;;	   (type card16 width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((array array)
;;;	  (start (index+ index
;;;			 (index* y padded-bytes-per-line)
;;;			 (index* x 3))
;;;		 (index+ start padded-bytes-per-line))
;;;	  (y 0 (index1+ y)))
;;;	 ((index>= y height))
;;;      (declare (sys:array-register-1d array)
;;;	       (type array-index start y))
;;;      (do* ((end (index+ start (index* width 3)))
;;;	    (i start (index+ i 3))
;;;	    (x (array-row-major-index array y 0) (index1+ x)))
;;;	   ((index>= i end))
;;;	(declare (type array-index end i x))
;;;	(setf (sys:%1d-aref array x)
;;;	      (read-image-assemble-bytes
;;;		(aref buffer-bbuf (index+ i 0))
;;;		(aref buffer-bbuf (index+ i 1))
;;;		(aref buffer-bbuf (index+ i 2)))))))
;;;  t)
;;;
;;;#+(or lcl3.0 excl)
;;;(defun fast-read-pixarray-24 (buffer-bbuf index array x y width height 
;;;			      padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-24 array)
;;;	   (type card16 width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((vector (underlying-simple-vector array))
;;;	  (start (index+ index
;;;			 (index* y padded-bytes-per-line)
;;;			 (index* x 3))
;;;		 (index+ start padded-bytes-per-line))
;;;	  (y 0 (index1+ y)))
;;;	 ((index>= y height))
;;;      (declare (type (simple-array pixarray-24-element-type (*)) vector)
;;;	       (type array-index start y))
;;;      (do* ((end (index+ start (index* width 3)))
;;;	    (i start (index+ i 3))
;;;	    (x (array-row-major-index array y 0) (index1+ x)))
;;;	   ((index>= i end))
;;;	(declare (type array-index end i x))
;;;	(setf (aref vector x)
;;;	      (read-image-assemble-bytes
;;;		(aref buffer-bbuf (index+ i 0))
;;;		(aref buffer-bbuf (index+ i 1))
;;;		(aref buffer-bbuf (index+ i 2)))))))
;;;  t)
;;;
;;;#+(or lcl3.0 excl)
;;;(defun fast-read-pixarray-32 (buffer-bbuf index array x y width height 
;;;			      padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-32 array)
;;;	   (type card16 width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((vector (underlying-simple-vector array))
;;;	  (start (index+ index
;;;			 (index* y padded-bytes-per-line)
;;;			 (index* x 4))
;;;		 (index+ start padded-bytes-per-line))
;;;	  (y 0 (index1+ y)))
;;;	 ((index>= y height))
;;;      (declare (type (simple-array pixarray-32-element-type (*)) vector)
;;;	       (type array-index start y))
;;;      (do* ((end (index+ start (index* width 4)))
;;;	    (i start (index+ i 4))
;;;	    (x (array-row-major-index array y 0) (index1+ x)))
;;;	   ((index>= i end))
;;;	(declare (type array-index end i x))
;;;	(setf (aref vector x)
;;;	      (read-image-assemble-bytes
;;;		(aref buffer-bbuf (index+ i 0))
;;;		(aref buffer-bbuf (index+ i 1))
;;;		(aref buffer-bbuf (index+ i 2))
;;;		(aref buffer-bbuf (index+ i 3)))))))
;;;  t)
;;;
;;;(defun fast-read-pixarray (bbuf boffset pixarray
;;;			   x y width height padded-bytes-per-line
;;;			   bits-per-pixel)
;;;  (declare (type buffer-bytes bbuf)
;;;	   (type array-index boffset
;;;		 padded-bytes-per-line)
;;;	   (type pixarray pixarray)
;;;	   (type card16 x y width height)
;;;	   (type (member 1 4 8 16 24 32) bits-per-pixel))
;;;  (progn bbuf boffset pixarray x y width height padded-bytes-per-line
;;;	 bits-per-pixel)
;;;  (or
;;;    #+lispm
;;;    (let* ((padded-bits-per-line (* padded-bytes-per-line 8))
;;;	   (padded-pixels-per-line
;;;	     (floor padded-bits-per-line bits-per-pixel))
;;;	   (pixarray-padded-pixels-per-line
;;;	     #+Genera (sys:array-row-span pixarray)
;;;	     #-Genera (array-dimension pixarray 1))
;;;	   (pixarray-padded-bits-per-line
;;;	     (* pixarray-padded-pixels-per-line bits-per-pixel)))
;;;      (when (and (= (sys:array-element-size pixarray) bits-per-pixel)
;;;		 (zerop (index-mod padded-bits-per-line 32))
;;;		 (zerop (index-mod pixarray-padded-bits-per-line 32)))
;;;	(#+Genera sys:stack-let* #-Genera let*
;;;	 ((dimensions (list height padded-pixels-per-line))
;;;	  (a (make-array
;;;	       dimensions
;;;	       :element-type (array-element-type pixarray)
;;;	       :displaced-to bbuf
;;;	       :displaced-index-offset (floor (* boffset 8) bits-per-pixel))))
;;;	 (sys:bitblt boole-1 width height a x y pixarray 0 0))
;;;	t))
;;;    #+Genera
;;;    (when (= bits-per-pixel 24)
;;;      (fast-read-pixarray-24
;;;	bbuf boffset pixarray x y width height padded-bytes-per-line))
;;;    #+(or lcl3.0 excl)
;;;    (funcall
;;;      (ecase bits-per-pixel 
;;;	(1 #'fast-read-pixarray-1) (4 #'fast-read-pixarray-4)
;;;	(8 #'fast-read-pixarray-8) (16 #'fast-read-pixarray-16)
;;;	(24 #'fast-read-pixarray-24) (32 #'fast-read-pixarray-32))
;;;      bbuf boffset pixarray x y width height padded-bytes-per-line)
;;;    ))
;;;
;;;;;; FAST-WRITE-PIXARRAY - copy part of a pixarray into an array of CARD8s
;;;
;;;#+(or lcl3.0 excl)
;;;(defun fast-write-pixarray-1 (buffer-bbuf index array x y width height
;;;			      padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-1 array)
;;;	   (type card16 x y width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((vector (underlying-simple-vector array))
;;;	  (h 0 (index1+ h))
;;;	  (y y (index1+ y))
;;;	  (right-bits (index-mod width 8))
;;;	  (middle-bits (index- width right-bits))
;;;	  (middle-bytes (index-ceiling middle-bits 8))
;;;	  (start index (index+ start padded-bytes-per-line)))
;;;	 ((index>= h height))
;;;      (declare (type (simple-array pixarray-1-element-type (*)) vector)
;;;	       (type array-index h y right-bits middle-bits
;;;		     middle-bytes start))
;;;      (do* ((end (index+ start middle-bytes))
;;;	    (i start (index1+ i))
;;;	    (start-x x)
;;;	    (x (array-row-major-index array y start-x) (index+ x 8)))
;;;	   ((index>= i end)
;;;	    (unless (index-zerop right-bits)
;;;	      (let ((x (array-row-major-index
;;;			 array y (index+ start-x middle-bits))))
;;;		(declare (type array-index x))
;;;		(setf (aref buffer-bbuf end)
;;;		      (write-image-assemble-bytes
;;;			(aref vector (index+ x 0))
;;;			(if (index> right-bits 1)
;;;			    (aref vector (index+ x 1))
;;;			  0)
;;;			(if (index> right-bits 2)
;;;			    (aref vector (index+ x 2))
;;;			  0)
;;;			(if (index> right-bits 3)
;;;			    (aref vector (index+ x 3))
;;;			  0)
;;;			(if (index> right-bits 4)
;;;			    (aref vector (index+ x 4))
;;;			  0)
;;;			(if (index> right-bits 5)
;;;			    (aref vector (index+ x 5))
;;;			  0)
;;;			(if (index> right-bits 6)
;;;			    (aref vector (index+ x 6))
;;;			  0)
;;;			0)))))
;;;	(declare (type array-index end i start-x x))
;;;	(setf (aref buffer-bbuf i)
;;;	      (write-image-assemble-bytes
;;;		(aref vector (index+ x 0))
;;;		(aref vector (index+ x 1))
;;;		(aref vector (index+ x 2))
;;;		(aref vector (index+ x 3))
;;;		(aref vector (index+ x 4))
;;;		(aref vector (index+ x 5))
;;;		(aref vector (index+ x 6))
;;;		(aref vector (index+ x 7)))))))
;;;  t)
;;;
;;;#+(or lcl3.0 excl)
;;;(defun fast-write-pixarray-4 (buffer-bbuf index array x y width height
;;;			      padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-4 array)
;;;	   (type int16 x y)
;;;	   (type card16 width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((vector (underlying-simple-vector array))
;;;	  (h 0 (index1+ h))
;;;	  (y y (index1+ y))
;;;	  (right-nibbles (index-mod width 2))
;;;	  (middle-nibbles (index- width right-nibbles))
;;;	  (middle-bytes (index-ceiling middle-nibbles 2))
;;;	  (start index (index+ start padded-bytes-per-line)))
;;;	 ((index>= h height))
;;;      (declare (type (simple-array pixarray-4-element-type (*)) vector)
;;;	       (type array-index h y right-nibbles middle-nibbles
;;;		     middle-bytes start))
;;;      (do* ((end (index+ start middle-bytes))
;;;	    (i start (index1+ i))
;;;	    (start-x x)
;;;	    (x (array-row-major-index array y start-x) (index+ x 2)))
;;;	   ((index>= i end)
;;;	    (unless (index-zerop right-nibbles)
;;;	      (setf (aref buffer-bbuf end)
;;;		    (write-image-assemble-bytes
;;;		      (aref array y (index+ start-x middle-nibbles))
;;;		      0))))
;;;	(declare (type array-index end i start-x x))
;;;	(setf (aref buffer-bbuf i)
;;;	      (write-image-assemble-bytes
;;;		(aref vector (index+ x 0))
;;;		(aref vector (index+ x 1)))))))
;;;  t)
;;;
;;;#+(or lcl3.0 excl)
;;;(defun fast-write-pixarray-8 (buffer-bbuf index array x y width height
;;;			      padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-8 array)
;;;	   (type int16 x y)
;;;	   (type card16 width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((vector (underlying-simple-vector array))
;;;	  (h 0 (index1+ h))
;;;	  (y y (index1+ y))
;;;	  (start index (index+ start padded-bytes-per-line)))
;;;	 ((index>= h height))
;;;      (declare (type (simple-array pixarray-8-element-type (*)) vector)
;;;	       (type array-index h y start))
;;;      (do* ((end (index+ start width))
;;;	    (i start (index1+ i))
;;;	    (x (array-row-major-index array y x) (index1+ x)))
;;;	   ((index>= i end))
;;;	(declare (type array-index end i x))
;;;	(setf (aref buffer-bbuf i) (the card8 (aref vector x))))))
;;;  t)
;;;
;;;#+(or lcl3.0 excl)
;;;(defun fast-write-pixarray-16 (buffer-bbuf index array x y width height
;;;			       padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-16 array)
;;;	   (type int16 x y)
;;;	   (type card16 width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((vector (underlying-simple-vector array))
;;;	  (h 0 (index1+ h))
;;;	  (y y (index1+ y))
;;;	  (start index (index+ start padded-bytes-per-line)))
;;;	 ((index>= h height))
;;;      (declare (type (simple-array pixarray-16-element-type (*)) vector)
;;;	       (type array-index h y start))
;;;      (do* ((end (index+ start (index* width 2)))
;;;	    (i start (index+ i 2))
;;;	    (x (array-row-major-index array y x) (index1+ x)))
;;;	   ((index>= i end))
;;;	(declare (type array-index end i x))
;;;	(let ((pixel (aref vector x)))
;;;	  (declare (type pixarray-16-element-type pixel))
;;;	  (setf (aref buffer-bbuf (index+ i 0))
;;;		(write-image-load-byte 0 pixel 16))
;;;	  (setf (aref buffer-bbuf (index+ i 1))
;;;		(write-image-load-byte 8 pixel 16))))))
;;;  t)
;;;
;;;#+Genera
;;;(defun fast-write-pixarray-24 (buffer-bbuf index array x y width height
;;;			       padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-24 array)
;;;	   (type int16 x y)
;;;	   (type card16 width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((array array)
;;;	  (h 0 (index1+ h))
;;;	  (y y (index1+ y))
;;;	  (start index (index+ start padded-bytes-per-line)))
;;;	 ((index>= h height))
;;;      (declare (sys:array-register-1d array)
;;;	       (type array-index y start))
;;;      (do* ((end (index+ start (index* width 3)))
;;;	    (i start (index+ i 3))
;;;	    (x (array-row-major-index array y x) (index1+ x)))
;;;	   ((index>= i end))
;;;	(declare (type array-index end i x))
;;;	(let ((pixel (sys:%1d-aref array x)))
;;;	  (declare (type pixarray-24-element-type pixel))
;;;	  (setf (aref buffer-bbuf (index+ i 0))
;;;		(write-image-load-byte 0 pixel 24))
;;;	  (setf (aref buffer-bbuf (index+ i 1))
;;;		(write-image-load-byte 8 pixel 24))
;;;	  (setf (aref buffer-bbuf (index+ i 2))
;;;		(write-image-load-byte 16 pixel 24))))))
;;;  t)
;;;
;;;#+(or lcl3.0 excl)
;;;(defun fast-write-pixarray-24 (buffer-bbuf index array x y width height
;;;			       padded-bytes-per-line)
;;;  (declare (type buffer-bytes buffer-bbuf)
;;;	   (type pixarray-24 array)
;;;	   (type int16 x y)
;;;	   (type card16 width height)
;;;	   (type array-index index padded-bytes-per-line))
;;;  #.(declare-buffun)
;;;  (with-vector (buffer-bbuf buffer-bytes)
;;;    (do* ((vector (underlying-simple-vector array))
;;;	  (h 0 (index1+ h))
;;;	  (y y (index1+ y))
;;;	  (start index (index+ start padded-bytes-per-line)))
;;;	 ((index>= h height))
;;;      (declare (type (simple-array pixarray-24-element-type (*)) vector)
;	       (type array-index y start))
;      (do* ((end (index+ start (index* width 3)))
;	    (i start (index+ i 3))
;	    (x (array-row-major-index array y x) (index1+ x)))
;	   ((index>= i end))
;	(declare (type array-index end i x))
;	(let ((pixel (aref vector x)))
;	  (declare (type pixarray-24-element-type pixel))
;	  (setf (aref buffer-bbuf (index+ i 0))
;		(write-image-load-byte 0 pixel 24))
;	  (setf (aref buffer-bbuf (index+ i 1))
;		(write-image-load-byte 8 pixel 24))
;	  (setf (aref buffer-bbuf (index+ i 2))
;		(write-image-load-byte 16 pixel 24))))))
;  t)
;
;#+(or lcl3.0 excl)
;(defun fast-write-pixarray-32 (buffer-bbuf index array x y width height
;			       padded-bytes-per-line)
;  (declare (type buffer-bytes buffer-bbuf)
;	   (type pixarray-32 array)
;	   (type int16 x y)
;	   (type card16 width height)
;	   (type array-index index padded-bytes-per-line))
;  #.(declare-buffun)
;  (with-vector (buffer-bbuf buffer-bytes)
;    (do* ((vector (underlying-simple-vector array))
;	  (h 0 (index1+ h))
;	  (y y (index1+ y))
;	  (start index (index+ start padded-bytes-per-line)))
;	 ((index>= h height))
;      (declare (type (simple-array pixarray-32-element-type (*)) vector)
;	       (type array-index h y start))
;      (do* ((end (index+ start (index* width 4)))
;	    (i start (index+ i 4))
;	    (x (array-row-major-index array y x) (index1+ x)))
;	   ((index>= i end))
;	(declare (type array-index end i x))
;	(let ((pixel (aref vector x)))
;	  (declare (type pixarray-32-element-type pixel))
;	  (setf (aref buffer-bbuf (index+ i 0))
;		(write-image-load-byte 0 pixel 32))
;	  (setf (aref buffer-bbuf (index+ i 1))
;		(write-image-load-byte 8 pixel 32))
;	  (setf (aref buffer-bbuf (index+ i 2))
;		(write-image-load-byte 16 pixel 32))
;	  (setf (aref buffer-bbuf (index+ i 2))
;		(write-image-load-byte 24 pixel 32))))))
;  t)
;
;(defun fast-write-pixarray (bbuf boffset pixarray x y width height
;			    padded-bytes-per-line bits-per-pixel)
;  (declare (type buffer-bytes bbuf)
;	   (type pixarray pixarray)
;	   (type card16 x y width height)
;	   (type array-index boffset padded-bytes-per-line)
;	   (type (member 1 4 8 16 24 32) bits-per-pixel))
;  (progn bbuf boffset pixarray x y width height padded-bytes-per-line
;	 bits-per-pixel)
;  (or
;    #+lispm
;    (let* ((padded-bits-per-line (* padded-bytes-per-line 8))
;	   (padded-pixels-per-line
;	     (floor padded-bits-per-line bits-per-pixel))
;	   (pixarray-padded-pixels-per-line
;	     #+Genera (sys:array-row-span pixarray)
;	     #-Genera (array-dimension pixarray 1))
;	   (pixarray-padded-bits-per-line
;	     (* pixarray-padded-pixels-per-line bits-per-pixel)))
;      (when (and (= (sys:array-element-size pixarray) bits-per-pixel)
;		 (zerop (index-mod padded-bits-per-line 32))
;		 (zerop (index-mod pixarray-padded-bits-per-line 32)))
;	(#+Genera sys:stack-let* #-Genera let*
;	 ((dimensions (list height padded-pixels-per-line))
;	  (a (make-array
;	       dimensions
;	       :element-type (array-element-type pixarray)
;	       :displaced-to bbuf
;	       :displaced-index-offset (floor (* boffset 8) bits-per-pixel))))
;	 (sys:bitblt boole-1 width height pixarray x y a 0 0))
;	t))
;    #+Genera
;    (when (= bits-per-pixel 24)
;      (fast-write-pixarray-24
;	bbuf boffset pixarray x y width height padded-bytes-per-line))
;    #+(or lcl3.0 excl)
;    (funcall
;      (ecase bits-per-pixel 
;	(1 #'fast-write-pixarray-1) (4 #'fast-write-pixarray-4)
;	(8 #'fast-write-pixarray-8) (16 #'fast-write-pixarray-16)
;	(24 #'fast-write-pixarray-24) (32 #'fast-write-pixarray-32))
;      bbuf boffset pixarray x y width height padded-bytes-per-line)
;    ))
;
;;;; FAST-COPY-PIXARRAY - copy part of a pixarray into another
;
;(defun fast-copy-pixarray (pixarray copy x y width height bits-per-pixel)
;  (declare (type pixarray pixarray copy)
;	   (type card16 x y width height)
;	   (type (member 1 4 8 16 24 32) bits-per-pixel))
;  (progn pixarray copy x y width height bits-per-pixel)
;  (or
;    #+lispm
;    (let* ((pixarray-padded-pixels-per-line
;	     #+Genera (sys:array-row-span pixarray)
;	     #-Genera (array-dimension pixarray 1))
;	   (pixarray-padded-bits-per-line
;	     (* pixarray-padded-pixels-per-line bits-per-pixel))
;	   (copy-padded-pixels-per-line
;	     #+Genera (sys:array-row-span copy)
;	     #-Genera (array-dimension copy 1))
;	   (copy-padded-bits-per-line
;	     (* copy-padded-pixels-per-line bits-per-pixel)))
;      (when (and (= (sys:array-element-size pixarray) bits-per-pixel)
;		 (zerop (index-mod pixarray-padded-bits-per-line 32))
;		 (zerop (index-mod copy-padded-bits-per-line 32)))
;	(sys:bitblt boole-1 width height pixarray x y copy 0 0)
;	t))
;    #+Genera
;    (let ((src pixarray)
;	  (dest copy))
;      (declare (sys:array-register-1d src dest))
;      (do* ((dst-y 0 (index1+ dst-y))
;	    (src-y y (index1+ src-y)))
;	   ((index>= dst-y height))
;	(declare (type card16 dst-y src-y))
;	(do* ((dst-idx (array-row-major-index copy dst-y 0)
;		       (index1+ dst-idx))
;	      (dst-end (index+ dst-idx width))
;	      (src-idx (array-row-major-index pixarray src-y x)
;		       (index1+ src-idx)))
;	     ((index>= dst-idx dst-end))
;	  (declare (type array-index dst-idx src-idx dst-end))
;	  (setf (sys:%1d-aref dest dst-idx)
;		(sys:%1d-aref src src-idx))))
;      t)
;    #+(or lcl3.0 excl)
;    (macrolet
;      ((copy (type element-type)
;	 `(let* ((pixarray pixarray)
;		 (copy copy)
;		 (src (underlying-simple-vector pixarray))
;		 (dst (underlying-simple-vector copy)))
;	    (declare (type ,type pixarray copy)
;		     (type (simple-array ,element-type (*)) src dst))
;	    #.(declare-buffun)
;	    (do* ((dst-y 0 (index1+ dst-y))
;		  (src-y y (index1+ src-y)))
;		 ((index>= dst-y height))
;	      (declare (type card16 dst-y src-y))
;	      (do* ((dst-idx (array-row-major-index copy dst-y 0)
;			     (index1+ dst-idx))
;		    (dst-end (index+ dst-idx width))
;		    (src-idx (array-row-major-index pixarray src-y x)
;			     (index1+ src-idx)))
;		   ((index>= dst-idx dst-end))
;		(declare (type array-index dst-idx src-idx dst-end))
;		(setf (aref dst dst-idx)
;		      (the ,element-type (aref src src-idx))))))))
;      (ecase bits-per-pixel
;	(1  (copy pixarray-1  pixarray-1-element-type))
;	(4  (copy pixarray-4  pixarray-4-element-type))
;	(8  (copy pixarray-8  pixarray-8-element-type))
;	(16 (copy pixarray-16 pixarray-16-element-type))
;	(24 (copy pixarray-24 pixarray-24-element-type))
;	(32 (copy pixarray-32 pixarray-32-element-type)))
;      t)))




