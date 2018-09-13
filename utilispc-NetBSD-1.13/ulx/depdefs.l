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

(defmacro with-vector ((var type) . body)
  `(progn . ,body))

;;; WITHIN-DEFINITION (name type) &body body --- Includes definitions for
;;; Meta-.

;#+utilisp
(defmacro within-definition ((name type) . body)
  `(progn . ,body))

;;;-------------------------------------------------------------------------
;;; CLX can maintain a mapping from X server ID's to local data types.  If
;;; one takes the view that CLX objects will be instance variables of
;;; objects at the next higher level, then PROCESS-EVENT will typically map
;;; from resource-id to higher-level object.  In that case, the lower-level
;;; CLX mapping will almost never be used (except in rare cases like
;;; query-tree), and only serve to consume space (which is difficult to
;;; GC), in which case always-consing versions of the make-<mumble>s will
;;; be better.  Even when maps are maintained, it isn't clear they are
;;; useful for much beyond xatoms and windows (since almost nothing else
;;; ever comes back in events).
;;;--------------------------------------------------------------------------
(defconstant *clx-cached-types*
	     '( drawable
		window
		pixmap
;		gcontext
		cursor
		colormap
		font
		xatom))

; toriaezu no defmacro
(defmacro resource-id-map-test () '(function eq))
(defmacro atom-cache-test () '(function eq))
(defmacro keysym->character-map-test () '(function eq))
  
;;; You must define this to match the real byte order.  It is used by
;;; overlapping array and image code.

;;; Steele's Common-Lisp states:  "It is an error if the array specified
;;; as the :displaced-to argument  does not have the same :element-type
;;; as the array being created" If this is the case on your lisp, then
;;; leave the overlapping-arrays feature turned off.  Lisp machines
;;; (Symbolics TI and LMI) don't have this restriction, and allow arrays
;;; with different element types to overlap.  CLX will take advantage of
;;; this to do fast array packing/unpacking when the overlapping-arrays
;;; feature is enabled.

;;; This defines a type which is a subtype of the integers.
;;; This type is used to describe all variables that can be array indices.
;;; It is here because it is used below.
;;; This is inclusive because start/end can be 1 past the end.
;(deftype array-index () `(integer 0 ,array-dimension-limit))

(macro index+ (numbers) `(+ . ,numbers))
(macro index-logand (numbers) `(logand . ,numbers))
(macro index-logior (numbers) `(logior . ,numbers))
(macro index- (numbers) `(- . ,numbers))
(macro index* (numbers) `(* . ,numbers))

(defmacro index1+ (number) `(1+ ,number))
(defmacro index1- (number) `(1- ,number))

(defmacro index-incf (place . delta)
  `(setl ,place (index+ ,place ,(or (and delta (car delta)) 1))))
(defmacro index-decf (place . delta)
  `(setl ,place (index- ,place ,(or (and delta (car delta)) 1))))

(macro index-min (numbers) `(min . ,numbers))
(macro index-max (numbers) `(max . ,numbers))

;; index-floor to index-ceiling ha number < 0 deha usono atai wo kaesu.

(defmacro index-floor (number divisor)
  (cond ((eq divisor 1) number)
	(t `(// ,number ,divisor))))

(defmacro index-ceiling (number divisor)
  (cond ((eq divisor 1) number)
	(t `(// (+ ,number (/1- ,divisor)) ,divisor))))

;;; truncate ha tukawarete inai node pass.
;(defmacro index-truncate (number divisor)
;  (cond ((eql divisor 1) number)
;	((and (positive-power-of-two-p divisor) (fboundp 'si:%fixnum-floor))
;	 `(si:%fixnum-floor ,number ,divisor))
;	(t `(truncate ,number ,divisor))))

;;; imaha pass.
;(defmacro index-mod (number divisor)
;  (cond ((and (positive-power-of-two-p divisor) (fboundp 'si:%fixnum-mod))
;	 `(si:%fixnum-mod ,number ,divisor))
;	(t `(mod ,number ,divisor))))
(defmacro index-mod (number divisor) `(\ ,number ,divisor))
  
(defmacro index-ash (number count)
  (cond ((eq count 0) number)
	(t `(logshift ,number ,count))))

; konomama de ok.
(defmacro index-plusp (number) `(plusp ,number))
(defmacro index-zerop (number) `(zerop ,number))
(defmacro index-evenp (number) `(evenp ,number))
(defmacro index-oddp  (number) `(oddp  ,number))

(macro index> (numbers) `(> . ,numbers))
(macro index= (numbers) `(= . ,numbers))
(macro index< (numbers) `(< . ,numbers))
(macro index>= (numbers) `(>= . ,numbers))
(macro index<= (numbers) `(<= . ,numbers))

;;;; Stuff for BUFFER definition

(defconstant *replysize* 32)

;; used in defstruct initializations to avoid compiler warnings
;(defvar *empty-bytes* (make-sequence 'buffer-bytes 0))
(setq *empty-bytes* 0)
;(proclaim '(type buffer-bytes *empty-bytes*))
;#+clx-overlapping-arrays
;(progn
;(defvar *empty-words* (make-sequence 'buffer-words 0))
;(proclaim '(type buffer-words *empty-words*))
;)
;#+clx-overlapping-arrays
;(progn
;(defvar *empty-longs* (make-sequence 'buffer-longs 0))
;(proclaim '(type buffer-longs *empty-longs*))
;)

(defstruct (reply-buffer (:conc-name reply-) (:constructor make-reply-buffer-internal)
			 (:copier nil) (:predicate nil))
  (size 0 :type array-index)			;Buffer size
  ;; Byte (8 bit) input buffer
  (ibuf8 *empty-bytes* :type buffer-bytes)
  (next nil :type (or null reply-buffer))
  (data-size 0 :type array-index)
  )

(defconstant *buffer-text16-size* 256)

;(defun xintern (&rest parts)
;  (intern (apply #'concatenate 'string (mapcar #'string parts)) *xlib-package*))

(macro xintern (parts)
  `(intern (apply (function string-append)
		  (mapcar (list . ,parts)
			  (function (lambda (x) (or (and (key-p x) (unkeyword x))
						    x)))))))

;(defun kintern (name)
;  (intern (string name) *keyword-package*))
;; fake.
(defun kintern (name)
  (intern
   (cond ((key-p name) name)
	 (t (string-append ":" name)))))

;;; Pseudo-class mechanism.

(defmacro def-ulx-class ((name . options) . slots)
  `(defstruct (,name . ,options) . ,slots))

;; We need this here so we can define DISPLAY for CLX.
;;
;; This structure is :INCLUDEd in the DISPLAY structure.
;; Overlapping (displaced) arrays are provided for byte
;; half-word and word access on both input and output.
;;

(def-ulx-class (buffer (:constructor nil) (:copier nil) (:predicate nil))
  ;; Lock for multi-processing systems
  ;(lock (make-process-lock "CLX Buffer Lock"))
  (lock)
  (output-stream -1 :type fixnum)
  ;; Buffer size
  (size 0 :type array-index)
  (request-number 0 :type (unsigned-byte 16))
  ;; Byte position of start of last request
  ;; used for appending requests and error recovery
  (last-request nil :type (or null array-index))
  ;; Byte position of start of last flushed request
  (last-flushed-request nil :type (or null array-index))
  ;; Current byte offset
  (boffset 0 :type array-index)
  ;; Byte (8 bit) output buffer
  (obuf8 *empty-bytes* :type buffer-bytes)
  ;; Holding buffer for 16-bit text
  ;(tbuf16 (make-sequence 'buffer-text16 *buffer-text16-size* :initial-element 0))
  ;(tbuf16 (vector *buffer-text16-size* 0))
  (tbuf16 (make-string (* 2 *buffer-text16-size*) 0))
  ;; Probably EQ to Output-Stream
  (input-stream -1)
  ;; T when the host connection has gotten errors
  (dead nil :type (or null (not null)))
  ;; T makes buffer-flush a noop.  Manipulated with with-buffer-flush-inhibited.
  (flush-inhibit nil :type (or null (not null)))
  
  ;; Change these functions when using shared memory buffers to the server
  ;; Function to call when writing the buffer
  (write-function 'buffer-write-default)
  ;; Function to call when flushing the buffer
  (force-output-function 'buffer-force-output-default)
  ;; Function to call when closing a connection
  (close-function 'buffer-close-default)
  ;; Function to call when reading the buffer
  (input-function 'buffer-read-default)
  ;; Function to call to wait for data to be input
  (input-wait-function 'buffer-input-wait-default)
  ;; Function to call to listen for input data
  (listen-function 'buffer-listen-default)

  )
;;-----------------------------------------------------------------------------
;; Printing routines.
;;-----------------------------------------------------------------------------

;#-lispm
;(defmacro printing-object ((object stream) &body body)
;  `(let ((.object. ,object)
;	 (.stream. ,stream))
;     (princ "#<" .stream.)
;     (prin1 (type-of .object.) .stream.)
;     ,@(when body
;	 `((princ " " .stream.)
;	   ,@body))
;     (princ ">" .stream.)))

;#+lispm
;(defmacro printing-object ((object stream) &body body)
;  `(si:printing-random-object (,object ,stream :typep :no-pointer)
;     ,@body))

;
;(defmacro type-of (o) `(get ,o 'type))
(defmacro type-of (o) `(vref ,o 0))
;#+utilisp
(defmacro printing-object ((object stream) . body)
  `(let ((/.object. ,object)
	 (/.stream. ,stream))
     (princ "#<" /.stream.)
     (prin1 (type-of /.object.) /.stream.)
     ,@(when body
	 `((princ " " /.stream.)
	   ,@body))
     (princ ">" /.stream.)))
	 
;;-----------------------------------------------------------------------------
;; Image stuff
;;-----------------------------------------------------------------------------

(defconstant *image-bit-lsb-first-p* nil)
;	     #+clx-little-endian t
;	     #-clx-little-endian nil)

(defconstant *image-byte-lsb-first-p* nil)
;	     #+clx-little-endian t
;	     #-clx-little-endian nil)

;(defconstant *image-unit* 32)

;(defconstant *image-pad* 32)
