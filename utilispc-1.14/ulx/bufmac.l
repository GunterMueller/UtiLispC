;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; This file contains macro definitions for the BUFFER object for Common-Lisp
;;; X windows version 11

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

;;; The read- macros are in buffer.lisp, because event-case depends on (most of) them.

(defmacro write-card8 (byte-index item)
  `(aset-card8 ,item buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro write-int8 (byte-index item)
  `(aset-int8 ,item buffer-bbuf (index+ buffer-boffset ,byte-index)))

(defmacro write-card16 (byte-index item)
  `(aset-card16 ,item buffer-bbuf
		(index+ buffer-boffset ,byte-index)))

(defmacro write-int16 (byte-index item)
  `(aset-int16 ,item buffer-bbuf
	       (index+ buffer-boffset ,byte-index)))

(defmacro write-card32 (byte-index item)
  `(aset-card32 ,item  buffer-bbuf
		(index+ buffer-boffset ,byte-index)))

(defmacro write-int32 (byte-index item)
  `(aset-int32 ,item buffer-bbuf
	       (index+ buffer-boffset ,byte-index)))

(defmacro write-card29 (byte-index item)
  `(aset-card29 ,item buffer-bbuf
		(index+ buffer-boffset ,byte-index)))

(defmacro set-buffer-offset (value)
  `(setq buffer-boffset ,value))

(defmacro advance-buffer-offset (value)
  `(set-buffer-offset (index+ buffer-boffset ,value)))

;;; ignoring :sizes key.
;;; (I assume sizes is needed only when overlapping-arrays are used.
(defmacro with-buffer-output ((buffer . keys) . body)
  (let ((*x* (gensym)))
    (setplist *x* keys)
    (let ((length (get *x* ':length))
	  (index (get *x* ':index)))
      `(let ((%buffer ,buffer))
	 ,(when length
	    `(when (index>= (index+ (buffer-boffset %buffer) ,length) (buffer-size %buffer))
	       (buffer-flush %buffer)))
	 (lets ((buffer-boffset ,(or index `(buffer-boffset %buffer)))
		(buffer-bbuf (buffer-obuf8 %buffer)))
	   buffer-boffset		; I dont know why these are here.
	   buffer-bbuf
	   . ,body)))))

;;; This macro is just used internally in buffer

(defmacro writing-buffer-chunks (type args decls . body)
  (when (> (length body) 2)
    (error "writing-buffer-chunks called with too many forms"))
  (lets ((size (* 8 (index-increment type)))
	 (form (first body)))
    `(with-buffer-output (buffer :index boffset)
       (do* ,(append args
		     `((len ,(if (= size 8)
				`(index- end start)
			      `(index-ash (index- end start) ,(// size 16)))
			   (index- len chunk))
		     ;; Number of bytes available in buffer
		     (chunk (index-min len (index- (buffer-size buffer) buffer-boffset))
			    (index-min len (index- (buffer-size buffer) buffer-boffset)))))
	    ((not (index-plusp len)))
	    ,form
	    (index-incf buffer-boffset chunk)
	    ;; Flush the buffer
	    (when (and (index-plusp len) (index>= buffer-boffset (buffer-size buffer)))
	      (setl (buffer-boffset buffer) buffer-boffset)
	      (buffer-flush buffer)
	      (setq buffer-boffset (buffer-boffset buffer))))
       (setl (buffer-boffset buffer) (lround buffer-boffset)))))
