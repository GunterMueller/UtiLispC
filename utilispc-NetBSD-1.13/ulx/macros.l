;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

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

;;; CLX basicly implements a very low overhead remote procedure call
;;; to the server.  This file contains macros which generate the code
;;; for both the client AND the server, given a specification of the
;;; interface. This was done to eliminate errors that may occur because
;;; the client and server code get/put bytes in different places, and
;;; it makes it easier to extend the protocol.

;;; This is built on top of BUFFER
(defmacro required-arg (name)
  `(progn (x-error 'missing-parameter :parameter ',name)
	  *required-arg-dummy*))

(defmacro lround (index)
  `(logand (index+ ,index 3) -4))

(defmacro wround (index)
  `(logand (index+ ,index 1) -2))

;;
;; Data-type accessor functions
;;
;;   These functions translate between lisp data-types and the byte,
;;   half-word or word that gets transmitted across the client/server
;;   connection

(defun index-increment (type)
  (lets ((name (if (consp type)(car type) type))
	 (increment (get name 'byte-width)))
    (or increment
	(progn
	  (or (memq 'byte-width (plist name))
	      (format "unknown field width for '/s'/n" name))
	  nil))))

(defun putify (name)
  (intern (string-append name "-put")))

(defun getify (name)
  (intern (string-append name "-get")))

;; does no type check
(defmacro define-accessor (name (width) . get-put-macros)
  (let ((get-macro (first get-put-macros))
	(put-macro (second get-put-macros)))
    `(progn
       (setf (get ',name 'byte-width) ,(and width (floor width 8)))
       (defmacro ,(getify name) ,(car get-macro) . ,(cdr get-macro))
       (defmacro ,(putify name) ,(car put-macro) . ,(cdr put-macro))
       )))
	 
(define-accessor card32 (32)
  ((index) `(read-card32 ,index))
  ((index thing) `(write-card32 ,index ,thing)))

(define-accessor card29 (32)
  ((index) `(read-card29 ,index))
  ((index thing) `(write-card29 ,index ,thing)))

(define-accessor card16 (16)
  ((index) `(read-card16 ,index))
  ((index thing) `(write-card16 ,index ,thing)))

(define-accessor card8 (8)
  ((index) `(read-card8 ,index))
  ((index thing) `(write-card8 ,index ,thing)))

(define-accessor integer (32)
  ((index) `(read-int32 ,index))
  ((index thing) `(write-int32 ,index ,thing)))

(define-accessor int16 (16)
  ((index) `(read-int16 ,index))
  ((index thing) `(write-int16 ,index ,thing)))

(define-accessor rgb-val (16)
  ;; Used for color's
  ((index) `(card16->rgb-val (read-card16 ,index)))
  ((index thing) `(write-card16 ,index (rgb-val->card16 ,thing))))

(define-accessor angle (16)
  ;; Used for drawing arcs
  ((index) `(int16->radians (read-int16 ,index)))
  ((index thing) `(write-int16 ,index (radians->int16 ,thing))))

(define-accessor bit (0)
  ;; Like BOOLEAN, but tests bits
  ;; only used by declare-event (:enter-notify :leave-notify)
  ((index bit)
   `(logbitp ,bit (read-card8 ,index)))
  ((index thing bit)
   (if (zerop bit)
       `(write-card8 ,index (if ,thing 1 0))
     `(write-card8 ,index
		   (if thing
		       (logand (lognot (logshift 1 ,bit)) (read-card8 ,index))
		     (logor (logshift 1 ,bit) (read-card8 ,index)))))))

(define-accessor boolean (8)
  ((index)
   `(plusp (read-card8 ,index)))
  ((index thing) `(write-card8 ,index (if ,thing 1 0))))

;;;
;;; 32 bit values are passed in the form of cons of 2 integers.
;;;
(define-accessor drawable (32)
  ((index . buffer)
   `(lookup-drawable ,(or (and buffer (car buffer)) '%buffer)
		     (read-card29 ,index)))
  ((index thing) `(write-card29 ,index (drawable-id ,thing))))

(define-accessor window (32)
  ((index . buffer)
   `(lookup-window ,(or (and buffer (car buffer)) '%buffer) (read-card29 ,index)))
  ((index thing) `(write-card29 ,index (window-id ,thing))))

(define-accessor pixmap (32)
  ((index . buffer)
   `(lookup-window ,(or (and buffer (car buffer) '%buffer)) (read-card29 ,index)))
  ((index thing) `(write-card29 ,index (pixmap-id ,thing))))
   
(define-accessor gcontext (32)
  ((index . buffer)
   `(lookup-gcontext ,(or (and buffer (car buffer) '%buffer)) (read-card29 ,index)))
  ((index thing) `(write-card29 ,index (gcontext-id ,thing))))

(define-accessor cursor (32)
  ((index . buffer)
   `(lookup-cursor ,(or (and buffer (car buffer) '%buffer)) (read-card29 ,index)))
  ((index thing) `(write-card29 ,index (cursor-id ,thing))))

(define-accessor colormap (32)
  ((index . buffer)
   `(lookup-colormap ,(or (and buffer (car buffer) '%buffer)) (read-card29 ,index)))
  ((index thing) `(write-card29 ,index (colormap-id ,thing))))

(define-accessor font (32)
  ((index . buffer)
   `(lookup-font ,(or (and buffer (car buffer) '%buffer)) (read-card29 ,index)))
  ;; The FONT-ID accessor may make a OpenFont request.  Since we don't support recursive
  ;; with-buffer-request, issue a compile time error, rather than barf at run-time.
  ((index thing)
;   (declare (ignore index thing))
   (error "FONT-ID must be called OUTSIDE with-buffer-request.  Use RESOURCE-ID instead.")))

;; Needed to get and put xatom's in events
(define-accessor keyword (32)
  ((index . buffer)
   `(lookup-xatom ,(or (and buffer (car buffer)) '%buffer) (read-card29 ,index)))
  ((index thing . buffer)
   (let ((buf (and buffer (eq (car buffer) ':buffer) (second buffer))))
     `(write-card29 ,index (or (atom-id ,thing ,(or buf '%buffer))
			       (error "CLX implementation error in KEYWORD-PUT"))))))

(define-accessor resource-id (32)
  ((index) `(read-card29 ,index))
  ((index thing) `(write-card29 ,index ,thing)))

(define-accessor resource-id-or-nil (32)
  ((index) (let ((id (gensym)))
	     `(let ((,id (read-card29 ,index)))
		(and (plusp32 ,id) ,id))))
  ((index thing) `(write-card29 ,index (or ,thing 0))))

(defmacro char-info-get (index)
  `(make-char-info
     :left-bearing (int16-get ,index)
     :right-bearing (int16-get ,(+ index 2))
     :width	   (int16-get ,(+ index 4))
     :ascent	   (int16-get ,(+ index 6))
     :descent	   (int16-get ,(+ index 8))
     :attributes   (card16-get ,(+ index 10))))

;(define-accessor member8 (8)
;  ((index . keywords)
;   (let ((value (gensym)))
;     `(let ((,value (read-card8 ,index)))
;	(and (< ,value ,(length keywords))
;	     (vref ,(vector (length keywords) keywords) ,value)))))
;  ((index thing . keywords)
;   `(write-card8 ,index (position ,thing ,(vector (length keywords) keywords)))))

;;; utilisp compiler cannot dump vectors...
;;; position can handle both list and vector.
(define-accessor member8 (8)
  ((index . keywords)
   (let ((value (gensym)))
     `(let ((,value (read-card8 ,index)))
	(and (< ,value ,(length keywords))
	     (nth ,value ',keywords)))))
  ((index thing . keywords)
   `(write-card8 ,index (position ,thing ',keywords))))

;; vector wo umekomu no ha compiler ga kyohi suru.
(define-accessor member16 (16)
  ((index . keywords)
   (let ((value (gensym)))
     `(let ((,value (read-card16 ,index)))
	(and (< ,value ,(length keywords))
	     ;;(vref ,(vector (length keywords) keywords) ,value)
	     (nth ,value ',keywords)
	     ))))
  ((index thing . keywords)
   `(write-card16 ,index (position ,thing ',keywords))))

;;; mada chanto sita implementation deha nai.
;;; member no kazu ha big ni natte simawanai kotowo inoru (maa futuu soudaro).
(define-accessor member (32)
  ((index . keywords)
   (let ((value (gensym)))
     `(let ((,value (card32->big (read-card29 ,index))))
	(and (lessp ,value ,(length keywords))
	     ;;(vref ,(vector (length keywords) keywords) ,value)
	     (nth ,value ',keywords)
	     ))))
  ((index thing . keywords)
   `(write-card29 ,index (position ,thing ',keywords))))

;;(deftype member-vector (vector) `(member ,@(coerce (symbol-value vector) 'list)))

(define-accessor member-vector (32)
  ((index membership-vector)
   (let ((value (gensym)) (vec (gensym)))
     `(let ((,value (card32->big (read-card29 ,index)))
	    (,vec membership-vector))
	(and (lessp ,value (vector-length ,vec))
	     (vref ,vec ,value)))))
  ((index thing membership-vector)
   `(write-card29 ,index (position ,thing ,membership-vector))))

(define-accessor member16-vector (16)
  ((index membership-vector)
   (let ((value (gensym)) (vec (gensym)))
     `(let ((,value (read-card16 ,index))
	    (,vec membership-vector))
	(and (< ,value (vector-length ,vec))
	     (vref ,vec ,value)))))
  ((index thing membership-vector)
   `(write-card16 ,index (position ,thing ,membership-vector))))

(define-accessor member8-vector (8)
  ((index membership-vector)
   (let ((value (gensym)) (vec (gensym)))
     `(let ((,value (read-card8 ,index))
	    (,vec membership-vector))
	(and (< ,value (vector-length ,vec))
	     (vref ,vec ,value)))))
  ((index thing membership-vector)
   `(write-card8 ,index (position ,thing ,membership-vector))))

;;; utilisp no baai eq de iinode member-vector ni sitemo yoi.
(define-accessor boole-constant (32)
  ;; this isn't member-vector because we need eql instead of eq
  ((index)
   (let ((value (gensym)))
     `(let ((,value (card32->big (read-card29 ,index))))
	(and (< ,value ,(vector-length *boole-vector*))
	     (vref *boole-vector* ,value)))))
  ((index thing)
   `(write-card29 ,index (position ,thing *boole-vector*))))

(define-accessor null (32)
  ((index) `(if (zerop32 (read-card32 ,index)) nil (read-card32 ,index)))
  ((index value) `(write-card32 ,index 0)))

(define-accessor pad8 (8)
  ((index) nil)
  ((index value) nil))

(define-accessor pad16 (16)
  ((index) nil)
  ((index value) nil))

(define-accessor bit-vector256 (256)
  ((index . optargs)			; (index &optional (real-index index) data)
   (lets ((real-index (or (and optargs (pop optargs)) index))
	  (data (and optargs (pop optargs))))
     `(read-bitvector256 buffer-bbuf ,real-index ,data)))
  ((index map . optargs)		; (index map &optional (real-index index) data)
   (lets ((real-index (or (and optargs (pop optargs)) index))
	  (buffer (or (and optargs (pop optargs)) '%buffer)))
     `(write-bitvector256 ,buffer (index+ buffer-boffset ,real-index) ,map))))
   
(define-accessor string (nil)
  ;; put macro
  ((length index . key)
   (let ((reply-buffer (and key (eq (car key) ':reply-buffer) (second key))))
     `(read-sequence-char
       ,(or reply-buffer '%reply-buffer) 'string ,length nil nil 0 ,index)))
  ;; get macro
  ((index string . keys)
   (let ((x (gensym)))
     (setplist x keys)
     (lets ((buffer (or (get x ':buffer) '%buffer))
	    (start (or (get x ':start) 0))
	    (end (or (get x ':end) `(string-length ,string)))
	    (header-length (or (get x ':header-length) (lround index)))
	    (appending (get x ':appending))
	    
	    (real-end (if appending end (gensym)))
	    (form `(write-sequence-char ,buffer
					(index+ buffer-boffset ,header-length)
					,string ,start ,real-end)))
       (if appending
	   form
	 ;; every string data has its length (in card16) before it.
	 `(let ((,real-end ,end))
	    (write-card16 2 (index-ceiling (index+ (index- ,real-end ,start)
						   ,header-length)
					   4))
	    ,form)))))))
		  
;;; sequence-get ha argument subetega key nanode, define-accessor de teigi suru
;;; kotoga dekinainode, kounatta.

(setf (get 'sequence 'byte-width) nil)

(macro sequence-get (keys)
  (lets ((x (gensym)))
    (setplist x keys)
    (lets ((length (get x ':length))
	   (format (or (get x ':format) 'card32))
	   (result-type (get x ':result-type))
	   (transform (get x ':transform))
	   (reply-buffer (get x ':reply-buffer))
	   (data (get x ':data))
	   (index (get x ':index))
	   (start (get x ':start)))
      (append `(,(selectq format
		   (card8 'read-sequence-card8)
		   (int8 'read-sequence-int8)
		   (card16 'read-sequence-card16)
		   (int16 'read-sequence-int16)
		   (card32 'read-sequence-card32)
		   (int32 'read-sequence-int32))
		,(or reply-buffer '%reply-buffer)
		,result-type ,length ,transform ,data
		)
	      (when (or start index) `(,(or start 0)))
	      (when index `(,index))))))

(defmacro sequence-put (index data . keys)
  (lets ((x (gensym)))
    (setplist x keys)
    (lets ((format (or (get x ':format) 'card32))
	   (start (or (get x ':start) 0))
	   (end (get x ':end))
	   (transform (get x ':transform))
	   (buffer (get x ':buffer))
	   (appending (get x ':appending)))
      (unless buffer (setq buffer '%buffer))
      (lets ((real-end (if appending (or end `(length ,data)) (gensym)))
	     (writer (xintern 'write-sequence- format))
	     (form `(,writer ,buffer (index+ buffer-boffset ,(lround index))
			     ,data ,start ,real-end ,transform)))
	(lets ((maker (function (lambda (size)
				   (if appending
				       form
				     (let ((idx `(index- ,real-end ,start)))
				       (unless (= size 1)
					 (setq idx `(index-ceiling ,idx ,size)))
				       `(let ((,real-end ,(or end `(length ,data))))
					  (write-card16 2 (index+ ,idx ,(index-ceiling index 4)))
					  ,form)))))))
	  (selectq format
	    ((card8 int8) (funcall maker 4))
	    ((card16 int16) (funcall maker 2))
	    ((card32 int32) (funcall maker 1))))))))

(defmacro client-message-event-get-sequence ()
  '(let* ((format (read-card8 1))
 	  (sequence (make-array (ceiling 160 format)
				:element-type `(unsigned-byte ,format))))
     (do ((i 12)
	  (j 0 (1+ j)))
	 ((>= i 32))
       (case format
	 (8 (setf (aref sequence j) (read-card8 i))
	    (incf i))
	 (16 (setf (aref sequence j) (read-card16 i))
	     (incf i 2))
	 (32 (setf (aref sequence j) (read-card32 i))
	     (incf i 4))))
     sequence))

(defmacro client-message-event-put-sequence (format sequence)
  `(ecase ,format
     (8  (sequence-put 12 ,sequence
		       :format card8
		       :end (min (length ,sequence) 20)
		       :appending t))
     (16 (sequence-put 12 ,sequence
		       :format card16
		       :end (min (length ,sequence) 10)
		       :appending t))
     (32 (sequence-put 12 ,sequence
		       :format card32
		       :end (min (length ,sequence) 5)
		       :appending t))))

;; Used only in declare-event
(define-accessor client-message-sequence (160)
  ((index format) `(client-message-event-get-sequence))
  ((index value format)
   `(client-message-event-put-sequence ,format ,value)))

;;;
;;; Compound accessors
;;;    Accessors that take other accessors as parameters
;;;
(define-accessor code (0)
  ((index) '(read-card8 0))
  ((index value) `(write-card8 0 ,value))
  ((index value) `(write-card8 0 ,value)))

  
(define-accessor length (0)
  ((index) '(read-card16 2))
  ((index value) `(write-card16 2 ,value))
  ((index value) `(write-card16 2 ,value)))

(deftype data () 'card8)

(define-accessor data (0)
  ;; Put data in byte 1 of the reqeust
  ((index . stuff)
   (and stuff (setq stuff (car stuff)))
   (if stuff
       (if (consp stuff)
	   `(,(getify (car stuff)) 1 . ,(cdr stuff))
	 `(,(getify stuff) 1))
     `(read-card8 1)))

  ((index thing . stuff)
   (and stuff (setq stuff (car stuff)))
   (if stuff
       (if (consp stuff)
	   `(macrolet ((write-card32 (index value) index value))
	      (write-card8 1 (,(putify (car stuff)) ,index ,thing . ,(cdr stuff))))
	 `(,(putify stuff) 1 ,thing))
     `(write-card8 1 ,thing)))

  ((index thing . stuff)
   (and stuff (setq stuff (car stuff)))
   (if stuff
       `(and (type? ,thing ',stuff)
	     ,(if (consp stuff)
		  `(macrolet ((write-card32 (index value) index value))
		     (write-card8 1 (,(putify (car stuff)) ,index ,thing . ,(cdr stuff))))
		`(,(putify stuff) 1 ,thing)))
     `(and (type? ,thing 'card8) (write-card8 1 ,thing)))))

;; Macroexpand the result of OR-GET to allow the macros file to not be loaded
;; when using event-case.  This is pretty gross.

;(defmacro or-expand (&rest forms &environment environment)
;  `(cond ,@(cl:mapcar #'(lambda (forms)
;		       (cl:mapcar #'(lambda (form)
;				   (macroexpand form environment))
;			       forms))
;		   forms)))

;;
;; the OR type
;;
;(define-accessor or (32)
;  ;; Select from among several types (usually NULL and something else)
;  ((index &rest type-list &environment environment)
;   (do ((types type-list (cdr types))
;	(value (gensym))
;	(result nil))
;       ((atom types)
;	`(let ((,value (read-card32 ,index)))
;	   (macrolet ((read-card32 (index) index ',value)
;		      (read-card29 (index) index ',value))
;	     ,(macroexpand `(or-expand ,@(nreverse result)) environment))))
;     (let ((item (car types))
;	   (args nil))
;       (when (consp item)
;	 (setq args (cdr item)
;	       item (car item)))
;       (if (eq item 'null)  ;; Special case for NULL
;	   (push `((zerop ,value) nil) result)
;	 (push
;	   `((,(getify item) ,index ,@args))
;	   result)))))
;
;  ((index value &rest type-list)
;   (do ((types type-list (cdr types))
;	(result))
;       ((endp types)
;	`(cond ,@(nreverse result)
;	       ,@(when *type-check?*
;		   `((t (x-type-error ,value '(or ,@type-list)))))))
;     (let* ((type (car types))
;	    (type-name type)
;	    (args nil))
;       (when (consp type)
;	 (setq args (cdr type)
;	       type-name (car type)))
;       (push
;	 `(,@(cond ((get type-name 'predicating-put) nil)
;		   ((or *type-check?* (cdr types)) `((type? ,value ',type)))
;		   (t '(t)))
;	   (,(putify type-name (get type-name 'predicating-put)) ,index ,value ,@args))
;	 result)))))

(define-accessor or (32)
  ;; Select from among several types (usually NULL and something else)
  ((index . type-list)
   (do ((types type-list (cdr types))
	(value (gensym))
	(result nil))
       ((atom types)
	`(let ((,value (read-card32 ,index)))
	   (macrolet ((read-card32 (index) index ',value)
		      (read-card29 (index) index ',value))
	     (cond . ,(nreverse result)))))
       (let ((item (car types))
	     (args nil))
	 (when (consp item)
	   (setq args (cdr item)
		 item (car item)))
	 (cond ((eq item 'null)
		(push `((zerop32 ,value) nil) result))
	       (t
		(push `((,(getify item) ,index . ,args)) result)))
	 )))
  ((index value . type-list)
   (do ((types type-list (cdr types))
	(result nil))
       ((atom types)
	`(cond . ,(nreverse result)))
       (let* ((type (car types))
	      (type-name type)
	      (args nil))
	 (when (consp type)
	   (setq args (cdr type)
		 type-name (car type)))
	 (push
	  `(,(cond ((cdr types) `(type? ,value ',type))
		   (t t))
	    (,(putify type-name) ,index ,value . ,args))
	  result)))))

;;
;; the MASK type...
;;     is used to specify a subset of a collection of "optional" arguments.
;;     A mask type consists of a 32 bit mask word followed by a word for each one-bit
;;     in the mask.  The MASK type is ALWAYS the LAST item in a request.
;;
(setf (get 'mask 'byte-width) nil)

(defun mask-get (index type-values b-f)
  ;;  (comment declare (type function body-function)
  ;;	   (downward-funarg body-function))
  ;; This is a function, because it must return more than one form (called by get-put-items)
  ;; Functions that use this must have a binding for %MASK
  (let* ((bit 0)
	 (result
	  (cl:mapcar
	   #'(lambda (form)
	       (if (atom form)
		   form;; Hack to allow BODY-FUNCTION to return keyword/value pairs
		 (prog1
		     `(when (logbitp32 ,bit %mask)
			;; Execute form when bit is set
			,form)
		   (incf bit))))
	   (car ;; get-put-items returns multiple values.
	    (get-put-items
	     (+ index 4) type-values nil
	     #'(lambda (type index item args)
		 (funcall b-f type '(* (incf %index) 4) item args)))))))
    ;; First form must load %MASK
    `(,@(when (atom (car result))
	  (list (pop result)))
	(progn (setq %mask (read-card32 ,index))
	       (setq %index ,(ceiling index 4))
	       ,(car result))
	,@(cdr result))))

;; MASK-PUT 

;(defun mask-put (index type-values b-f)
;  ;;(declare (type function body-function)
;  ;;(downward-funarg body-function))
;  ;; The MASK type writes a 32 bit mask with 1 bits for each non-nil value in TYPE-VALUES
;  ;; A 32 bit value follows for each non-nil value.
;  `((let ((%mask (fix32 0))
;	  (%index ,index))
;      . ,(append 
;	  (car ;; get-put-items returns multiple value.
;	   (let ((bit (fix32 1)))
;	     (get-put-items
;	      index type-values t 
;	      #'(lambda (type index item args)
;		  ;;(declare (ignore index))
;		  (if (or (symbolp item) (constantp item))
;		      `((unless (null ,item)
;			  (setq %mask
;				(logor32 %mask
;					 ',(prog1 bit
;					    (setq bit (logshift32 bit 1)))))
;			  . ,(funcall b-f type
;				      `(index-incf %index 4) item args)))
;		    `((let ((%item% ,item))
;			(unless (null %item%)
;			  (setq %mask
;				(logior %mask
;					',(prog1 bit
;					   (setq bit (logshift32 bit 1)))))
;			  . ,(funcall b-f type
;				      `(index-incf %index 4) '%item% args)))))))))
;	  `((write-card32 ,index %mask)
;	    (write-card16 2 (index-ceiling (index-incf %index 4) 4))
;	    (incf (buffer-boffset %buffer) %index))))))

(defun mask-put (index type-values b-f)
  ;;(declare (type function body-function)
  ;;(downward-funarg body-function))
  ;; The MASK type writes a 32 bit mask with 1 bits for each non-nil value in TYPE-VALUES
  ;; A 32 bit value follows for each non-nil value.
  `((let ((%mask (make-string 4 0))
	  (%index ,index))
      ,@(let ((bit 31))
	  (car (get-put-items
		index type-values t
		#'(lambda (type index item args)
		    (if (or (symbolp item) (constantp item))
			`((unless (null ,item)
			    (bset %mask ,(prog1 bit (setq bit (1- bit))) t)
			    . ,(funcall b-f type
					`(index-incf %index 4) item args)))
		      `((let ((%item% ,item))
			  (unless (null %item%)
			    (bset %mask ,(prog1 bit (setq bit (1- bit))) t)
			    . ,(funcall b-f type
					`(index-incf %index 4) '%item% args)))))))))
      (write-card32 ,index %mask)
      (write-card16 2 (index-ceiling (index-incf %index 4) 4))
      (incf (buffer-boffset %buffer) %index))))

;;; uti - progn de hajimaru shorini natte iru toki. (see create-gcontext)
;(define-accessor progn (nil)
;  ;; Catch-all for inserting random code
;  ;; Note that code using this is then responsible for setting the request length
;  ((index statement) (declare (ignore index)) statement)
;  ((index statement) (declare (ignore index)) statement))

(define-accessor progn (nil)
  ((index statement) statement)
  ((index statement) statement))

;
; Wrapper macros, for use around the above
;
;(defmacro type-check (value type)
;  value type
;  (when *type-check?*
;    `(unless (type? ,value ,type)
;       (x-type-error ,value ,type))))
;

; No type checking (yet).
(defmacro check-put (index value type . args)
  `(,(putify type) ,index ,value . ,args))

;(defmacro check-put (index value type &rest args &environment env)
;  (let* ((var (if (or (symbolp value) (constantp value)) value '.value.))
;	 (body
;	   (if (or (null (macroexpand `(type-check ,var ',type) env))
;		   (member type '(or progn pad8 pad16))
;		   (constantp value))
;	       `(,(putify type) ,index ,var ,@args)
;	     ;; Do type checking
;	     (if (get type 'predicating-put)
;		 `(or (,(putify type t) ,index ,var ,@args)
;		      (x-type-error ,var ',(if args `(,type ,@args) type)))
;	       `(if (type? ,var ',type)
;		    (,(putify type) ,index ,var ,@args)
;		  (x-type-error ,var ',(if args `(,type ,@args) type)))))))
;    (if (eq var value)
;	body
;      `(let ((,var ,value))
;	 ,body))))

;;;
;;; get-put-items
;;; 普通は put 関数として check-put を使う
;;;
(defun get-put-items (index type-args putp (body-function nil))
  (comment declare (type (or null function) body-function)
	   (downward-funarg body-function))
  ;; Given a lists of the form (type item item ... item)
  ;; Calls body-function with four arguments, a function name,
  ;; index, item name, and optional arguments.
  ;; The results are appended together and retured.
  (unless body-function
    (setq body-function
	  #'(lambda (type index item args)
	      `((check-put ,index ,item ,type . ,args)))))
  (do* ((items type-args (cdr items))
;	(type (caar items) (caar items))
	(type (and items (caar items)) (and items (caar items)))
	(args nil nil)
	(result nil)
	(sizes nil))
      ((atom items) (values result index sizes))
    (when (consp type)
      (setq args (cdr type)
	    type (car type)))
    (cond ((member type '(return buffer)))
	  ((eq type 'mask) ;; Hack to enable mask-get/put to retun multiple values
	   (setq result
		 (append result (if putp 
				    (mask-put index (cdar items) body-function)
				  (mask-get index (cdar items) body-function)))
		 index nil))
	 (t (do* ((item (cdar items) (cdr item))
		  (increment (index-increment type)))
		((atom item))
	      ;;(format "  /s/n" item)
	      (when (constantp index)
		(case increment		;Round up index when needed
		  (2 (setq index (wround index)))
		  (4 (setq index (lround index)))))
	      (setq result
		    (append result (funcall body-function type index (car item) args)))
	      (when (constantp index)
		;; Variable length requests have null length increment.
		;; Variable length requests set the request size 
		;; & maintain buffer pointers
		(if (null increment) 
		    (setq index nil)
		  (progn
		    (incf index increment)
		    (when (and increment (zerop increment)) (setq increment 1))
		    (pushnew (* increment 8) sizes)))))))))

(defun remove-duplicates (x) x)
;;;
;;; with-buffer-request-internal
;;;
;;; utilisp
(defmacro with-buffer-request-internal
  ((buffer opcode . keys) . type-args)
  (let ((x (gensym)))
    (setplist x keys)			;keys contains other keys.
    (lets ((length (get x ':length))
	   (sizes (get x ':sizes)))
      (lets ((res (get-put-items 4 type-args t))
	     (code (first res))
	     (index (second res))
	     (item-sizes (third res)))
	(let ((length (if length `(index+ ,length *requestsize*) '*requestsize*))
	      (sizes (remove-duplicates (append '(8 16) item-sizes sizes))))
	  `(with-buffer-output (,buffer :length ,length :sizes ,sizes)
	     (setl (buffer-last-request ,buffer) buffer-boffset)
	     (write-card8 0 ,opcode)	;; Stick in the opcode
	     . ,(append
		code
		(when index
		   (setq index (lround index))
		   `((write-card16 2 ,(ceiling index 4))
		     (setl (buffer-boffset ,buffer)(index+ buffer-boffset ,index))))
		`((buffer-new-request-number ,buffer)))))))))

(defmacro with-buffer-request ((buffer opcode . options) . type-args)
  (let ((gc-force (memq ':gc-force options)))
    (and gc-force (setq gc-force (second gc-force)))
    `(let ((/.display/. ,buffer))
       (with-buffer (/.display/.)
	 ,@(when gc-force `((force-gcontext-changes-internal ,gc-force)))
	 (prog1 
	     (with-buffer-request-internal (/.display/. ,opcode . ,options)
					   . ,type-args)
	   (display-invoke-after-function /.display/.))))))

(defmacro with-buffer-request-and-reply ((buffer opcode reply-size . keys)
					 type-args . reply-forms)
  (let ((multiple-reply (memq ':multiple-reply keys)))
    (lets ((inner-reply-body
	    `(with-buffer-input (/.reply-buffer. :display /.display.)
				nil . ,reply-forms))
	   (reply-body
	    (if (or (not (symbolp reply-size)) (constantp reply-size))
		inner-reply-body
	      `(let ((,reply-size (reply-data-size /.reply-buffer.)))
		 ,inner-reply-body))))
      `(let ((/.display. ,buffer)
	     (/.pending-command. nil)
	     (/.reply-buffer. nil))
	 (unwind-protect
	     (progn 
	       (with-buffer (/.display.)
		 (setq /.pending-command. (start-pending-command /.display.))
		 (without-aborts
		  (with-buffer-request-internal (/.display. ,opcode) . ,type-args))
		 (buffer-force-output /.display.)
		 (display-invoke-after-function /.display.))
	       . ,(if multiple-reply
		      `((loop
			 (setq /.reply-buffer. (read-reply /.display. /.pending-command.))
			 (when ,reply-body (exit nil))
			 (deallocate-reply-buffer (prog1 /.reply-buffer
							 (setq /.reply-buffer. nil)))))
		    `((setq /.reply-buffer. (read-reply /.display. /.pending-command.))
		      ,reply-body)))
	   (when /.reply-buffer.
		 (deallocate-reply-buffer /.reply-buffer.))
	   (when /.pending-command.
		 (stop-pending-command /.display. /.pending-command.))
	   )
	 ))))

(defmacro compare-request ((index) . body)
  `(macrolet ((write-card32 (index item) `(equal ,item (read-card32 ,index)))
	      (write-int32 (index item) `(equal ,item (read-int32 ,index)))
	      (write-card29 (index item) `(equal ,item (read-card29 ,index)))
	      (write-int29 (index item) `(equal ,item (read-int29 ,index)))
	      (write-card16 (index item) `(= ,item (read-card16 ,index)))
	      (write-int16 (index item) `(= ,item (read-int16 ,index)))
	      (write-card8 (index item) `(= ,item (read-card8 ,index)))
	      (write-int8 (index item) `(= ,item (read-int8 ,index))))
     (macrolet ((type-check (value type) value type nil))
       (and . ,(car (get-put-items index body t))))))

(defmacro put-items ((index) . body)
  `(progn . ,(car (get-put-items index body t))))

(defmacro decode-type (type value)
  ;; Given an integer and type, return the value
  (let ((args nil))
    (when (consp type)
      (setq args (cdr type)
	    type (car type)))
    `(macrolet ((read-card29 (value) value)
		(read-card32 (value) value)
		(read-int32 (value) `(card32->int32 ,value))
		(read-card16 (value) `(card32->big ,value))
		(read-int16 (value) `(card16->int16 (card32->big ,value)))
		(read-card8 (value) `(card32->big ,value))
		(read-int8 (value) `(int8->card8 (card32->big ,value))))
       (,(getify type) ,value ,@args))))

;;; macrolet wo tukawanai decode-type
;(defmacro decode-type (type value)
;  (let ((args nil))
;    (when (consp type)
;      (setq args (cdr type)
;	    type (car type)))
;    (selectq type
;      ((card29 card32 card16 card8) value)
;      (int32 `(card32->int32 ,value))
;      (int16 `(card16->int16 ,value))
;      (int8  `(int8->int8 ,value))
;      (member (let ((v (gensym))
;		    `(let ((,v (card32->big ,value)))
;		       (and (lessp ,v ,(length args))
;			    (vref ,(vector (length args) args) ,v))))))
;
;      (t '(format "Warning: decode-type needs more selectq blanches!!!/n")))))

;;; utilisp deha meiji tekini atai wo 32bit integer (high . low) ni naosite yaranakereba
;;; naranai.
(defmacro encode-type (type value)
  ;; Given a value and type, return an integer
  ;; When check-p, do type checking on value
  (let ((args nil))
   (when (consp type)
      (setq args (cdr type)
	    type (car type)))
    `(macrolet ((write-card29 (index value) index value)
		(write-card32 (index value) index value)
		(write-int32 (index value) index `(int32->card32 ,value))
		(write-card16 (index value) index `(fix32 ,value))
		(write-int16 (index value) index `(fix32 (int16->card16 ,value)))
		(write-card8 (index value) index `(fix32 ,value))
		(write-int8 (index value) index `(fix32 (int8->card8 ,value))))
       (check-put 0 ,value ,type ,@args))))

;;; macrolet wo tukawanai encode-type (test).
;(defmacro encode-type (type value)
;  ;; Given a value and type, return an integer
;  ;; When check-p, do type checking on value
;  (let ((args nil))
;    (when (consp type)
;      (setq args (cdr type)
;	    type (car type)))
;    (selectq type
;      ((card29 card32) value)
;      ((card16 card8) `(fix32 ,value))
;      (int32 `(int32->card32 ,value))
;      (int16 `(fix32 (int16->card16 ,value)))
;      (int8  `(fix32 (int8->card8 ,value)))
;      (member `(fix32 (position ,value ,(vector (length args) args)))
;      (boole-constant)
;      (t)))))
       
(defmacro set-decode-type (type accessor value)
  `(setf ,accessor (encode-type ,type ,value)))
;(defsetf decode-type set-decode-type)


;;;
;;; Request codes
;;; 

(defconstant *x-createwindow*                  1)
(defconstant *x-changewindowattributes*        2)
(defconstant *x-getwindowattributes*           3)
(defconstant *x-destroywindow*                 4)
(defconstant *x-destroysubwindows*             5)  
(defconstant *x-changesaveset*                 6)
(defconstant *x-reparentwindow*                7)
(defconstant *x-mapwindow*                     8)
(defconstant *x-mapsubwindows*                 9)
(defconstant *x-unmapwindow*                  10)
(defconstant *x-unmapsubwindows*              11) 
(defconstant *x-configurewindow*              12)
(defconstant *x-circulatewindow*              13)
(defconstant *x-getgeometry*                  14)
(defconstant *x-querytree*                    15)
(defconstant *x-internatom*                   16)
(defconstant *x-getatomname*                  17)
(defconstant *x-changeproperty*               18)
(defconstant *x-deleteproperty*               19)
(defconstant *x-getproperty*                  20)
(defconstant *x-listproperties*               21)
(defconstant *x-setselectionowner*            22)  
(defconstant *x-getselectionowner*            23) 
(defconstant *x-convertselection*             24)
(defconstant *x-sendevent*                    25)
(defconstant *x-grabpointer*                  26)
(defconstant *x-ungrabpointer*                27)
(defconstant *x-grabbutton*                   28)
(defconstant *x-ungrabbutton*                 29)
(defconstant *x-changeactivepointergrab*      30)         
(defconstant *x-grabkeyboard*                 31)
(defconstant *x-ungrabkeyboard*               32)
(defconstant *x-grabkey*                      33)
(defconstant *x-ungrabkey*                    34)
(defconstant *x-allowevents*                  35)
(defconstant *x-grabserver*                   36)     
(defconstant *x-ungrabserver*                 37)       
(defconstant *x-querypointer*                 38)       
(defconstant *x-getmotionevents*              39)          
(defconstant *x-translatecoords*              40)               
(defconstant *x-warppointer*                  41)      
(defconstant *x-setinputfocus*                42)        
(defconstant *x-getinputfocus*                43)        
(defconstant *x-querykeymap*                  44)      
(defconstant *x-openfont*                     45)   
(defconstant *x-closefont*                    46)    
(defconstant *x-queryfont*                    47)
(defconstant *x-querytextextents*             48)    
(defconstant *x-listfonts*                    49) 
(defconstant *x-listfontswithinfo*    	      50)
(defconstant *x-setfontpath*                  51)
(defconstant *x-getfontpath*                  52)
(defconstant *x-createpixmap*                 53)      
(defconstant *x-freepixmap*                   54)   
(defconstant *x-creategc*                     55)
(defconstant *x-changegc*                     56)
(defconstant *x-copygc*                       57)
(defconstant *x-setdashes*                    58)  
(defconstant *x-setcliprectangles*            59)         
(defconstant *x-freegc*                       60)
(defconstant *x-cleartobackground*            61)          
(defconstant *x-copyarea*                     62)
(defconstant *x-copyplane*                    63)
(defconstant *x-polypoint*                    64)
(defconstant *x-polyline*                     65)
(defconstant *x-polysegment*                  66)  
(defconstant *x-polyrectangle*                67)   
(defconstant *x-polyarc*                      68)
(defconstant *x-fillpoly*                     69)
(defconstant *x-polyfillrectangle*            70)        
(defconstant *x-polyfillarc*                  71) 
(defconstant *x-putimage*                     72)
(defconstant *x-getimage*                     73)
(defconstant *x-polytext8*                    74)   
(defconstant *x-polytext16*                   75)   
(defconstant *x-imagetext8*                   76)  
(defconstant *x-imagetext16*                  77)  
(defconstant *x-createcolormap*               78)    
(defconstant *x-freecolormap*                 79) 
(defconstant *x-copycolormapandfree*          80)       
(defconstant *x-installcolormap*              81)  
(defconstant *x-uninstallcolormap*            82)   
(defconstant *x-listinstalledcolormaps*       83)       
(defconstant *x-alloccolor*                   84)
(defconstant *x-allocnamedcolor*              85)    
(defconstant *x-alloccolorcells*              86)   
(defconstant *x-alloccolorplanes*             87)   
(defconstant *x-freecolors*                   88)
(defconstant *x-storecolors*                  89)
(defconstant *x-storenamedcolor*              90)   
(defconstant *x-querycolors*                  91)
(defconstant *x-lookupcolor*                  92)
(defconstant *x-createcursor*                 93)
(defconstant *x-createglyphcursor*            94)    
(defconstant *x-freecursor*                   95)
(defconstant *x-recolorcursor*                96)  
(defconstant *x-querybestsize*                97) 
(defconstant *x-queryextension*               98) 
(defconstant *x-listextensions*               99)
(defconstant *x-setkeyboardmapping*           100)
(defconstant *x-getkeyboardmapping*           101)
(defconstant *x-changekeyboardcontrol*        102)               
(defconstant *x-getkeyboardcontrol*           103)           
(defconstant *x-bell*                         104)
(defconstant *x-changepointercontrol*         105)
(defconstant *x-getpointercontrol*            106)
(defconstant *x-setscreensaver*               107)         
(defconstant *x-getscreensaver*               108)        
(defconstant *x-changehosts*                  109)    
(defconstant *x-listhosts*                    110) 
(defconstant *x-changeaccesscontrol*          111)          
(defconstant *x-changeclosedownmode*          112)
(defconstant *x-killclient*                   113)
(defconstant *x-rotateproperties*	      114)
(defconstant *x-forcescreensaver*	      115)
(defconstant *x-setpointermapping*            116)
(defconstant *x-getpointermapping*            117)
(defconstant *x-setmodifiermapping*	      118)
(defconstant *x-getmodifiermapping*	      119)
(defconstant *x-nooperation*                  127)

;;; Some macros for threaded lists
;(defmacro threaded-atomic-push (item list next type)
;  (let ((x (gensym))
;	(y (gensym)))
;    `(let ((,x ,item))
;       (loop
;	 (let ((,y ,list))
;	   (declare (type (or null ,type) ,y)
;		    (optimize (speed 3) (safety 0)))
;	   (setf (,next ,x) ,y)
;	   (when (conditional-store ,list ,y ,x)
;	     (return ,x)))))))
;
(defmacro threaded-atomic-push (item list next type)
  `(threaded-push ,item ,list ,next ,type))
(defmacro threaded-atomic-pop (item next type)
  `(threaded-pop ,item ,next ,type))
;(defmacro threaded-atomic-pop (list next type)
;  (let ((y (gensym)))
;    `(loop
;       (let ((,y ,list))
;	 (if (null ,y)
;	     (return nil)
;	   (when (conditional-store ,list ,y (,next (the ,type ,y)))
;	     (setf (,next (the ,type ,y)) nil)
;	     (return ,y)))))))

(defmacro threaded-nconc (item list next type)
  (let ((first (gensym))
	(x (gensym))
	(y (gensym))
	(z (gensym)))
    `(let ((,z ,item)
	   (,first ,list))
       (if (null ,first)
	   (setf ,list ,z)
	 (do* ((,x ,first ,y)
	       (,y (,next ,x) (,next ,x)))
	      ((null ,y)
	       (setf (,next ,x) ,z)
	       ,first)
	   )))))

(defmacro threaded-push (item list next type)
  (let ((x (gensym)))
    `(let ((,x ,item))
       (setf (,next ,x) ,list)
       (setf ,list ,x)
       ,x)))

(defmacro threaded-pop (list next type)
  (let ((x (gensym)))
    `(let ((,x ,list))
       (when ,x
	 (setf ,list (,next ,x))
	 (setf (,next ,x) nil))
       ,x)))

;; uti
(defmacro threaded-enqueue (item head tail next type)
  (let ((x (gensym)))
    `(let ((,x ,item))
       (if (null ,tail)
	   (threaded-nconc ,x ,head ,next ,type)
	 (threaded-nconc ,x (,next (the ,type ,tail)) ,next ,type))
       (setf ,tail ,x))))

;;uti
(defmacro threaded-dequeue (head tail next type)
  (let ((x (gensym)))
    `(let ((,x ,head))
       (when ,x
	 (when (eq ,x ,tail)
	   (setf ,tail (,next (the ,type ,x))))
	 (setf ,head (,next (the ,type ,x))))
       ,x)))

;;uti
(defmacro threaded-requeue (item head tail next type)
  (let ((x (gensym)))
    `(let ((,x ,item))
       (if (null ,tail)
	   (setf ,tail (setf ,head ,x))
	 (prog1 (,next ,x) (setf (,next ,x) ,head) (setf ,head ,x)))
       ,x)))

;; uti
;(defmacro threaded-dolist ((variable list next type) . body)
;  `(block nil
;     (do* ((,variable ,list (,next (the ,type ,variable))))
;	  ((null ,variable))
;       . ,body)))

(defmacro threaded-dolist ((variable list next type) . body)
  `(do* ((,variable ,list (,next ,variable)))
	((null ,variable))
	. ,body))

(defmacro threaded-delete (item list next type)
  (let ((x (gensym))
	(y (gensym))
	(z (gensym))
	(first (gensym)))
    `(let ((,x ,item)
	   (,first ,list))
       (when ,first
	 (if (eq ,first ,x)
	     (setf ,first (setf ,list (,next ,x)))
	   (do* ((,y ,first ,z)
		 (,z (,next ,y) (,next ,y)))
		((or (null ,z) (eq ,z ,x))
		 (when (eq ,z ,x)
		   (setf (,next ,y) (,next ,x)))) )))
       (setf (,next ,x) nil)
       ,first)))

(defmacro threaded-length (list next type)
  (let ((x (gensym))
	(count (gensym)))
    `(do ((,x ,list (,next (the ,type ,x)))
	  (,count 0 (index1+ ,count)))
	 ((null ,x)
	  ,count))))
