;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; GContext

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

;;;	GContext values are usually cached locally in the GContext object.
;;;	This is required because the X.11 server doesn't have any requests
;;;	for getting GContext values back.
;;;
;;;	GContext changes are cached until force-GContext-changes is called.
;;;	All the requests that use GContext (including the GContext accessors,
;;;	but not the SETF's) call force-GContext-changes.
;;;	In addition, the macro WITH-GCONTEXT may be used to provide a 
;;;	local view if a GContext.
;;;
;;;	Each GContext keeps a copy of the values the server has seen, and
;;;	a copy altered by SETF, called the LOCAL-STATE (bad name...).
;;;	The SETF accessors increment a timestamp in the GContext.
;;;	When the timestamp in a GContext isn't equal to the timestamp in
;;;	the local-state, changes have been made, and force-GContext-changes
;;;	loops through the GContext and local-state, sending differences to
;;;	the server, and updating GContext.
;;;
;;;	WITH-GCONTEXT works by BINDING the local-state slot in a GContext to
;;;	a private copy.  This is easy (and fast) for lisp machines, but other
;;;	lisps will have problems.  Fortunately, most other lisps don't care,
;;;	because they don't run in a multi-processing shared-address space
;;;	environment.

(in-package 'xlib :use '(lisp))

(export '(force-gcontext-changes
	  with-gcontext
	  create-gcontext
	  copy-gcontext-components
	  copy-gcontext
	  free-gcontext
	  
	  gcontext-function
	  gcontext-plane-mask
	  gcontext-foreground
	  gcontext-background
	  gcontext-line-width
	  gcontext-line-style
	  gcontext-cap-style
	  gcontext-join-style
	  gcontext-fill-style
	  gcontext-fill-rule
	  gcontext-tile
	  gcontext-stipple
	  gcontext-ts-x
	  gcontext-ts-y
	  gcontext-font
	  gcontext-subwindow-mode
	  gcontext-exposures
	  gcontext-clip-x
	  gcontext-clip-y
	  gcontext-clip-mask
	  gcontext-dashes
	  gcontext-arc-mode
	  gcontext-dash-offset
          gcontext-clip-ordering

	  define-gcontext-accessor
	  ))

;; GContext state accessors
;;	The state vector contains all card32s to speed server updating

(eval-when (eval compile load)

;(defconstant *gcontext-fast-change-length* #.(length *gcontext-components*))
;
(defconstant *gcontext-fast-change-length* (length *gcontext-components*))
)




(eval-when (eval compile)

;;; uti
;(defmacro setfgetf (pl key value)
;  (let ((spl (gensym))
;	(skey (gensym))
;	(svalue (gensym))
;	(sx (gensym)))
;    `(lets ((,spl ,pl)
;	    (,skey ,key)
;	    (,svalue ,value)
;	    (,sx (memq ,skey ,spl)))
;       (cond (,sx (rplaca (cdr ,sx) ,svalue))
;	     (t (push ,svalue ,pl)
;		(push ,skey ,pl)))
;       ,svalue)))

;(defmacro def-gc-internals (name . extras)
;  (let ((macros nil)
;	(indexes nil)
;	(masks nil)
;	(index 0))
;    (dolist (name *gcontext-components*)
;      (push `(defmacro ,(xintern 'gcontext-internal- name) (state)
;	       `(svref ,state ,,index))
;	    macros)
;      ;;(setf (getf indexes name) index)
;      (setfgetf indexes name index)
;      (push (logshift 1 index) masks)
;      (incf index))
;    (dolist (extra extras)
;      (push `(defmacro ,(xintern 'gcontext-internal- (first extra)) (state)
;	       `(svref ,state ,,index))
;	    macros)
;      ;;(setf (getf indexes (or (second extra) (first extra))) index)
;      (setfgetf indexes (or (second extra) (first extra)) index)
;      (push (logor (logshift 1 index)
;		    (if (second extra)
;			(logshift 1 (position (second extra) *gcontext-components*))
;			0))
;	    masks)
;      (incf index))
;    `(within-definition (def-gc-internals ,name)
;       ,@(nreverse macros)
;       (eval-when (eval compile load)
;	 (defconstant *gcontext-data-length* ,index)
;	 (defconstant *gcontext-indexes* ',indexes)
;	 ;;(defconstant *gcontext-masks* ',(coerce (nreverse masks) 'simple-vector))
;	 (defconstant *gcontext-masks* ',(vector (length masks) (nreverse masks)))
;	 ))))

;;; setfgetf wo tukawanai version
(defmacro def-gc-internals (name . extras)
  (let ((macros nil)
	(indexes (gensym))
	(masks nil)
	(index 0))
    (dolist (name *gcontext-components*)
      (push `(defmacro ,(xintern 'gcontext-internal- name) (state)
	       `(vref ,state ,,index))
	    macros)
      ;;(setf (getf indexes name) index)
      (putprop indexes index name)
      (push (logshift32 (fix32 1) index) masks)
      (incf index))

    (dolist (extra extras)
      (push `(defmacro ,(xintern 'gcontext-internal- (first extra)) (state)
	       `(vref ,state ,,index))
	    macros)
      ;;(setf (getf indexes (or (second extra) (first extra))) index)
      (putprop indexes index (or (second extra) (first extra)))
      (push (logor32 (logshift32 (fix32 1) index)
		    (if (second extra)
			(logshift32 (fix32 1) (position (second extra) *gcontext-components*))
		      (fix32 0)))
	    masks)
      (incf index))
    `(within-definition (def-gc-internals ,name)
       ,@(nreverse macros)
       (eval-when (eval compile load)
	 (defconstant *gcontext-data-length* ,index)
	 (defconstant *gcontext-indexes* ',(plist indexes))
	 ;;(defconstant *gcontext-masks* ',(coerce (nreverse masks) 'simple-vector))
	 (defconstant *gcontext-masks* ',(vector (length masks) (nreverse masks)))
	 ))))

)

(def-gc-internals ignore
		  (:clip :clip-mask) (:dash :dashes) (:font-obj :font) (:timestamp nil))

(deftype gcmask () '(unsigned-byte #.*gcontext-fast-change-length*))

(deftype xgcmask () '(unsigned-byte #.*gcontext-data-length*))

(defstruct (gcontext-extension (:type vector) (:copier nil)) ;; un-named
  (name nil :type symbol :read-only t)
  (default nil :type t :read-only t)
  (set-function #'identity :type (function (gcontext t) t) :read-only t)
  (copy-function #'identity :type (function (gcontext gcontext t) t) :read-only t))

(defvar *gcontext-extensions* nil) ;; list of gcontext-extension

;; Gcontext state Resource
(defvar *gcontext-local-state-cache* nil) ;; List of unused gcontext local states

(defmacro gcontext-state-next (state)
  `(vref ,state 0))

(defun allocate-gcontext-state ()
  ;; Allocate a gcontext-state
  ;; Loop until a local state is found that's large enough to hold
  ;; any extensions that may exist.
  (let ((length (index+ *gcontext-data-length* (length *gcontext-extensions*))))
    (comment declare (type array-index length))
    (loop
      (let ((state (or (threaded-atomic-pop *gcontext-local-state-cache*
					    gcontext-state-next gcontext-state)
		       (make-array length :initial-element nil))))
	(comment declare (type gcontext-state state))
	(when (index>= (vector-length state) length)
	  (exit state))))))

;(defun deallocate-gcontext-state (state)
;  (comment declare (type gcontext-state state))
;  (fill state nil)
;  (threaded-atomic-push state *gcontext-local-state-cache*
;			gcontext-state-next gcontext-state))

(defun deallocate-gcontext-state (state)
  (fill-vector state nil)
  (threaded-atomic-push state *gcontext-local-state-cache*
			gcontext-state-next gcontext-state))

;; Temp-Gcontext Resource
(defvar *temp-gcontext-cache* nil) ;; List of unused gcontexts

(defun allocate-temp-gcontext ()
  (or (threaded-atomic-pop *temp-gcontext-cache* gcontext-next gcontext)
      (make-gcontext :local-state '#() :server-state '#())))

(defun deallocate-temp-gcontext (gc)
  (comment declare (type gcontext gc))
  (threaded-atomic-push gc *temp-gcontext-cache* gcontext-next gcontext))

;; For each argument to create-gcontext (except clip-mask and clip-ordering) comment declared
;; as (type <type> <name>), there is an accessor:

;(defun gcontext-<name> (gcontext)
;  ;; The value will be nil if the last value stored is unknown (e.g., the cache was
;  ;; off, or the component was copied from a gcontext with unknown state).
;  (comment declare (type gcontext gcontext)
;	   (values <type>)))

;; For each argument to create-gcontext (except clip-mask and clip-ordering) comment declared
;; as (type (or null <type>) <name>), there is a setf for the corresponding accessor:

;(defsetf gcontext-<name> (gcontext) (value)
;  )

;; Generate all the accessors and defsetf's for GContext

;(defmacro xgcmask->gcmask (mask)
;  `(the gcmask (logand ,mask #.(1- (ash 1 *gcontext-fast-change-length*)))))

(defmacro xgcmask->gcmask (mask)
  `(logand32 ,mask ',(dec32 (logshift32 (fix32 1) *gcontext-fast-change-length*))))

;(defmacro access-gcontext ((gcontext local-state) &body body)
;  `(let ((,local-state (gcontext-local-state ,gcontext)))
;     (comment declare (type gcontext-state ,local-state))
;     ,@body))

(defmacro access-gcontext ((gcontext local-state) . body)
  `(let ((,local-state (gcontext-local-state ,gcontext)))
     . ,body))

;(defmacro modify-gcontext ((gcontext local-state) &body body)
;  ;; The timestamp must be altered after the modification
;  `(let ((,local-state (gcontext-local-state ,gcontext)))
;     (comment declare (type gcontext-state ,local-state))
;     (prog1
;	 (progn ,@body)
;       (setf (gcontext-internal-timestamp ,local-state) 0))))

(defmacro modify-gcontext ((gcontext local-state) . body)
  ;; The timestamp must be altered after the modification
  `(let ((,local-state (gcontext-local-state ,gcontext)))
     (prog1
	 (progn . ,body)
       (setf (gcontext-internal-timestamp ,local-state) 0))))

(defmacro def-gc-accessor (name type)
  (let* ((gcontext-name (xintern 'gcontext- name))
	 (internal-accessor (xintern 'gcontext-internal- name))
	 (internal-setfer (xintern 'set- gcontext-name)))
    `(within-definition (,name def-gc-accessor)

       (defun ,gcontext-name (gcontext)
	 (let ((value (,internal-accessor (gcontext-local-state gcontext))))
	   (when value ;; Don't do anything when value isn't known
	     (let ((%buffer (gcontext-display gcontext)))
	       %buffer
	       (decode-type ,type value)))))
       
       (defun ,internal-setfer (gcontext value)
	 (modify-gcontext (gcontext local-state)
	   (setf (,internal-accessor local-state) (encode-type ,type value))
	   ,@(when (eq type 'pixmap)
	       ;; write-through pixmaps, because the protocol allows
	       ;; the server to copy the pixmap contents at the time
	       ;; of the store, rather than continuing to share with
	       ;; the pixmap.
	       `((let ((server-state (gcontext-server-state gcontext)))
		   (setf (,internal-accessor server-state) nil))))
	   value))
       
       (defsetf ,gcontext-name ,internal-setfer))))

(defmacro incf-internal-timestamp (state)
  (let ((ts (gensym)))
    `(let ((,ts (the fixnum (gcontext-internal-timestamp ,state))))
       (setq ,ts (if (= ,ts 134217727)	; 2^27 - 1.
		     1
		   (the fixnum (1+ ,ts))))
       (setf (gcontext-internal-timestamp ,state) ,ts))))

(def-gc-accessor function boole-constant)
(def-gc-accessor plane-mask card32)
(def-gc-accessor foreground card32)
(def-gc-accessor background card32)
(def-gc-accessor line-width card16)
(def-gc-accessor line-style (member :solid :dash :double-dash))
(def-gc-accessor cap-style (member :not-last :butt :round :projecting))
(def-gc-accessor join-style (member :miter :round :bevel))
(def-gc-accessor fill-style (member :solid :tiled :stippled :opaque-stippled))
(def-gc-accessor fill-rule (member :even-odd :winding))
(def-gc-accessor tile pixmap)
(def-gc-accessor stipple pixmap)
(def-gc-accessor ts-x int16) ;; Tile-Stipple-X-origin
(def-gc-accessor ts-y int16) ;; Tile-Stipple-Y-origin
;; (def-GC-accessor font font) ;; See below
(def-gc-accessor subwindow-mode (member :clip-by-children :include-inferiors))
(def-gc-accessor exposures (member :off :on))
(def-gc-accessor clip-x int16)
(def-gc-accessor clip-y int16)
;; (def-GC-accessor clip-mask) ;; see below
(def-gc-accessor dash-offset card16)
;; (def-GC-accessor dashes)  ;; see below
(def-gc-accessor arc-mode (member :chord :pie-slice))


(defun gcontext-clip-mask (gcontext)
  (access-gcontext (gcontext local-state)
    (multiple-value-bind (clip clip-mask)
	(progn
	  (values (gcontext-internal-clip local-state)
		  (gcontext-internal-clip-mask local-state)))
      (if (null clip)
	  (values (let ((%buffer (gcontext-display gcontext)))
		    (comment declare (type display %buffer))
		    (decode-type (or (member :none) pixmap) clip-mask))
		  nil)
	(values (second clip)
		(decode-type (or null (member :unsorted :y-sorted :yx-sorted :yx-banded))
			     (first clip)))))))

;(defsetf gcontext-clip-mask (gcontext &optional ordering) (clip-mask)
;  ;; A bit strange, but retains setf form.
;  ;; a nil clip-mask is transformed to an empty vector
;  `(set-gcontext-clip-mask ,gcontext ,ordering ,clip-mask))

(defsetf gcontext-clip-mask (gcontext . ordering) (clip-mask)
  `(set-gcontext-clip-mask ,gcontext ,(and ordering (car ordering)) ,clip-mask))

;(defun set-gcontext-clip-mask (gcontext ordering clip-mask)
;  ;; a nil clip-mask is transformed to an empty vector
;  (comment declare (type gcontext gcontext)
;	   (type (or null (member :unsorted :y-sorted :yx-sorted :yx-banded)) ordering)
;	   (type (or (member :none) pixmap rect-seq) clip-mask))
;  (unless clip-mask (x-type-error clip-mask '(or (member :none) pixmap rect-seq)))
;  (multiple-value-bind (clip-mask clip)
;      (typecase clip-mask
;	(pixmap (values (pixmap-id clip-mask) nil))
;	((member :none) (values 0 nil))
;	(sequence
;	 (values nil
;		 (list (encode-type
;			(or null (member :unsorted :y-sorted :yx-sorted :yx-banded))
;			ordering)
;		       (copy-seq clip-mask))))
;	(otherwise (x-type-error clip-mask '(or (member :none) pixmap rect-seq))))
;    (modify-gcontext (gcontext local-state)
;      (let ((server-state (gcontext-server-state gcontext)))
;	(comment declare (type gcontext-state server-state))
;	(progn
;	  (setf (gcontext-internal-clip local-state) clip
;		(gcontext-internal-clip-mask local-state) clip-mask)
;	  (if (null clip)
;	      (setf (gcontext-internal-clip server-state) nil)
;	    (setf (gcontext-internal-clip-mask server-state) nil))
;	  (when (and clip-mask (not (zerop clip-mask)))
;	    ;; write-through clip-mask pixmap, because the protocol allows the
;	    ;; server to copy the pixmap contents at the time of the store,
;	    ;; rather than continuing to share with the pixmap.
;	    (setf (gcontext-internal-clip-mask server-state) nil))))))
;  clip-mask)

;;; uti
(defun set-gcontext-clip-mask (gcontext ordering clip-mask)
  ;; a nil clip-mask is transformed to an empty vector
  (unless clip-mask (x-type-error clip-mask '(or (member :none) pixmap rect-seq)))
  (multiple-value-bind (clip-mask clip)
      (cond ((pixmap-p clip-mask) (values (pixmap-id clip-mask) nil))
	    ((eq clip-mask ':none) (values 0 nil))
	    ((consp clip-mask)
	     (values nil
		     (list (encode-type
			    (or null (member :unsorted :y-sorted :yx-sorted :yx-banded))
			    ordering)
			   (copy-seq clip-mask))))
	    (t (x-type-error clip-mask '(or (member :none) pixmap rect-seq))))
      (modify-gcontext (gcontext local-state)
      (let ((server-state (gcontext-server-state gcontext)))
	(comment declare (type gcontext-state server-state))
	(progn
	  (setf (gcontext-internal-clip local-state) clip
		(gcontext-internal-clip-mask local-state) clip-mask)
	  (if (null clip)
	      (setf (gcontext-internal-clip server-state) nil)
	    (setf (gcontext-internal-clip-mask server-state) nil))
	  (when (and clip-mask (not (zerop clip-mask)))
	    ;; write-through clip-mask pixmap, because the protocol allows the
	    ;; server to copy the pixmap contents at the time of the store,
	    ;; rather than continuing to share with the pixmap.
	    (setf (gcontext-internal-clip-mask server-state) nil))))))
  clip-mask)

(defun gcontext-dashes (gcontext)
  (access-gcontext (gcontext local-state)
    (multiple-value-bind (dash dashes)
	(progn
	  (values (gcontext-internal-dash local-state)
		  (gcontext-internal-dashes local-state)))
      (if (null dash)
	  dashes
	dash))))

(defsetf gcontext-dashes set-gcontext-dashes)

(defun set-gcontext-dashes (gcontext dashes)
  (multiple-value-bind (dashes dash)
      (if (type? dashes 'sequence)
	  (if (zerop (length dashes))
	      (x-type-error dashes '(or card8 sequence) "non-empty sequence")
	    (values nil (or (copy-seq dashes) (vector))))
	(values (encode-type card8 dashes) nil))
    (modify-gcontext (gcontext local-state)
      (let ((server-state (gcontext-server-state gcontext)))
	(progn
	  (setf (gcontext-internal-dash local-state) dash
		(gcontext-internal-dashes local-state) dashes)
	  (if (null dash)
	      (setf (gcontext-internal-dash server-state) nil)
	    (setf (gcontext-internal-dashes server-state) nil))))))
  dashes)

(defun gcontext-font (gcontext (metrics-p))
  ;; If the stored font is known, it is returned.  If it is not known and
  ;; metrics-p is false, then nil is returned.  If it is not known and
  ;; metrics-p is true, then a pseudo font is returned.  Full metric and
  ;; property information can be obtained, but the font does not have a name or
  ;; a resource-id, and attempts to use it where a resource-id is required will
  ;; result in an invalid-font error.
  (access-gcontext (gcontext local-state)
    (let ((font (gcontext-internal-font-obj local-state)))
      (or font
	  (when metrics-p
	    ;; XXX this isn't correct
	    (make-font :display (gcontext-display gcontext)
		       :id (gcontext-id gcontext)
		       :name nil))))))

(defsetf gcontext-font set-gcontext-font)

(defun set-gcontext-font (gcontext font)
  (let* ((font-object (if (font-p font) font (open-font (gcontext-display gcontext) font)))
	 (font (and font-object (font-id font-object))))
    ;; XXX need to check font has id (and name?)
    (modify-gcontext (gcontext local-state)
      (let ((server-state (gcontext-server-state gcontext)))
	(progn
	  (setf (gcontext-internal-font-obj local-state) font-object
		(gcontext-internal-font local-state) font)
	  (if (null font-object)
	      (setf (gcontext-internal-font server-state) nil)
	    (setf (gcontext-internal-font-obj server-state) font-object))))))
  font)

;(defun force-gcontext-changes-internal (gcontext)
;  ;; Force any delayed changes.
;
;  (let ((display (gcontext-display gcontext))
;	(server-state (gcontext-server-state gcontext))
;	(local-state (gcontext-local-state gcontext)))
;
;    ;; Update server when timestamps don't match
;    (unless (= (the fixnum (gcontext-internal-timestamp local-state))
;	       (the fixnum (gcontext-internal-timestamp server-state)))
;
;      ;; The display is already locked.
;      (macrolet ((with-buffer ((buffer &key timeout) &body body)
;		   `(progn (progn ,buffer ,@(and timeout `(,timeout)) nil)
;			   ,@body)))
;
;	;; Because there is no locking on the local state we have to
;	;; assume that state will change and set timestamps up front,
;	;; otherwise by the time we figured out there were no changes
;	;; and tried to store the server stamp as the local stamp, the
;	;; local stamp might have since been modified.
;	(setf (gcontext-internal-timestamp local-state)
;	      (incf-internal-timestamp server-state))
;
;	(block no-changes
;	  (let ((last-request (buffer-last-request display)))
;	    (with-buffer-request (display *x-changegc*)
;	      (gcontext gcontext)
;	      (progn
;		(do ((i 0 (index+ i 1))
;		     (bit 1 (the xgcmask (logshift bit 1)))
;		     (nbyte 12)
;		     (mask 0)
;		     (local 0))
;		    ((index>= i *gcontext-fast-change-length*)
;		     (when (zerop mask)
;		       ;; If nothing changed, restore last-request and quit
;		       (setf (buffer-last-request display)
;			     (if (zerop (buffer-last-request display))
;				 nil
;			       last-request))
;		       (return-from no-changes nil))
;		     (card29-put 8 mask)
;		     (card16-put 2 (index-ash nbyte -2))
;		     (index-incf (buffer-boffset display) nbyte))
;		  (comment declare (type array-index i nbyte)
;			   (type xgcmask bit)
;			   (type gcmask mask)
;			   (type (or null card32) local))
;		  (unless (eql (the (or null card32) (svref server-state i))
;			       (setq local (the (or null card32) (svref local-state i))))
;		    (setf (svref server-state i) local)
;		    (card32-put nbyte local)
;		    (setq mask (the gcmask (logor mask bit)))
;		    (index-incf nbyte 4)))))))
;
;	;; Update GContext extensions
;	(do ((extension *gcontext-extensions* (cdr extension))
;	     (i *gcontext-data-length* (index+ i 1))
;	     (local))
;	    ((endp extension))
;	  (unless (eql (svref server-state i)
;		       (setq local (svref local-state i)))
;	    (setf (svref server-state i) local)
;	    (funcall (gcontext-extension-set-function (car extension)) gcontext local)))
;
;	;; Update clipping rectangles
;	(multiple-value-bind (local-clip server-clip)
;	    (progn 
;	      (values (gcontext-internal-clip local-state)
;		      (gcontext-internal-clip server-state)))
;	  (unless (equalp local-clip server-clip)
;	    (setf (gcontext-internal-clip server-state) nil)
;	    (unless (null local-clip)
;	      (with-buffer-request (display *x-setcliprectangles*)
;		(data (first local-clip))
;		(gcontext gcontext)
;		;; XXX treat nil correctly
;		(card16 (or (gcontext-internal-clip-x local-state) 0)
;			(or (gcontext-internal-clip-y local-state) 0))
;		;; XXX this has both int16 and card16 values
;		((sequence :format int16) (second local-clip)))
;	      (setf (gcontext-internal-clip server-state) local-clip))))
;
;	;; Update dashes
;	(multiple-value-bind (local-dash server-dash)
;	    (progn
;	      (values (gcontext-internal-dash local-state)
;		      (gcontext-internal-dash server-state)))
;	  (unless (equalp local-dash server-dash)
;	    (setf (gcontext-internal-dash server-state) nil)
;	    (unless (null local-dash)
;	      (with-buffer-request (display *x-setdashes*)
;		(gcontext gcontext)
;		;; XXX treat nil correctly
;		(card16 (or (gcontext-internal-dash-offset local-state) 0)
;			(length local-dash))
;		((sequence :format card8) local-dash))
;	      (setf (gcontext-internal-dash server-state) local-dash))))))))

(defun force-gcontext-changes-internal (gcontext)
  ;; Force any delayed changes.

  (let ((display (gcontext-display gcontext))
	(server-state (gcontext-server-state gcontext))
	(local-state (gcontext-local-state gcontext)))

    ;; Update server when timestamps don't match
    (unless (= (gcontext-internal-timestamp local-state)
	       (gcontext-internal-timestamp server-state))

      ;; Because there is no locking on the local state we have to
      ;; assume that state will change and set timestamps up front,
      ;; otherwise by the time we figured out there were no changes
      ;; and tried to store the server stamp as the local stamp, the
      ;; local stamp might have since been modified.
      (setf (gcontext-internal-timestamp local-state)
	    (incf-internal-timestamp server-state))

      (prog nil
	(let ((last-request (buffer-last-request display)))
	  (with-buffer-request (display *x-changegc*)
	      (gcontext gcontext)
	      (progn
		(do ((i 0 (index+ i 1))
		     (bit (fix32 1) (logshift32 bit 1))
		     (nbyte 12)
		     (mask (fix32 0))
		     (local (fix32 0)))
		    ((index>= i *gcontext-fast-change-length*)
		     (when (zerop32 mask)
		       ;; If nothing changed, restore last-request and quit
		       (setf (buffer-last-request display)
			     (if (zerop (buffer-last-request display))
				 nil
			       last-request))
		       (return nil))
		     (card29-put 8 mask)
		     (card16-put 2 (index-ash nbyte -2))
		     (index-incf (buffer-boffset display) nbyte))
		  (unless (equal (vref server-state i)
				 (setq local (vref local-state i)))
		    (setf (svref server-state i) local)
		    (card32-put nbyte local)
		    (setq mask (logor32 mask bit))
		    (index-incf nbyte 4)))))))

	;; Update GContext extensions
      (do ((extension *gcontext-extensions* (cdr extension))
	   (i *gcontext-data-length* (index+ i 1))
	   (local))
	  ((atom extension))
	  (unless (equal (svref server-state i)
			 (setq local (svref local-state i)))
	    (setf (svref server-state i) local)
	    (funcall (gcontext-extension-set-function (car extension)) gcontext local)))

	;; Update clipping rectangles
      (multiple-value-bind (local-clip server-clip)
	(progn 
	  (values (gcontext-internal-clip local-state)
		  (gcontext-internal-clip server-state)))
	;; moto ha equalp de atta....
	(unless (equal local-clip server-clip)
	  (setf (gcontext-internal-clip server-state) nil)
	  (unless (null local-clip)
	    (with-buffer-request (display *x-setcliprectangles*)
	      (data (first local-clip))
	      (gcontext gcontext)
	      ;; XXX treat nil correctly
	      (card16 (or (gcontext-internal-clip-x local-state) 0)
		      (or (gcontext-internal-clip-y local-state) 0))
	      ;; XXX this has both int16 and card16 values
	      ((sequence :format int16) (second local-clip)))
	    (setf (gcontext-internal-clip server-state) local-clip))))

	;; Update dashes
      (multiple-value-bind (local-dash server-dash)
	(progn
	  (values (gcontext-internal-dash local-state)
		  (gcontext-internal-dash server-state)))
	;; moto ha equalp datta.
	(unless (equal local-dash server-dash)
	  (setf (gcontext-internal-dash server-state) nil)
	  (unless (null local-dash)
	    (with-buffer-request (display *x-setdashes*)
	      (gcontext gcontext)
	      ;; XXX treat nil correctly
	      (card16 (or (gcontext-internal-dash-offset local-state) 0)
		      (sequence-length local-dash))
	      ((sequence :format card8) local-dash))
	    (setf (gcontext-internal-dash server-state) local-dash))))))))

(defun force-gcontext-changes (gcontext)
  ;; Force any delayed changes.
  (comment declare (type gcontext gcontext))
  (let ((display (gcontext-display gcontext))
	(server-state (gcontext-server-state gcontext))
	(local-state (gcontext-local-state gcontext)))
    (comment declare (type gcontext-state server-state local-state)
	     (array-register server-state local-state))
    ;; Update server when timestamps don't match
    (unless (= (the fixnum (gcontext-internal-timestamp local-state))
	       (the fixnum (gcontext-internal-timestamp server-state)))
      (with-display (display)
	(force-gcontext-changes-internal gcontext)))))

;;; WARNING: WITH-GCONTEXT WORKS MUCH MORE EFFICIENTLY WHEN THE OPTIONS BEING "BOUND" ARE
;;;	     SET IN THE GCONTEXT ON ENTRY.  BECAUSE THERE'S NO WAY TO GET THE VALUE OF AN
;;;	     UNKNOWN GC COMPONENT, WITH-GCONTEXT MUST CREATE A TEMPORARY GC, COPY THE UNKNOWN
;;;	     COMPONENTS TO THE TEMPORARY GC, ALTER THE GC BEING USED, THEN COPY COMPOMENTS
;;;          BACK.

;(defmacro with-gcontext ((gcontext &rest options &key clip-ordering
;				   &allow-other-keys)
;			 &body body)
(defmacro with-gcontext ((gcontext . options-keys) . body)
  ;; "Binds" the gcontext components specified by options within the
  ;; dynamic scope of the body (i.e., indefinite scope and dynamic
  ;; extent), on a per-process basis in a multi-process environment.
  ;; The body is not surrounded by a with-display.  If cache-p is nil or
  ;; the some component states are unknown, this will implement
  ;; save/restore by creating a temporary gcontext and doing
  ;; copy-gcontext-components to and from it.

  (remf options :clip-ordering)

  (let ((gc (gensym))
	(saved-state (gensym))
	(temp-gc (gensym))
	(temp-mask (gensym))
	(temp-vars nil)
	(setfs nil)
	(indexes nil) ; List of gcontext field indices
	(extension-indexes nil) ; List of gcontext extension field indices
	(ts-index (getf *gcontext-indexes* :timestamp)))

    (do* ((option options (cddr option))
	  (name (and (consp option) (car option)) (and (consp option) (car option)))
	  (value (and (consp option) (cadr option)) (and (consp option) (cadr option))))
	 ((endp option) (setq setfs (nreverse setfs)))
      (let ((index (getf *gcontext-indexes* name)))
	(if index
	    (push index indexes)
	  (let ((extension (find name *gcontext-extensions*
				 :key #'gcontext-extension-name)))
	    (if extension
		(progn
		  (push (xintern "Internal-" 'gcontext- name "-State-Index")
			extension-indexes))
	      (x-type-error name 'gcontext-key)))))
      (let ((accessor `(,(xintern 'gcontext- name) ,gc
			,@(when (eq name :clip-mask) `(,clip-ordering))))
	    (temp-var (gensym)))
	(when value
	  (push `(,temp-var ,value) temp-vars)
	  (push `(setf ,accessor ,temp-var) setfs))))
    (if setfs
	`(multiple-value-bind (,gc ,saved-state ,temp-mask ,temp-gc)
	     (copy-gcontext-local-state ,gcontext ',indexes ,@extension-indexes)
	   (comment declare (type gcontext ,gc)
		    (type gcontext-state ,saved-state)
		    (type xgcmask ,temp-mask)
		    (type (or null resource-id) ,temp-gc))
	   (with-gcontext-bindings (,gc ,saved-state
					,(append indexes extension-indexes)
				    ,ts-index ,temp-mask ,temp-gc)
	     (let ,temp-vars
	       ,@setfs)
	     ,@body))
      `(progn ,@body))))

(defun copy-gcontext-local-state (gcontext indexes &rest extension-indices)
  ;; Called from WITH-GCONTEXT to save the fields in GCONTEXT indicated by MASK
  (comment declare (type gcontext gcontext)
	   (type list indexes)
	   (dynamic-extent extension-indices))
  (let ((local-state (gcontext-local-state gcontext))
	(saved-state (allocate-gcontext-state))
	(cache-p (gcontext-cache-p gcontext)))
    (comment declare (type gcontext-state local-state saved-state))
    (setf (gcontext-internal-timestamp saved-state) 1)
    (let ((temp-gc nil)
	  (temp-mask 0)
	  (extension-mask 0))
      (comment declare (type xgcmask temp-mask)
	       (type integer extension-mask))
      (dolist (i indexes)
	(when (or (not (setf (svref saved-state i) (svref local-state i)))
		  (not cache-p))
	  (setq temp-mask
		(the xgcmask (logor temp-mask
				     (the xgcmask (svref *gcontext-masks* i)))))))
      (dolist (i extension-indices)
	(when (or (not (setf (svref saved-state i) (svref local-state i)))
		  (not cache-p))
	  (setq extension-mask
		(the xgcmask (logior extension-mask (logshift 1 i))))))
      (when (or (plusp temp-mask)
		(plusp extension-mask))
	;; Copy to temporary GC when field unknown or cache-p false
	(let ((display (gcontext-display gcontext)))
	  (comment declare (type display display))
	  (with-display (display)
	    (setq temp-gc (allocate-temp-gcontext))
	    (setf (gcontext-id temp-gc) (allocate-resource-id display gcontext 'gcontext)
		  (gcontext-display temp-gc) display
		  (gcontext-drawable temp-gc) (gcontext-drawable gcontext)
		  (gcontext-server-state temp-gc) saved-state
		  (gcontext-local-state temp-gc) saved-state)
	    ;; Create a new (temporary) gcontext
	    (with-buffer-request (display *x-creategc*)
	      (gcontext temp-gc)
	      (drawable (gcontext-drawable gcontext))
	      (card29 0))
	    ;; Copy changed components to the temporary gcontext
	    (when (plusp temp-mask)
	      (with-buffer-request (display *x-copygc*)
		(gcontext gcontext)
		(gcontext temp-gc)
		(card29 (xgcmask->gcmask temp-mask))))
	    ;; Copy extension fields to the new gcontext
	    (when (plusp extension-mask)
	      ;; Copy extension fields from temp back to gcontext
	      (do ((bit (logshift extension-mask (- *gcontext-data-length*)) (logshift bit -1))
		   (i 0 (index+ i 1)))
		  ((zerop bit))
		(let ((copy-function (gcontext-extension-copy-function
				       (elt *gcontext-extensions* i))))
		  (funcall copy-function gcontext temp-gc
			   (svref local-state (index+ i *gcontext-data-length*))))))
	    )))
      (values gcontext saved-state (logior temp-mask extension-mask) temp-gc)))) 

(defun restore-gcontext-temp-state (gcontext temp-mask temp-gc)
  (comment declare (type gcontext gcontext temp-gc)
	   (type xgcmask temp-mask))
  (let ((display (gcontext-display gcontext)))
    (comment declare (type display display))
    (with-display (display)
      (with-buffer-request (display *x-copygc*)
	(gcontext temp-gc)
	(gcontext gcontext)
	(card29 (xgcmask->gcmask temp-mask)))
      ;; Copy extension fields from temp back to gcontext
      (do ((bit (logshift temp-mask (- *gcontext-data-length*)) (logshift bit -1))
	   (extensions *gcontext-extensions* (cdr extensions))
	   (i *gcontext-data-length* (index+ i 1))
	   (local-state (gcontext-local-state temp-gc)))
	  ((zerop bit))
	(let ((copy-function (gcontext-extension-copy-function (car extensions))))
	  (funcall copy-function temp-gc gcontext (svref local-state i))))
      ;; free gcontext
      (with-buffer-request (display *x-freegc*)
	(gcontext temp-gc))
      (deallocate-resource-id display (gcontext-id temp-gc) 'gcontext)
      (deallocate-temp-gcontext temp-gc)
      ;; Copy saved state back to server state
      (do ((server-state (gcontext-server-state gcontext))
	   (bit (xgcmask->gcmask temp-mask) (the gcmask (ash bit -1)))
	   (i 0 (index+ i 1)))
	  ((zerop bit)
	   (incf-internal-timestamp server-state))
	(comment declare (type gcontext-state server-state)
		 (type gcmask bit)
		 (type array-index i))
	(when (oddp bit)
	  (setf (svref server-state i) nil))))))

(defun create-gcontext (&rest options &key (drawable (required-arg drawable))
			function plane-mask foreground background
			line-width line-style cap-style join-style fill-style fill-rule
			arc-mode tile stipple ts-x ts-y font subwindow-mode
			exposures clip-x clip-y clip-mask clip-ordering
			dash-offset dashes
			(cache-p t)
			&allow-other-keys)
  ;; Only non-nil components are passed on in the request, but for effective caching
  ;; assumptions have to be made about what the actual protocol defaults are.  For
  ;; all gcontext components, a value of nil causes the default gcontext value to be
  ;; used.  For clip-mask, this implies that an empty rect-seq cannot be represented
  ;; as a list.  Note:  use of stringable as font will cause an implicit open-font.
  ;; Note:  papers over protocol SetClipRectangles and SetDashes special cases.  If
  ;; cache-p is true, then gcontext state is cached locally, and changing a gcontext
  ;; component will have no effect unless the new value differs from the cached
  ;; value.  Component changes (setfs and with-gcontext) are always deferred
  ;; regardless of the cache mode, and sent over the protocol only when required by a
  ;; local operation or by an explicit call to force-gcontext-changes.
  (comment declare (type drawable drawable) ; Required to be non-null
	   (type (or null boole-constant) function)
	   (type (or null pixel) plane-mask foreground background)
	   (type (or null card16) line-width dash-offset)
	   (type (or null int16) ts-x ts-y clip-x clip-y)
	   (type (or null (member :solid :dash :double-dash)) line-style)
	   (type (or null (member :not-last :butt :round :projecting)) cap-style)
	   (type (or null (member :miter :round :bevel)) join-style)
	   (type (or null (member :solid :tiled :opaque-stippled :stippled)) fill-style)
	   (type (or null (member :even-odd :winding)) fill-rule)
	   (type (or null (member :chord :pie-slice)) arc-mode)
	   (type (or null pixmap) tile stipple)
	   (type (or null fontable) font)
	   (type (or null (member :clip-by-children :include-inferiors)) subwindow-mode)
	   (type (or null (member :on :off)) exposures)
	   (type (or null (member :none) pixmap rect-seq) clip-mask)
	   (type (or null (member :unsorted :y-sorted :yx-sorted :yx-banded)) clip-ordering)
	   (type (or null card8 sequence) dashes)
	   (dynamic-extent options)
	   (type boolean cache-p))
  (comment declare (values gcontext))
  (let* ((display (drawable-display drawable))
	 (gcontext (make-gcontext :display display :drawable drawable :cache-p cache-p))
	 (local-state (gcontext-local-state gcontext))
	 (server-state (gcontext-server-state gcontext))
	 (gcontextid (allocate-resource-id display gcontext 'gcontext)))
    (comment declare (type display display)
	     (type gcontext gcontext)
	     (type resource-id gcontextid)
	     (type gcontext-state local-state server-state))
    (setf (gcontext-id gcontext) gcontextid)

    (unless function (setf (gcontext-function gcontext) boole-1))
    ;; using the depth of the drawable would be better, but ...
    (unless plane-mask (setf (gcontext-plane-mask gcontext) '(#xffff . #xffff)))
    (unless foreground (setf (gcontext-foreground gcontext) (fix32 0)))
    (unless background (setf (gcontext-background gcontext) (fix32 1)))
    (unless line-width (setf (gcontext-line-width gcontext) 0))
    (unless line-style (setf (gcontext-line-style gcontext) ':solid))
    (unless cap-style (setf (gcontext-cap-style gcontext) ':butt))
    (unless join-style (setf (gcontext-join-style gcontext) ':miter))
    (unless fill-style (setf (gcontext-fill-style gcontext) ':solid))
    (unless fill-rule (setf (gcontext-fill-rule gcontext) ':even-odd))
    (unless arc-mode (setf (gcontext-arc-mode gcontext) ':pie-slice))
    (unless ts-x (setf (gcontext-ts-x gcontext) 0))
    (unless ts-y (setf (gcontext-ts-y gcontext) 0))
    (unless subwindow-mode (setf (gcontext-subwindow-mode gcontext)
				 ':clip-by-children))
    (unless exposures (setf (gcontext-exposures gcontext) ':on))
    (unless clip-mask (setf (gcontext-clip-mask gcontext) ':none))
    (unless clip-x (setf (gcontext-clip-x gcontext) 0))
    (unless clip-y (setf (gcontext-clip-y gcontext) 0))
    (unless dashes (setf (gcontext-dashes gcontext) 4))
    (unless dash-offset (setf (gcontext-dash-offset gcontext) 0))
    ;; a bit kludgy, but ...
    ;;;(replace server-state local-state)
    ;;;(vector-replace server-state local-state)
    (fill-vector server-state local-state)

    (when function (setf (gcontext-function gcontext) function))
    (when plane-mask (setf (gcontext-plane-mask gcontext) plane-mask))
    (when foreground (setf (gcontext-foreground gcontext) foreground))
    (when background (setf (gcontext-background gcontext) background))
    (when line-width (setf (gcontext-line-width gcontext) line-width))
    (when line-style (setf (gcontext-line-style gcontext) line-style))
    (when cap-style (setf (gcontext-cap-style gcontext) cap-style))
    (when join-style (setf (gcontext-join-style gcontext) join-style))
    (when fill-style (setf (gcontext-fill-style gcontext) fill-style))
    (when fill-rule (setf (gcontext-fill-rule gcontext) fill-rule))
    (when arc-mode (setf (gcontext-arc-mode gcontext) arc-mode))
    (when tile (setf (gcontext-tile gcontext) tile))
    (when stipple (setf (gcontext-stipple gcontext) stipple))
    (when ts-x (setf (gcontext-ts-x gcontext) ts-x))
    (when ts-y (setf (gcontext-ts-y gcontext) ts-y))
    (when font (setf (gcontext-font gcontext) font))
    (when subwindow-mode (setf (gcontext-subwindow-mode gcontext) subwindow-mode))
    (when exposures (setf (gcontext-exposures gcontext) exposures))
    (when clip-x (setf (gcontext-clip-x gcontext) clip-x))
    (when clip-y (setf (gcontext-clip-y gcontext) clip-y))
    (when clip-mask (setf (gcontext-clip-mask gcontext clip-ordering) clip-mask))
    (when dash-offset (setf (gcontext-dash-offset gcontext) dash-offset))
    (when dashes (setf (gcontext-dashes gcontext) dashes))
    
    (setf (gcontext-internal-timestamp server-state) 1)
    (setf (gcontext-internal-timestamp local-state) 1)
    
    (with-buffer-request (display *x-creategc*)
      (resource-id gcontextid)
      (drawable drawable)
      (progn (do* ((i 0 (index+ i 1))
		   (bit 0 (1+ bit))
		   ;;(bit (fix32 1) (logshift32 bit 1))
		   (nbyte 16)
		   (mask (null-mask))
		   ;;(mask (fix32 0))
		   ;; local-state niha subeteno atai ga 32bit de haitte iru.
		   (local (svref local-state i) (svref local-state i)))
		 ((index>= i *gcontext-fast-change-length*)
		  (card29-put 12 mask)
		  (card16-put 2 (index-ash nbyte -2))
		  (index-incf (buffer-boffset display) nbyte))
		 (unless (equal local (the (or null card32) (svref server-state i)))
		   (setf (svref server-state i) local)
		   (card32-put nbyte local)
		   (set-mask mask bit)
		   ;;(setq mask (logor32 mask bit))
		   (index-incf nbyte 4)))))

    ;; Initialize extensions
    (do ((extensions *gcontext-extensions* (cdr extensions))
	 (i *gcontext-data-length* (index+ i 1)))
	((atom extensions))
      (comment declare (type list extensions)
	       (type array-index i))
      (setf (svref server-state i)
	    (setf (svref local-state i)
		  (gcontext-extension-default (car extensions)))))

    ;; Set extension values
    (do* ((option-list options (cddr option-list))
	  (option (and (consp option-list) (car option-list)) (and (consp option-list) (car option-list)))
	  (extension))
	 ((atom option-list))
      (comment declare (type list option-list))
      (cond ((getf *gcontext-indexes* option))	; Gcontext field
	    ((memq option '(:drawable :clip-ordering :cache-p)))	; Optional parameter
	    ((setq extension (find option *gcontext-extensions*
				   :key #'gcontext-extension-name))
	     (funcall (gcontext-extension-set-function extension)
		      gcontext (second option-list)))
	    (t (x-type-error option 'gcontext-key))))
    gcontext)) 

;(defun copy-gcontext-components (src dst &rest keys)
;  (comment declare (type gcontext src dst)
;	   (dynamic-extent keys))
;  ;; you might ask why this isn't just a bunch of
;  ;;   (setf (gcontext-<mumble> dst) (gcontext-<mumble> src))
;  ;; the answer is that you can do that yourself if you want, what we are
;  ;; providing here is access to the protocol request, which will generally
;  ;; be more efficient (particularly for things like clip and dash lists).
;  (when keys
;    (let ((display (gcontext-display src))
;	  (mask 0))
;      (comment declare (type xgcmask mask))
;      (with-display (display)
;	(force-gcontext-changes src)
;	(force-gcontext-changes dst)
;	
;	;; collect entire mask and handle extensions
;	(dolist (key keys)
;	  (let ((i (getf *gcontext-indexes* key)))
;	    (comment declare (type (or null array-index) i))
;	    (if i
;		(setq mask (the xgcmask (logior mask
;						(the xgcmask (svref *gcontext-masks* i)))))
;	      (multiple-value-bind (extension index)
;		  (find key *gcontext-extensions* :key #'gcontext-extension-name)
;		(if extension
;		    (funcall (gcontext-extension-copy-function extension)
;			     src dst (svref (gcontext-local-state src)
;					    (index+ index *gcontext-data-length*)))
;		  (x-type-error key 'gcontext-key))))))
;	
;	(when (plusp mask)
;	  (do ((src-server-state (gcontext-server-state src))
;	       (dst-server-state (gcontext-server-state dst))
;	       (dst-local-state (gcontext-local-state dst))
;	       (bit mask (the xgcmask (logshift bit -1)))
;	       (i 0 (index+ i 1)))
;	      ((zerop bit)
;	       (incf-internal-timestamp dst-server-state)
;	       (setf (gcontext-internal-timestamp dst-local-state) 0))
;	    (comment declare (type gcontext-state src-server-state dst-server-state dst-local-state)
;		     (type xgcmask bit)
;		     (type array-index i))
;	    (when (oddp bit)
;	      (setf (svref dst-local-state i)
;		    (setf (svref dst-server-state i) (svref src-server-state i)))))
;	  (with-buffer-request (display *x-copygc*)
;	    (gcontext src dst)
;	    (card29 (xgcmask->gcmask mask))))))))

(defmacro copy-gcontext-components (src dst . keys)
  `(_copy-gcontext-components src dst keys))

(defun _copy-gcontext-components (src dst keys)
  ;; you might ask why this isn't just a bunch of
  ;;   (setf (gcontext-<mumble> dst) (gcontext-<mumble> src))
  ;; the answer is that you can do that yourself if you want, what we are
  ;; providing here is access to the protocol request, which will generally
  ;; be more efficient (particularly for things like clip and dash lists).
  (when keys
    (let ((display (gcontext-display src))
	  (mask (fix32 0)))
      (with-display (display)
	(force-gcontext-changes src)
	(force-gcontext-changes dst)
	
	;; collect entire mask and handle extensions
	(dolist (key keys)
	  (let ((i (getf *gcontext-indexes* key)))
	    (if i
		(setq mask (logor32 mask (vref *gcontext-masks* i)))
	      (multiple-value-bind (extension index)
		  (find key *gcontext-extensions* :key #'gcontext-extension-name)
		(if extension
		    (funcall (gcontext-extension-copy-function extension)
			     src dst (svref (gcontext-local-state src)
					    (index+ index *gcontext-data-length*)))
		  (x-type-error key 'gcontext-key))))))
	
	(unless (zerop32 mask)
	  (do ((src-server-state (gcontext-server-state src))
	       (dst-server-state (gcontext-server-state dst))
	       (dst-local-state (gcontext-local-state dst))
	       (bit mask (the xgcmask (logshift32 bit -1)))
	       (i 0 (index+ i 1)))
	      ((zerop32 bit)
	       (incf-internal-timestamp dst-server-state)
	       (setf (gcontext-internal-timestamp dst-local-state) 0))
	    (when (oddp32 bit)
	      (setf (svref dst-local-state i)
		    (setf (svref dst-server-state i) (svref src-server-state i)))))
	  (with-buffer-request (display *x-copygc*)
	    (gcontext src dst)
	    (card29 (xgcmask->gcmask mask))))))))

;(defun copy-gcontext (src dst)
;  (comment declare (type gcontext src dst))
;  ;; Copies all components.
;  (apply #'copy-gcontext-components src dst *gcontext-components*)
;  (do ((extensions *gcontext-extensions* (cdr extensions))
;       (i *gcontext-data-length* (index+ i 1)))
;      ((atom extensions))
;    (funcall (gcontext-extension-copy-function (car extensions))
;	     src dst (svref (gcontext-local-state src) i))))

;; copy-gcontext-components cannot be apply'ed because it's a macro.
;; instead i funcall _copy-gcontext-components. (this is better...).
(defun copy-gcontext (src dst)
  ;; Copies all components.
  (_copy-gcontext-components src dst *gcontext-components*)
  (do ((extensions *gcontext-extensions* (cdr extensions))
       (i *gcontext-data-length* (index+ i 1)))
      ((atom extensions))
    (funcall (gcontext-extension-copy-function (car extensions))
	     src dst (svref (gcontext-local-state src) i))))
	   
(defun free-gcontext (gcontext)
  (comment declare (type gcontext gcontext))
  (let ((display (gcontext-display gcontext)))
    (with-buffer-request (display *x-freegc*)
      (gcontext gcontext))
    (deallocate-resource-id display (gcontext-id gcontext) 'gcontext)
    (deallocate-gcontext-state (gcontext-server-state gcontext))
    (deallocate-gcontext-state (gcontext-local-state gcontext))
    nil))

(defmacro define-gcontext-accessor (name &key default set-function copy-function)
  ;; This will define a new gcontext accessor called NAME.
  ;; Defines the gcontext-NAME accessor function and its defsetf.
  ;; Gcontext's will cache DEFAULT-VALUE and the last value SETF'ed when
  ;; gcontext-cache-p is true.  The NAME keyword will be allowed in
  ;; CREATE-GCONTEXT, WITH-GCONTEXT, and COPY-GCONTEXT-COMPONENTS.
  ;; SET-FUNCTION will be called with parameters (GCONTEXT NEW-VALUE)
  ;; from create-gcontext, and force-gcontext-changes.
  ;; COPY-FUNCTION will be called with parameters (src-gc dst-gc src-value)
  ;; from copy-gcontext and copy-gcontext-components.
  ;; The copy-function defaults to:
  ;; (lambda (ignore dst-gc value)
  ;;    (if value
  ;;	    (,set-function dst-gc value)
  ;;	  (error "Can't copy unknown GContext component ~a" ',name)))
  (comment declare (type symbol name)
	   (type t default)
	   (type (function (gcontext t) t) set-function) ;; required
	   (type (or null (function (gcontext gcontext t) t))
		 copy-function))
  (let* ((gc-name (intern (concatenate 'string
				       (string 'gcontext-)
				       (string name)))) ;; in current package
	 (key-name (kintern name))
	 (setfer (xintern "Set-" gc-name))
	 (internal-set-function (xintern "Internal-Set-" gc-name))
	 (internal-copy-function (xintern "Internal-Copy-" gc-name))
	 (internal-state-index (xintern "Internal-" gc-name "-State-Index")))
    (unless copy-function
      (setq copy-function
	    `(lambda (src-gc dst-gc value)
	       (comment declare (ignore src-gc))
	       (if value
		   (,set-function dst-gc value)
		 (error "Can't copy unknown GContext component ~a" ',name)))))
    `(progn
       (eval-when (compile load eval)
	 (defparameter ,internal-state-index
		       (add-gcontext-extension ',key-name ,default ',internal-set-function
					       ',internal-copy-function))
	 ) ;; end eval-when
       (defun ,gc-name (gcontext)
	 (svref (gcontext-local-state gcontext) ,internal-state-index))
       (defun ,setfer (gcontext new-value)
	 (let ((local-state (gcontext-local-state gcontext)))
	   (setf (gcontext-internal-timestamp local-state) 0)
	   (setf (svref local-state ,internal-state-index) new-value)))
       (defsetf ,gc-name ,setfer)
       (defun ,internal-set-function (gcontext new-value)
	 (,set-function gcontext new-value)
	 (setf (svref (gcontext-server-state gcontext) ,internal-state-index)
	       (setf (svref (gcontext-local-state gcontext) ,internal-state-index)
		     new-value)))
       (defun ,internal-copy-function (src-gc dst-gc new-value)
	 (,copy-function src-gc dst-gc new-value)
	 (setf (svref (gcontext-local-state dst-gc) ,internal-state-index)
	       (setf (svref (gcontext-server-state dst-gc) ,internal-state-index)
		     new-value)))
       ',name)))

;; GContext extension fields are treated in much the same way as normal GContext
;; components.  The current value is stored in a slot of the gcontext-local-state,
;; and the value known to the server is in a slot of the gcontext-server-state.
;; The slot-number is defined by its position in the *gcontext-extensions* list.
;; The value of the special variable |Internal-GCONTEXT-name| (where "name" is 
;; the extension component name) reflects this position.  The position within
;; *gcontext-extensions* and the value of the special value are determined at
;; LOAD time to facilitate merging of seperately compiled extension files.

(defun add-gcontext-extension (name default-value set-function copy-function)
  (comment declare (type symbol name)
	   (type t default-value)
	   (type (function (gcontext t) t) set-function)
	   (type (function (gcontext gcontext t) t) copy-function))
  (let ((number (or (position name *gcontext-extensions* :key #'gcontext-extension-name)
		    (prog1 (length *gcontext-extensions*)
			   (push nil *gcontext-extensions*)))))
    (setf (nth number *gcontext-extensions*)
	  (make-gcontext-extension :name name
				   :default default-value
				   :set-function set-function
				   :copy-function copy-function))
    (+ number *gcontext-data-length*)))
