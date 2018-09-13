;;; packet displaying function 
;;;
;;;
(defun last-packet (display)
  (substring (display-obuf8 display)
	     (display-last-request display)
	     (- (display-boffset display) (display-last-request display))))

(defun x:buffer-get-num (buf from n)
   (and (< (string-length buf) (/+ from n)) (x:error "buffer overflow"))
   (cond ((< n 4) (cutout buf from n))
         ((= n 4) `(,(cutout buf from 2) . ,(cutout buf (/+ from 2) 2)))))

(defun x:buffer-get-str (buf from n) (substring buf from (/+ from n)))

(defmacro x:card8-get (buf from) `(x:buffer-get-num ,buf ,from 1))

(defmacro x:card16-get (buf from) `(x:buffer-get-num ,buf ,from 2))

(defmacro x:card32-get (buf from) `(x:buffer-get-num ,buf ,from 4))

(defmacro x:str-get (buf from n) `(x:buffer-get-str ,buf ,from ,n))

(defmacro show-request (buf len . vals)
   (do ((from 0) (x vals (cdr x)) (y) (var) (kinds) (itemsize) (ss) (locals))
       ((atom x)
	`(lets ,(nreverse locals) . ,(x:showprog vals)))
       (setq y (car x))
       (setq var (car y) kinds (cdr y))
       (cond ((eq var 'unused)
              (do ((kinds kinds (cdr kinds)))
                  ((atom kinds))
                  (or (= (setq itemsize (x:itemsize (car kinds))) 1)
                      (setq from (x:adjust from 2)))
                  (incr from itemsize)))
             ((eq (car kinds) 'string)
              (setq from (x:adjust from 4))
              (push `(,var
                      (x:str-get
                         ,buf
                         ,(cond ((null ss) from) (t `(/+ ,from . ,ss)))
                         ,(cond ((cdr kinds) (second kinds))
				((null ss) `(- ,len ,from))
				(t `(- ,len (+ ,from . ,ss))))))
                    locals)
              (cond ((null (cdr kinds)) (setq x '(nil)))
	            ((fixp (second kinds))
                     (incr from (x:adjust (second kinds) 4)))
                    (t (push `(x:adjust ,(second kinds) 4) ss))))
             (t (or (= (setq itemsize (x:itemsize (car kinds))) 1)
                    (setq from (x:adjust from 2)))
                (push `(,var (,(x:getfn (car kinds)) ,buf ,from)) locals)
                (incr from itemsize)))))

(defun x:showprog (vals)
   (do ((v vals (cdr v)) (res))
       ((atom v) (nreverse res))
       (push `(format "/s /s:  /s/n" ',(caar v) ',(cdar v) ,(caar v)) res)))
	
(defun x:adjust (size n) (/+ size (\ size n)))

(defun x:getfn (kind)
   (selectq kind
      ((boolean card8 int8 char keycode) 'x:card8-get)
      ((card16 int16) 'x:card16-get)
      ((card32 int32 rid) 'x:card32-get)
      (t 'x:str-get)))

(defun x:itemsize (kind)
   (selectq kind
      ((boolean card8 int8 char keycode) 1)
      ((card16 int16 space16) 2)
      ((card32 int32 rid) 4)
      (t -1)))

(defun x:spacep (kind) (eq kind 'space16))

(defun soko (d)
  (lets ((p (last-packet d)) (len (string-length p)) (code (cutout p 0 1)))
   (catch 'soko (eval `(show-request ,p ,len . ,(reqpattern code))))))

(defun reqpattern (code)
   (lets ((x (assq code code2req)))
    (cond (x (print (cdr x)) (get (cdr x) 'reqpattern))
	  (t (format "UNsupported request /s/n" code) (throw  'soko)))))

(setq code2req nil)

(defmacro defpacket ((reqfun opcode) . packet-spec)
  `(progn
     (push '(,(eval opcode) . ,reqfun) code2req)
     (putprop ',reqfun 
	      ,(cond ((symbolp (car packet-spec))
		      `(get ',(car packet-spec) 'reqpattern))
		     (t `',packet-spec))
	      'reqpattern)
     ',reqfun))

(defpacket (create-window *x-createwindow*)
  (opcode card8)
  (depth card8)
  (reqlen card16)
  (wid rid)
  (parent rid)
  (x int16)
  (y int16)
  (width card16)
  (height card16)
  (borderwidth card16)
  (class card16)
  (visual-id rid)
  (bitmask card32)
  (listofvalues string))

(defpacket (map-window *x-mapwindow*)
   (opcode card8)
   (reqlen card16)
   (window rid))

(defpacket (map-subwindows *x-mapsubwindows*) map-window)
(defpacket (unmap-window *x-unmapwindow*) map-window)
(defpacket (unmap-subwindows *x-unmapsubwindows*) map-window)
(defpacket (destroy-window *x-destroywindow*) map-window)
(defpacket (destroy-subwindows *x-destroysubwindows*) map-window)

(defpacket (reparent-window *x-reparentwindow*)
  (opcode card8)
  (reqlen card16)
  (window rid)
  (parent rid)
  (x card16)
  (y card16))

(defpacket (change-window-attributes *x-changewindowattributes*)
  (opcode card8)
  (reqlen card16)
  (window rid)
  (bitmask card32)
  (listofvalues string))

(defpacket (get-window-attributes *x-getwindowattributes*) map-window)
