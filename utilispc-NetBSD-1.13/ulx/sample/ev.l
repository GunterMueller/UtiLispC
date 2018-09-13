;;
;;

(defun getw (win sym)
  (let ((w (assq sym (window-plist win))))
    (and w (second w))))
(defmacro win-width (win) `(getw ,win 'width))
(defmacro win-height (win) `(getw ,win 'height))

(defun set-window-plist ()
    (mapcar *window-list*
	    (function (lambda (w)
			(setf (window-plist w)
			      `((width ,(drawable-width w))
				(height ,(drawable-height w))))))))

(defun ev ()
  (setq display-host (get-display-host))
  (and (boundp 'display) display (close-display display))
  (setq display (open-display display-host))
  (lets ((screen (first (display-roots display)))
	 (white (screen-white-pixel screen))
	 (black (screen-black-pixel screen))
	 (root (screen-root screen))
	 (highlight-gc (create-gcontext :drawable root :foreground black
					:background white))
	 (dehighlight-gc (create-gcontext :drawable root))
	 (window (create-window :parent root :x 0 :y 0 :width 400 :height 400
				:background white :class :input-output)))
    (setf (wm-name window) "ULX test")
    (comment setf (window-event-mask window)
	     '(:exposure :button-press :enter-window :leave-window
			 :structure-notify))
    (setf (window-event-mask window) '(:structure-notify))
    (setf (gcontext-line-width highlight-gc) 3)
    (copy-gcontext highlight-gc dehighlight-gc)
    (setf (gcontext-foreground dehighlight-gc) white)
    
    (lets ((width (- (drawable-width window) 2))
	   (height (- (drawable-width window) 2))
	   (halfw (// width 2)))
      (setq *window-list*
	    (mapcar `((0 0 ,(- halfw 2) ,height)
		      (,halfw 0 ,halfw ,height))
		    #'(lambda (l)
			(let ((w (create-window :parent window
						:x (first l)
						:y (second l)
						:width (third l)
						:height (fourth l)
						:background white
						:border-width 1)))
			  (setf (window-event-mask w)
				'(:button-press
				  :enter-window
				  :leave-window))
			  w)))))
    (set-window-plist)

    (map-subwindows window)
    (map-window window)
    (loop
     (and (eq 'aho
	      (event-case (display)
		(:button-press
		 ()
		 (close-display display)
		 (setq display nil)
		 'aho)
		(:exposure
		 (count)
		 nil
		 t)
		(:configure-notify
		 (width height)
		 (print "configure-notify")
		 (lets ((width (- width 2))
			(height (- height 2))
			(halfw (// width 2))
			(win1 (first *window-list*))
			(win2 (second *window-list*)))
		   (setf (drawable-width win1) (- halfw 2))
		   (setf (drawable-height win1) height)
		   (setf (drawable-width win2) halfw)
		   (setf (drawable-height win2) height)
		   (setf (drawable-x win2) halfw)
		 
		   (set-window-plist))
		 t)
		(:enter-notify
		 (event-window)
		 (let ((width (win-width event-window))
		       (height (win-height event-window)))
		   (draw-rectangle event-window
				   highlight-gc
				   1 1 (- width 2) (- height 2)))
		 t)
		(:leave-notify
		 (event-window)
		 (let ((width (win-width event-window))
		       (height (win-height event-window)))
		   (draw-rectangle event-window
				   dehighlight-gc
				   1 1 (- width 2) (- height 2)))
		 t)))
	  (exit)))
    )))
