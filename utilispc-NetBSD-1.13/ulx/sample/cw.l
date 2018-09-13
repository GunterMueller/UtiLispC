;;;
;;;
;;;
(defun main (host)
  (setq display (open-display host))
  (let* ((screen (first (display-roots display)))
	 (white (screen-white-pixel screen))
	 (black (screen-black-pixel screen))
	 (root (screen-root screen))
	 (gc (create-gcontext :drawable root :foreground black
			      :background white))
	 (window (create-window :parent root :x 0 :y 0 :width 400 :height 400
				:background white :class :input-output)))
    (setf (window-event-mask window) '(:exposure :button-press))
    (map-window window)
    (loop
     (and (eq 'end (event-case (display)
		     (:button-press
		      ()
		      (close-display display)
		      (setq display nil)
		      'end)
		       (:exposure
			(count)
			(and (= count 0)
			     (draw-on-window window gc))
			t)))
	  (return)))
    ))
(defun draw-on-window (window gc)
  (setf (gcontext-function gc) boole-xor)
  (clear-area window :x 0 :y 0 :width 400 :height 400)
  (do ((x 0 (+ x 2)))
      ((> x 400))
      (draw-line window gc x 0 (- 400 x) 400))
  (display-force-output display))

