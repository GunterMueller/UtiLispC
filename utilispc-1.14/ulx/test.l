

;;; DISPLAY environment var kara host mei wo totte kuru.
(setq display-host (get-display-host))
(or display-host (progn (format "Display Unknown/n") (err:argument-type)))

(setq display (open-display display-host))

(format "Connected: stream = /s/n" (display-input-stream display))
(setq screen (display-default-screen display))

(setq white (screen-white-pixel screen)
      black (screen-black-pixel screen))
(setq fg black
      bg white)

(setq root (screen-root screen))

;; force buffer flush after each request.
(setf (display-after-function display) 'display-force-output)

(setq window (create-window :parent root :x 0 :y 0 :width 400 :height 200
			    :background bg :class :input-output))

(setf (wm-name window) "ULX test")
(setf (window-event-mask window) '(:button-press))
(map-window window)

;;(setq font (open-font display "-*-helvetica-medium-r-*-240-*"))
(setq gc (create-gcontext :drawable window :foreground fg :background bg))

(defun draw-something ()
  (draw-line window gc 0 0 300 300)
  (draw-rectangle window gc 30 30 200 100 nil)
  (do ((i 0 (+ i 20)))
      ((> i 200))
      (draw-arc window gc i i (+ 20 i) (+ 20 i) 0.0 360.0 nil)))


(defun repaint ()
  (draw-something)
  (setf (window-event-mask window) '(:button-press :exposure))
  (event-case (display :discard-p t)
    (:button-press
     (event-window x y)
     (draw-arc window gc (- x 10) (- y 10) 20 20 0.0 360.0 t)
     nil)
    (:exposure
     (count)
     (and (zerop count) (draw-something))
     nil)
    (otherwise
     ()
     (print "other event")
     nil))
  )
		
