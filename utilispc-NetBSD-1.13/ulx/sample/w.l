;;;
;;;
;;;

(setq display (open-display (get-display-host)))
(setq screen (first (display-roots display)))
(setq root (screen-root screen))
(setq white (screen-white-pixel screen)
      black (screen-black-pixel screen))

(setq base (create-window :parent root :x 0 :y 0 :width 320 :height 320
			  :background white :class :input-output))
(do ((y 0 (1+ y)))
    ((> y 8))
    (do ((x 0 (1+ x)))
	((> x 8))
	(create-window :parent base :x (* x 40) :y (* y 40)
		       :width 39 :height 39 :background white
		       :border-width 1)))

(map-subwindows base)
(map-window base)

(display-force-output display)
