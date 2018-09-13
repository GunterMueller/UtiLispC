(setq s (display-default-screen display))
(setq cm (screen-default-colormap s))
(setq b (first (lookup-color cm "black")))
(setq w (first (lookup-color cm "white")))
(setq font (open-font display "cursor"))

(setq cursor (create-glyph-cursor
	      :source-font font
	      :source-char 0
	      :foreground b
	      :background w))
