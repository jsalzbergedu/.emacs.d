;;; Themes

;; Set the size of the window:
(if (window-system) (set-frame-size (selected-frame) 189 46))

;; Transpose frames to get frames spawned on the bottom to the right
(require 'transpose-frame)

;; Split window horozontally
(setq split-height-threshold 0)
(setq split-width-threshold 1000)

;; If in stumpwm, use pop-up frames and doom-nova. Else, use default setting
;; for pop-up frames and use doom-one
(require 'doom-themes)
(require 'all-the-icons-dired)
(when (getenv "XDG_SESSION_DESKTOP")
  (if (string= (getenv "XDG_SESSION_DESKTOP") "stumpwm")
      (progn (setq pop-up-frames t)
	     (load-theme 'doom-nova t))
    (load-theme 'doom-one t)))
