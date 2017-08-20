;;; Themes

;; Clean up all the unnecessary visual elements
(set-fringe-mode (setq fringe-mode 0))
(set-scroll-bar-mode (setq scroll-bar-mode nil))
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq cursor-in-non-selected-windows nil)

;; Set the size of the window:
(if (window-system) (set-frame-size (selected-frame) 189 46))

;; Transpose frames to get frames spawned on the bottom to the right
(require 'transpose-frame)

;; Split window horozontally
(setq split-height-threshold 0)
(setq split-width-threshold 1000)

;; If in stumpwm, use pop-up frames and doom-nova
;; Else, use default setting for pop-up frames and use doom-one
(require 'doom-themes)
(require 'all-the-icons-dired)
(when (getenv "XDG_SESSION_DESKTOP")
  (if (string= (getenv "XDG_SESSION_DESKTOP") "stumpwm")
      (progn (setq pop-up-frames t)
	     (load-theme 'doom-nova t))
    (load-theme 'doom-one t)))

;; Set the font when in graphical mode
(set-face-attribute 'default nil :height 113 :family "Inconsolata" :foundry "PfEd")
(add-to-list 'default-frame-alist '(font-backend . "xft"))

;; Set a fallback font to make the box-building characters look right
(set-fontset-font "fontset-default" '(#x2502 . #x2502) "Inconsolata")

;; Set colors
(setq ansi-color-names-vector
      [("#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF")]
      fci-rule-color "#5B6268"
      jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef")
      jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65")
      jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a")
      vc-annotate-background "#1B2229"
      vc-annotate-color-map
      (list
       (cons 20 "#98be65")
       (cons 40 "#b4be6c")
       (cons 60 "#d0be73")
       (cons 80 "#ECBE7B")
       (cons 100 "#e6ab6a")
       (cons 120 "#e09859")
       (cons 140 "#da8548")
       (cons 160 "#d38079")
       (cons 180 "#cc7cab")
       (cons 200 "#c678dd")
       (cons 220 "#d974b7")
       (cons 240 "#ec7091")
       (cons 260 "#ff6c6b")
       (cons 280 "#cf6162")
       (cons 300 "#9f585a")
       (cons 320 "#6f4e52")
       (cons 340 "#5B6268")
       (cons 360 "#5B6268"))
      vc-annotate-very-old-color nil)
