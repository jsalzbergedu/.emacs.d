;; -*- lexical-binding: t -*-
;;; Themes

;; Clean up all the unnecessary visual elements
(set-fringe-mode (setq fringe-mode 0))
(setq fringes-outside-margins t) ; for when they're necessary
(set-scroll-bar-mode (setq scroll-bar-mode nil))
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq cursor-in-non-selected-windows nil)

;; Set the size of the window:
(if (window-system) (set-frame-size (selected-frame) 189 46))

;; Transpose frames to get frames spawned on the bottom to the right
(use-package transpose-frame
  :straight t
  :demand t)

;; If in stumpwm, use pop-up frames and doom-nova
;; Else, use default setting for pop-up frames and use doom-one
;;(require 'doom-themes)
;; (require 'all-the-icons-dired)
;; (when (getenv "xdg_session_desktop")
;;   (if (string= (getenv "xdg_session_desktop") "stumpwm")
;;       (progn (setq pop-up-frames t)
;; 	     (load-theme 'doom-nova t))
;;     (load-theme 'doom-one t)))

;; Set the font when in graphical mode
(set-face-attribute 'default nil :height 113 :family "Inconsolata" :foundry "PfEd")
(add-to-list 'default-frame-alist '(font-backend . "xft"))

;; Set a fallback font to make the box-building characters look right
(defun fix-box-building (&optional frame)
  "Sets a fallback font to make the box-building characters look right"
  (set-fontset-font "fontset-default" '(#x2502 . #x2502) "Fira Mono" frame)
  (redisplay t))
(add-hook 'after-make-frame-functions 'fix-box-building)
(add-hook 'window-setup-hook 'fix-box-building)

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

;; Make mode-line look better
(set-face-attribute 'mode-line-inactive nil :background "#3c4c55" :foreground "#899ba6")
(set-face-attribute 'mode-line nil :background "#3c4b53 ")

;; Emoji
(use-package emojify
  :straight (emojify :type git
                     :host github
                     :repo "iqbalansari/emacs-emojify"
                     :files ("data" "emojify.el"))
  :demand t
  :init (setq emoji-emojify-styles '(unicode))
  :config (global-emojify-mode))

;; ;; Mode line
(use-package rich-minority
  :straight (rich-minority :type git
                           :host github
                           :repo "Malabarba/rich-minority")
  :demand t)

(use-package smart-mode-line
  :straight (smart-mode-line :type git
                             :host github
                             :repo "Malabarba/smart-mode-line")
  :demand t
  :config
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'light)
  (display-time-mode 1))

;; Use nice looking colors
(use-package flatui-theme
  :straight (flatui-theme :type git
                          :host github
                          :repo "john2x/flatui-theme.el")
  :demand t
  :config (load-theme 'flatui t))

;; ;; Hide mode line
;; (defvar mode-line-storage nil)
;; (make-variable-buffer-local 'mode-line-storage)

;; (defun toggle-mode-line-off ()
;;   "Toggles the mode line off"
;;   (setq mode-line-storage mode-line-format)
;;   (setq mode-line-format nil))

;; (defun toggle-mode-line-toggler ()
;;   "Toggles the mode-line"
;;   (if mode-line-format
;;       (toggle-mode-line-off)
;;     (setq mode-line-format mode-line-storage)
;;     (setq mode-line-storage nil))) 

;; (defun toggle-mode-line (&optional arg)
;;   "Stores the current mode-line-format in nil, toggles the mode-line.
;; If called with a negative argument, simply disable the mode-line."
;;   (interactive)
;;   (if arg
;;       (if (> arg 0)
;; 	  (toggle-mode-line-toggler)
;; 	(when mode-line-format (toggle-mode-line-off)))
;;     (toggle-mode-line-toggler)))

;; ;; (evil-leader/set-key "m" 'toggle-mode-line) -- we'll get back to this when we seperate it

;; (define-minor-mode hide-mode-line
;;   "A mode to hide the mode-line"
;;   nil
;;   nil
;;   nil)

;; (define-globalized-minor-mode global-hide-mode-line
;;   hide-mode-line
;;   (lambda () "Turn off mode line" (toggle-mode-line -1)))

;; (global-hide-mode-line 1)

