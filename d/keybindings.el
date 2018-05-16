;; -*- lexical-binding: t -*-
;;; Evil
(defun begone ()
  "Kills the buffer and the window"
  (interactive)
  (kill-this-buffer)
  (delete-window))

(use-package evil
  :demand t
  :bind (("C-c a q" . quit-window)
	 :map evil-normal-state-map
	 ("SPC" . nil)
	 ("SPC l" . windmove-cf-right)
	 ("SPC h" . windmove-cf-left)
	 ("SPC k" . windmove-cf-up)
	 ("SPC j" . windmove-cf-down)
	 ("SPC q" . begone)
	 ("SPC a" . switch-to-buffer)
	 :map evil-motion-state-map
	 ("SPC" . nil)
	 ("SPC l" . windmove-cf-right)
	 ("SPC h" . windmove-cf-left)
	 ("SPC k" . windmove-cf-up)
	 ("SPC j" . windmove-cf-down)
	 ("SPC q" . begone)
	 ("SPC a" . switch-to-buffer)))

;; Have to unset space in many packages
(use-package dired
  :demand t
  :bind (:map dired-mode-map
	      ("SPC" . nil)))

(use-package compile
  :demand t
  :bind (:map compilation-mode-map
	      ("SPC" . nil)))

(use-package info
  :defer t
  ;; For some reason Info must be brute forced here 
  :config (substitute-key-definition 'Info-scroll-up nil Info-mode-map)) 

;; Either move across emacs windows or stumpwm windows
(defun windmove-plain (dir)
  "Moves window focus in direction DIR"
  (interactive "sDIR: ")
  (cond
   ((string= "up" dir) (windmove-up))
   ((string= "down" dir) (windmove-down))
   ((string= "left" dir) (windmove-left))
   ((windmove-right) t)
   (t nil)))

(defun stump-move (dir)
  "Move stumpwm focus in direction DIR"
  (interactive "sDIR: ")
  (make-process :name "windmove" :buffer nil :command (list "~/.stumpwm.d/stumpwm-contrib/util/stumpish/stumpish" (concat "move-focus " dir))))


(defun windmove-or-change-focus (dir)
  "Windmoves or moves stumpwms focus in direction DIR"
  (interactive "sDIR: ")
  (unless (ignore-errors (windmove-plain dir))
    (stump-move dir)))

(defun windmove-cf-right ()
  (interactive)
  "Move or window or stumpwm focus right"
  (windmove-or-change-focus "right"))

(defun windmove-cf-left ()
  (interactive)
  "Move or window or stumpwm focus left"
  (windmove-or-change-focus "left"))

(defun windmove-cf-up ()
  (interactive)
  "Move or window or stumpwm focus up"
  (windmove-or-change-focus "up"))

(defun windmove-cf-down ()
  (interactive)
  "Move or window or stumpwm focus down"
  (windmove-or-change-focus "down"))


(add-hook 'finalize (lambda ()
		       (evil-mode 1)
		       (global-undo-tree-mode)))

;; Ivy, Swiper and Council, and flx:
(use-package swiper
  :bind ("C-s" . swiper))

;; Flx for ivy searching
(use-package flx
  :defer t)

(use-package flx-ido
  :defer t)

(use-package ivy
  :demand t
  :config (progn (setq ivy-use-virtual-buffers t
		       enable-recursive-minibuffers t
		       ivy-re-builders-alist '((t . ivy--regex-fuzzy))))
  :after flx
  :bind ("C-c C-r" . ivy-resume)
  :commands ivy-mode)

(use-package hydra
  :demand t)

(use-package ivy-hydra
  :demand t)

(ivy-mode 1)

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file)
	 ("C-h u" . counsel-unicode-char)
	 ("C-c g" . counsel-git)
	 ("C-c j" . counsel-git-grep)
	 ("C-c k" . counsel-ag)
	 ("C-c l" . counsel-locate)
	 ("C-S-o" . counsel-rhythmbox)
	 :map evil-normal-state-map
	 ("SPC f" . counsel-find-file)
	 :map evil-motion-state-map
	 ("SPC f" . counsel-find-file)))

;;; Other keybindings

;; q in evil:
(global-set-key (kbd "C-c a q") 'quit-window)
