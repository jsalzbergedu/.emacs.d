;; -*- lexical-binding: t -*-
;;; Applications in Emacs

;; Scratch bufffer
(setq initial-scratch-message
      (concat ";; -*- lexical-binding: t -*-\n" initial-scratch-message))
(add-hook 'finalize (lambda () (let ((current-buffer (current-buffer)))
                            (set-buffer "*scratch*")
                            (setq-default lexical-binding t)
                            (set-buffer current-buffer))))

;; Passwords
(use-package passwords
  :load-path "passwords/"
  :demand t)

;; Chronos
(use-package chronos
  :defer t
  :config (progn (evil-set-initial-state 'chronos-mode 'emacs)
		 (setq chronos-expiry-functions '(chronos-buffer-notify
						  chronos-desktop-notifications-notify
						  chronos-sound-notify))
		 chronos-notification-wav "~/Music/echoed-ding.wav"
		 chronos-shell-notify-program "mplayer -ao pulse /home/jacob/Music/echoed-ding.ogg")
  :init (global-set-key (kbd "C-c t t")  'chronos-add-timer))

;; Dired-subtree
(use-package dired-subtree
  :demand t
  :config (progn (defun reset-i ()
		   "Reset i from dired-maybe-insert-subdir to dired-subtree-insert."
		   (local-unset-key (kbd "i"))
		   (local-set-key (kbd "i") 'dired-subtree-insert))
		 (add-hook 'dired-mode-hook 'reset-i)))


;; Neotree
(use-package all-the-icons)
(set-face-attribute 'vertical-border nil :foreground "#899ba6") 
(use-package neotree
  :defer t
  :config (progn (global-set-key [f8] 'neotree-toggle)
		 (doom-themes-neotree-config)
		 (add-hook 'neotree-mode-hook (lambda ()
						(setq neo-window-width 30)
						(define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
					        (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
						(define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
						(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))))

;; Ccrypt
(if (file-exists-p "~/.emacs.d/ps-ccrypt/ps-ccrypt.el")
    (require 'ps-ccrypt "~/.emacs.d/ps-ccrypt/ps-ccrypt"))

;; Stumpwm
(use-package stumpwm-mode
  :defer t
  :init (progn (autoload 'stumpwm-mode "stumpwm-mode" nil t)
	       (when (file-exists-p "~/.stumpwm.d/stumpwm-contrib/util/stumpish/stumpish")
	       (setq stumpwm-shell-program (concat (getenv "HOME") "/.stumpwm.d/stumpwm-contrib/util/stumpish/stumpish")))))

(when (file-exists-p "~/.stumpwm.d/stumpwm-contrib/util/swm-emacs/stumpwm-utils")
  (require 'stumpwm-utils "/home/jacob/.stumpwm.d/stumpwm-contrib/util/swm-emacs/stumpwm-utils"))

(when (getenv "XDG_CURRENT_DEKSTOP") ;; - set pop up frames to t when the window manager is stumpwm
  (when (string= (getenv "XDG_CURRENT_DESKTOP") "stumpwm")
    (setq pop-up-frames t)))

;; Org
(use-package org
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  :config (setq org-indent-indentation-per-level 1
		org-ellipsis ":"
		org-fontify-done-headline t
		org-fontify-quote-and-verse-blocks t
		org-fontify-whole-heading-line t
		org-startup-indented t)
  :commands org-mode)

(use-package org-evil
  :after org)

;; (find-file-noselect "~/tmp/mathscratchpad.org")


;; Tramp
(setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-saves")
(add-to-list 'backup-directory-alist
	     (cons tramp-file-name-regexp nil))

;; PDF tools
;;(pdf-tools-install)
;; (setq
;;  pdf-view-continuous t
;;  pdf-view-display-size :fit-page)
;; (add-hook 'pdf-view-mode-hook
;; 	  (lambda ()
;; 	    (local-set-key (kbd "q") 'image-kill-buffer)
;; 	    (local-set-key (kbd "h") 'pdf-view-previous-page)
;; 	    (local-set-key (kbd "l") 'pdf-view-next-page)
;; 	    (local-unset-key (kbd "<SPC>"))
;; 	    (local-set-key (kbd "a a") 'ivy-switch-buffer)
;; 	    (local-set-key (kbd "j") 'pdf-view-next-line-or-next-page)
;; 	    (local-set-key (kbd "k") 'pdf-view-previous-line-or-previous-page)))

;; Info
(evil-define-key 'normal Info-mode-map
  (kbd "p") 'Info-prev)

;; Silver Searcher
(use-package ag
  :defer t)

;; ERC irc client
(setq erc-autojoin-channels-alist (list (cons "freenode.net" (list "#stratis-storage" "#scheme")) (cons "mozilla.org" (list "#rust" "#rust-beginners" "#servo"))))
(setq erc-prompt (concat "<jcob>:"))
(defun my-erc-connect () 
  "Connect to the IRC servers I usually connect to"
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "jcob" :full-name "Jacob Salzberg")
  (erc :server "irc.mozilla.org" :port 6667 :nick "jcob" :full-name "Jacob Salzberg"))

(setq erc-autojoin-mode t
      erc-button-mode t
      erc-fill-mode t
      erc-irccontrols-mode t
      erc-list-mode t
      erc-match-mode t
      erc-menu-mode t
      erc-move-to-prompt-mode t
      erc-netsplit-mode t
      erc-networks-mode t
      erc-noncommands-mode t
      erc-pcomplete-mode t
      erc-readonly-mode t
      erc-ring-mode t
      erc-stamp-mode t
      erc-track-minor-mode t)

;; Ag
(use-package ag)

;; Term
(use-package ansi-term
  :defer t
  :config (add-hook 'term-mode-hook (lambda () 
				      (evil-local-set-key 'normal (kbd "p") 'term-paste))))


;; Package update
(use-package auto-package-update
  :defer t
  :commands auto-package-update-now)

;; Gitter
(use-package gitter
  :defer t
  :init (setq gitter-token (passwords-get 'gitter)))
