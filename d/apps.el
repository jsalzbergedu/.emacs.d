;;; Applications in Emacs

;; Magit
(use-package magit
  :defer t
  :config (require 'evil-magit))

;; Chronos
(use-package chronos
  :defer t
  :config (progn (evil-set-initial-state 'chronos-mode 'emacs)
		 (setq chronos-expiry-functions '(chronos-buffer-notify
						  chronos-desktop-notifications-notify
						  chronos-sound-notify))
		 chronos-notification-wav "~/Music/echoed-ding.wav"
		 chronos-shell-notify-program "mplayer -ao pulse /home/jacob/Music/echoed-ding.ogg")
  :bind (("C-c t t" . chronos-add-timer)))

;; Dired-subtree
(use-package dired-subtree
  :demand t
  :config (progn (defun reset-i ()
		   "Reset i from dired-maybe-insert-subdir to dired-subtree-insert."
		   (local-unset-key (kbd "i"))
		   (local-set-key (kbd "i") 'dired-subtree-insert))
		 (add-hook 'dired-mode-hook 'reset-i)))


;; Neotree
(use-package neotree
  :defer t
  :config (progn (global-set-key [f8] 'neotree-toggle)
		 (doom-themes-neotree-config)
		 (add-hook 'neotree-mode-hook (lambda ()
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
	       (setq stumpwm-shell-program (concat (getenv "HOME") "/.stumpwm.d/stumpwm-contrib/util/stumpish/stumpish"))))

(require 'stumpwm-utils "/home/jacob/AUR/stumpwm-contrib/util/swm-emacs/stumpwm-utils")
(when (getenv "XDG_CURRENT_DEKSTOP") ;; - set pop up frames to t when the window manager is stumpwm
  (when (string= (getenv "XDG_CURRENT_DESKTOP") "stumpwm")
    (setq pop-up-frames t)))

;; Org
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-indent-indentation-per-level 1)
(defun load-org-evil () "A function used to lazily load org-evil" (require 'org-evil))
(add-hook 'org-mode-hook 'load-org-evil)
(setq 
 org-ellipsis "  "
 org-fontify-done-headline t
 org-fontify-quote-and-verse-blocks t
 org-fontify-whole-heading-line t
 org-startup-indented t)

;; Tramp
(setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-saves")
(add-to-list 'backup-directory-alist
	     (cons tramp-file-name-regexp nil))

;; Doc View
(doc-view-minor-mode 1)
(setq
 doc-view-continuous t
 doc-view-resolution 200)
