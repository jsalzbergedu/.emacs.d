;; -*- lexical-binding: t -*-
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
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-indent-indentation-per-level 1)
(defun load-org-evil () "A function used to lazily load org-evil" (require 'org-evil))
(add-hook 'org-mode-hook 'load-org-evil)
(setq 
 org-ellipsis ":"
 org-fontify-done-headline t
 org-fontify-quote-and-verse-blocks t
 org-fontify-whole-heading-line t
 org-startup-indented t)
(evil-leader/set-key
  ;; Org Mode
  "<SPC> l" 'org-store-link
  "<SPC> c" 'org-capture
  "<SPC> a" 'org-agenda
  "<SPC> b" 'org-iswitchb)
(find-file-noselect "~/tmp/mathscratchpad.org")


;; Tramp
(setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-saves")
(add-to-list 'backup-directory-alist
	     (cons tramp-file-name-regexp nil))

;; PDF tools
(pdf-tools-install)
(setq
 pdf-view-continuous t
 pdf-view-display-size :fit-page)
(add-hook 'pdf-view-mode-hook
	  (lambda ()
	    (local-set-key (kbd "q") 'image-kill-buffer)
	    (local-set-key (kbd "h") 'pdf-view-previous-page)
	    (local-set-key (kbd "l") 'pdf-view-next-page)
	    (local-unset-key (kbd "<SPC>"))
	    (local-set-key (kbd "a a") 'ivy-switch-buffer)
	    (local-set-key (kbd "j") 'pdf-view-next-line-or-next-page)
	    (local-set-key (kbd "k") 'pdf-view-previous-line-or-previous-page)))

;; Info
(evil-define-key 'normal Info-mode-map
  (kbd "p") 'Info-prev)

;; Silver Searcher
(use-package ag
  :defer t)

;; mu4e setup taken from http://cachestocaches.com/2017/3/complete-guide-email-emacs-using-mu-and-/
(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e/"
  :config (setq user-mail-address "jssalzbe@ncsu.edu"
		  smtpmail-smtp-user "jssalzbe@ncsu.edu"
		  smtpmail-local-domain "gmail.com"
		  smtpmail-default-smtp-server "smtp.gmail.com"
		  smtpmail-smtp-server "smtp.gmail.com"
		  smtpmail-smtp-service 587
		  mu4e-contexts `( ,(make-mu4e-context
				     :name "Gmail"
				     :match-func (lambda (msg) (when msg
							    (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
				     :vars '((mu4e-trash-folder . "/Gmail/[Gmail].Trash")
					     (mu4e-refile-folder . "/Gmail/[Gmail].Archive"))))))

(defun advised-mu4e-headers-view-message (orig-fun)
  "View message at point.
If there's an existing window for the view, re-use that one. If
not, create a new one, depending on the value of
`mu4e-split-view': if it's a symbol `horizontal' or `vertical',
split the window accordingly; if it is nil, replace the current
window. "
  (interactive)
  (unless (eq major-mode 'mu4e-headers-mode)
    (mu4e-error "Must be in mu4e-headers-mode (%S)" major-mode))
  (let* ((msg (mu4e-message-at-point))
	  (docid (or (mu4e-message-field msg :docid)
		   (mu4e-warn "No message at point")))
	  ;; decrypt (or not), based on `mu4e-decryption-policy'.
	  (decrypt
	    (and (member 'encrypted (mu4e-message-field msg :flags))
	      (if (eq mu4e-decryption-policy 'ask)
		(yes-or-no-p (mu4e-format "Decrypt message?"))
		mu4e-decryption-policy)))
					;(viewwin (mu4e~headers-redraw-get-view-window)))
	  )
    ;(unless (window-live-p viewwin)
    ;  (mu4e-error "Cannot get a message view"))
    ;(select-window viewwin)
    ;(switch-to-buffer (mu4e~headers-get-loading-buf))
    (mu4e~proc-view docid mu4e-view-show-images decrypt)
    (when (boundp 'mu4e~view-buffer)
      (ignore-errors (make-indirect-buffer mu4e~view-buffer "*mu4e-view-all*"))))
  )

(advice-add 'mu4e-headers-view-message :around #'advised-mu4e-headers-view-message)
(defun switch-to-buffer-just-work ()
  "This gosh darn thing just needs to work"
  (interactive)
  (switch-to-buffer "*mu4e-headers*"))
(advice-add 'mu4e-headers-view-message :after #'(lambda () (call-interactively 'switch-to-buffer-just-work)))



(use-package evil-mu4e)
;; ERC irc client
(setq erc-autojoin-channels-alist (list (cons "freenode.net" (list "#stratis-storage" "#scheme")) (cons "mozilla.org" (list "#rust" "#rust-beginners" "#servo"))))
(setq erc-prompt (concat "<jcob>:"))
(defun my-erc-connect () 
  "Connect to the IRC servers I usually connect to"
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "jcob" :full-name "Jacob Salzberg")
  (erc :server "irc.mozilla.org" :port 6667 :nick "jcob" :full-name "Jacob Salzberg"))

;; Term
(use-package term+)
