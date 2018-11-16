;; -*- lexical-binding: t -*-
;;; Applications in Emacs

;; Scratch bufffer
(setq initial-scratch-message
      (concat ";; -*- lexical-binding: t -*-\n" initial-scratch-message))
(add-hook 'finalize (lambda () (let ((current-buffer (current-buffer)))
                            (set-buffer "*scratch*")
                            (setq lexical-binding t)
                            (set-buffer current-buffer))))

;; Passwords
(use-package passwords
  :straight nil
  :load-path "passwords/"
  :demand t)

;; Chronos
(use-package chronos
  :defer t
  :straight (chronos :type git
                     :host github
                     :repo "dxknight/chronos")
  :config (progn (evil-set-initial-state 'chronos-mode 'emacs)
		 (setq chronos-expiry-functions '(chronos-buffer-notify
						  chronos-desktop-notifications-notify
						  ;;chronos-sound-notify
                                                  ))
		 chronos-shell-notify-program "mplayer -ao pulse /home/jacob/Music/echoed-ding.ogg")
  :init (global-set-key (kbd "C-c t t")  'chronos-add-timer))

;; Dired-subtree
(use-package dired-subtree
  :straight (dired-hacks :type git
                         :host github
                         :repo "Fuco1/dired-hacks")
  :demand t
  :config (progn (defun reset-i ()
		   "Reset i from dired-maybe-insert-subdir to dired-subtree-insert."
		   (local-unset-key (kbd "i"))
		   (local-set-key (kbd "i") 'dired-subtree-insert))
		 (add-hook 'dired-mode-hook 'reset-i)))


;; Neotree (use-package all-the-icons)
(use-package neotree
  :straight (neotree :type git
                     :host github
                     :repo "jaypei/emacs-neotree")
  :defer t
  :config
  (global-set-key [f8] #'(lambda ()
                           (interactive)
                           (with-temp-buffer
                             (when projectile-project-root
                               (cd projectile-project-root))
                             (neotree-toggle))))
  (setq neo-theme 'ascii)
  (add-hook 'neotree-mode-hook (lambda ()
				 (setq neo-window-width 30)
				 (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
				 (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
				 (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
				 (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))

;; Org
(use-package git
  :straight t
  :defer t)

(defun org-git-version ()
  "The Git version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of org-mode.
Inserted by installing org-mode or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

(use-package org
  :defer t
  :straight nil
  :init
  (straight-use-package 'org-plus-contrib)
  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  :config
  (setq org-indent-indentation-per-level 1
	org-ellipsis ":"
	org-fontify-done-headline t
	org-fontify-quote-and-verse-blocks t
	org-fontify-whole-heading-line t
	org-startup-indented t
        org-src-fontify-natively t)
  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
                                                           (emacs-lisp . t)
                                                           (scheme . t)
                                                           (scala . t)
                                                           (coq . t)
                                                           (haskell . t)))
  :commands org-mode)

(use-package ob-plantuml
  :init
  (setq org-plantuml-jar-path "/opt/plantuml/plantuml.jar")
  :straight nil
  :demand t
  :after org)

(use-package org-evil
  :straight (org-evil :type git
                      :host github
                      :repo "GuiltyDolphin/org-evil")
  :demand t
  :after org)

(use-package org-evil-motion
  :demand t
  :after org-evil)

;; Tramp
(use-package tramp
  :straight t
  :defer t
  :config
  (setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-saves")
  (add-to-list 'backup-directory-alist
	       (cons tramp-file-name-regexp nil)))


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


;; Silver Searcher
(use-package ag
  :straight (ag :type git
                :host github
                :repo "Wilfred/ag.el")
  :defer t)

;; ERC irc client
(use-package erc
  :straight t
  :init
  (setq erc-autojoin-channels-alist (list (cons "freenode.net" (list "#stratis-storage" "#scheme")) (cons "mozilla.org" (list "#rust" "#rust-beginners" "#servo"))))
  (setq erc-prompt (concat "<jcob>:"))
  (defun my-erc-connect () 
    "Connect to the IRC servers I usually connect to"
    (interactive)
    (erc :server "irc.freenode.net"
         :port 6667
         :nick "jcob"
         :password (passwords-get 'irc)
         :full-name "Jacob Salzberg")
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
        erc-track-minor-mode t))



;; Term
(use-package ansi-term
  :straight nil
  :defer t
  :config (add-hook 'term-mode-hook (lambda () 
				      (evil-local-set-key 'normal (kbd "p") 'term-paste))))


;; Gitter
(use-package gitter
  :defer t
  :straight (gitter :type git
                    :host github
                    :repo "xuchunyang/gitter.el")
  :init (setq gitter-token (passwords-get 'gitter)))
