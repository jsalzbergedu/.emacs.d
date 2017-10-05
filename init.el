;; -*- lexical-binding: t -*-
;; Initialize package, the package manager of emacs
(package-initialize)
;; Load use-package, a lazy loader and configurer for packages
;;(require 'use-package)
(defvar finalize (list) "A list of functions called after emacs initializes")
(load "~/.emacs.d/d/themes")
(load "~/.emacs.d/d/keybindings")
(load "~/.emacs.d/d/programming")
(load "~/.emacs.d/d/apps")
(setq  
 package-archives
   (quote
    (("melpa" . "http://melpa.org/packages/")
     ("melpa-stable" . "https://stable.melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("all-the-icons" . "~/AUR/all-the-icons.el/")))
 package-enable-at-startup t
 package-selected-packages
   (quote
    (paredit scheme-complete geiser ensime org-evil slime stumpwm-mode eclimd js3 ps-ccrypt flx elpy ivy-hydra counsel swiper ivy indent-tools i3wm dired-subtree dired-hacks-utils workgroups2 cargo package-build package-lint toml-mode company-arduino column-marker markdown-mode evil-magit evil-surround with-editor auctex sound-wav chronos magit yatex nhexl-mode js3-mode transpose-frame eww-lnum flycheck-rust flycheck racer company-racer helm use-package evil-leader goto-chg evil pdf-tools rust-mode neotree nlinum paradox all-the-icons-dired doom-themes ##))
   safe-local-variable-values (quote ((gieser-scheme-implementation . chicken))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-autojoin-mode t)
 '(erc-button-mode t)
 '(erc-fill-mode t)
 '(erc-irccontrols-mode t)
 '(erc-list-mode t)
 '(erc-match-mode t)
 '(erc-menu-mode t)
 '(erc-move-to-prompt-mode t)
 '(erc-netsplit-mode t)
 '(erc-networks-mode t)
 '(erc-noncommands-mode t)
 '(erc-pcomplete-mode t)
 '(erc-readonly-mode t)
 '(erc-ring-mode t)
 '(erc-stamp-mode t)
 '(erc-track-minor-mode t)
 '(erc-track-mode t)
 '(package-selected-packages
   (quote
    (term+ evil-mu4e json-mode ag meson-mode cmake-mode company-rtags noflet toml paredit scheme-complete geiser ensime org-evil slime stumpwm-mode eclimd js3 ps-ccrypt flx elpy ivy-hydra counsel swiper ivy indent-tools i3wm dired-subtree dired-hacks-utils workgroups2 cargo package-build package-lint toml-mode company-arduino column-marker markdown-mode evil-magit evil-surround with-editor auctex sound-wav chronos magit yatex nhexl-mode js3-mode transpose-frame eww-lnum flycheck-rust flycheck racer company-racer helm use-package evil-leader goto-chg evil pdf-tools rust-mode neotree nlinum paradox all-the-icons-dired doom-themes ##)))
 '(send-mail-function (quote smtpmail-send-it))
 '(tramp-syntax (quote default) nil (tramp)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil)))
 '(bg:erc-color-face0 ((t (:background "#516773"))))
 '(bg:erc-color-face1 ((t (:background "#2f3b41"))))
 '(bg:erc-color-face10 ((t (:background "#7392a3"))))
 '(bg:erc-color-face11 ((t (:background "#7594dc"))))
 '(bg:erc-color-face12 ((t (:background "#4f64aa"))))
 '(bg:erc-color-face13 ((t (:background "#d97482"))))
 '(bg:erc-color-face2 ((t (:background "#4f64ff"))))
 '(bg:erc-color-face3 ((t (:background "#26a136"))))
 '(bg:erc-color-face4 ((t (:background "#ff4c55"))))
 '(bg:erc-color-face5 ((t (:background "9e2f35"))))
 '(bg:erc-color-face6 ((t (:background "#a94ca9"))))
 '(bg:erc-color-face7 ((t (:background "#ad8c66"))))
 '(bg:erc-color-face8 ((t (:background "#c2c283"))))
 '(bg:erc-color-face9 ((t (:background "#3cff55"))))
 '(bold ((t (:weight bold :foundry "CYRE" :family "Inconsolata"))))
 '(bold-italic ((t (:inherit (bold italic) :family "Inconsolata LGC"))))
 '(erc-current-nick-face ((t (:foreground "#83afe5" :weight bold))))
 '(erc-dangerous-host-face ((t (:foreground "#d18ec2"))))
 '(erc-direct-msg-face ((t (:foreground "#9a93e1"))))
 '(erc-error-face ((t (:foreground "#d18ec2"))))
 '(erc-input-face ((t (:foreground "#b8c3c9"))))
 '(erc-keyword-face ((t (:foreground "#a8ce93" :weight bold))))
 '(erc-my-nick-face ((t (:foreground "#f2c38f" :weight bold))))
 '(erc-nick-msg-face ((t (:foreground "#d18ec2" :weight bold))))
 '(erc-prompt-face ((t (:foreground "#d18ec2" :weight bold))))
 '(fg:erc-color-face0 ((t (:foreground "#899ba6"))))
 '(fg:erc-color-face1 ((t (:foreground "#808080"))))
 '(fg:erc-color-face10 ((t (:foreground "#7fc1ca"))))
 '(fg:erc-color-face11 ((t (:foreground "#7fc1ca"))))
 '(fg:erc-color-face12 ((t (:foreground "#83afe5"))))
 '(fg:erc-color-face13 ((t (:foreground "#9a93e1"))))
 '(fg:erc-color-face14 ((t (:foreground "#6f7f88"))))
 '(fg:erc-color-face15 ((t (:foreground "#899ba6"))))
 '(fg:erc-color-face2 ((t (:foreground "#83afe5"))))
 '(fg:erc-color-face3 ((t (:foreground "#a8ce93"))))
 '(fg:erc-color-face4 ((t (:foreground "#d18ec2"))))
 '(fg:erc-color-face5 ((t (:foreground "#d18ec2"))))
 '(fg:erc-color-face6 ((t (:foreground "#9a93e1"))))
 '(fg:erc-color-face7 ((t (:foreground "#f2c38f"))))
 '(fg:erc-color-face8 ((t (:foreground "#dada93"))))
 '(fg:erc-color-face9 ((t (:foreground "#a8ce93"))))
 '(font-lock-comment-face ((t (:foreground "#899BA6" :foundry "PfEd" :family "Inconsolata"))))
 '(italic ((t (:slant italic :foundry "PfEd" :family "Inconsolata LGC"))))
 '(org-level-1 ((t (:background "#3C4C55" :foreground "#83AFE5" :weight bold :height 1.2)))))

(find-file-noselect "~/.emacs.d/init.el") ;; I have ~/.emacs.d/.emacs softlinked to ~/.emacs
(with-eval-after-load 'init.el (mapc 'funcall finalize))
(provide 'init.el)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
