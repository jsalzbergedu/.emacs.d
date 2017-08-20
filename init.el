;; Initialize package, the package manager of emacs
(package-initialize)
;; Load use-package, a lazy loader and configurer for packages
;;(require 'use-package)
(load "~/.emacs.d/d/themes")
(load "~/.emacs.d/d/keybindings")
(load "~/.emacs.d/d/programming")
(load "~/.emacs.d/d/apps")
(setq  
 package-archives
   (quote
    (("melpa" . "http://melpa.org/packages/")
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
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t nil)))
 '(bold ((t (:weight bold :foundry "CYRE" :family "Inconsolata"))))
 '(bold-italic ((t (:inherit (bold italic) :family "Inconsolata LGC"))))
 '(font-lock-comment-face ((t (:foreground "#899BA6" :foundry "PfEd" :family "Inconsolata"))))
 '(italic ((t (:slant italic :foundry "PfEd" :family "Inconsolata LGC"))))
 '(org-level-1 ((t (:background "#3C4C55" :foreground "#83AFE5" :weight bold :height 1.2)))))

(find-file-noselect "~/.emacs.d/init.el") ;; I have ~/.emacs.d/.emacs softlinked to ~/.emacs
(provide 'init.el)
;;; init.el ends here
