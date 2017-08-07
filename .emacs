;; Initialize package, the package manager of emacs
(package-initialize)
;; Load use-package, a lazy loader and configurer for packages
(require 'use-package)
(load "~/.emacs.d/d/themes")
(load "~/.emacs.d/d/keybindings")
(load "~/.emacs.d/d/programming")
(load "~/.emacs.d/d/apps")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   [("#1B2229" "#ff6c6b" "#98be65" "#ECBE7B" "#51afef" "#c678dd" "#46D9FF" "#DFDFDF")])
 '(chronos-notification-wav "~/Music/echoed-ding.wav")
 '(chronos-shell-notify-program "mplayer -ao pulse /home/jacob/Music/echoed-ding.ogg")
 '(cursor-in-non-selected-windows nil)
 '(custom-safe-themes
   (quote
    ("f67652440b66223b66a4d3e9c0ddeddbf4a6560182fa38693bdc4d940ce43a2e" "0f0022c8091326c9894b707df2ae58dd51527b0cf7abcb0a310fb1e7bda78cd2" "0eef522d30756a80b28333f05c7eed5721f2ba9b3eaaff244ea4c6f6a1b8ac62" "8d737627879eff1bbc7e3ef1e9adc657207d9bf74f9abb6e0e53a6541c5f2e88" "5310b88333fc64c0cb34a27f42fa55ce371438a55f02ac7a4b93519d148bd03d" default)))
 '(doc-view-continuous t)
 '(doc-view-resolution 200)
 '(fci-rule-color "#5B6268")
 '(fringe-mode 0 nil (fringe))
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#3f444a"))
 '(linum-format "%4d │ ")
 '(lockfile-no-pollute t)
 '(markdown-command "/usr/bin/pandoc")
 '(menu-bar-mode nil)
 '(mouse-wheel-scroll-amount (quote (0 ((shift) . 1) ((control)))))
 '(org-ellipsis "  ")
 '(org-fontify-done-headline t)
 '(org-fontify-quote-and-verse-blocks t)
 '(org-fontify-whole-heading-line t)
 '(org-startup-indented t)
 '(package-archives
   (quote
    (("melpa" . "http://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("all-the-icons" . "~/AUR/all-the-icons.el/"))))
 '(package-enable-at-startup t)
 '(package-selected-packages
   (quote
    (ensime org-evil slime stumpwm-mode eclimd eclim js3 ps-ccrypt flx elpy ivy-hydra counsel swiper ivy indent-tools i3wm dired-subtree dired-hacks-utils workgroups2 cargo package-build package-lint toml-mode company-arduino column-marker markdown-mode evil-magit evil-surround with-editor auctex sound-wav chronos magit yatex nhexl-mode js3-mode transpose-frame eww-lnum flycheck-rust flycheck racer company-racer helm use-package evil-leader goto-chg evil pdf-tools rust-mode neotree nlinum paradox all-the-icons-dired doom-themes ##)))
 '(rdxmk-lockfile-no-pollute t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#1B2229")
 '(vc-annotate-color-map
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
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil))
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

(find-file-noselect "~/.emacs.d/.emacs") ;; I have ~/.emacs.d/.emacs softlinked to ~/.emacs
(provide '.emacs)
;;; .emacs ends here
