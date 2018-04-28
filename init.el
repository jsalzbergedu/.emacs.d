(defvar finalize (list) "A list of functions called after emacs initializes")
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
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
   ["#ecf0f1" "#e74c3c" "#2ecc71" "#f1c40f" "#2492db" "#9b59b6" "#1abc9c" "#2c3e50"])
 '(custom-safe-themes
   (quote
    ("15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" default)))
 '(fci-rule-color "#f1c40f" t)
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(package-selected-packages
   (quote
    (forth-mode dash-functional google-c-style realgud json-mode exec-path-from-shell rainbow-identifiers ag rainbow-delimiters smartparens flatui-theme ## doom-themes all-the-icons-dired paradox nlinum neotree rust-mode pdf-tools goto-chg evil-leader use-package helm company-racer racer flycheck flycheck-rust eww-lnum transpose-frame js3-mode nhexl-mode yatex magit chronos sound-wav auctex with-editor evil-surround evil-magit markdown-mode column-marker company-arduino toml-mode package-lint package-build cargo workgroups2 dired-hacks-utils dired-subtree i3wm indent-tools ivy swiper counsel ivy-hydra elpy flx ps-ccrypt js3 eclimd stumpwm-mode slime org-evil geiser scheme-complete paredit evil)))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background "#ecf0f1" t)
 '(vc-annotate-color-map
   (quote
    ((30 . "#e74c3c")
     (60 . "#c0392b")
     (90 . "#e67e22")
     (120 . "#d35400")
     (150 . "#f1c40f")
     (180 . "#d98c10")
     (210 . "#2ecc71")
     (240 . "#27ae60")
     (270 . "#1abc9c")
     (300 . "#16a085")
     (330 . "#2492db")
     (360 . "#0a74b9"))) t)
 '(vc-annotate-very-old-color "#0a74b9" t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(put 'upcase-region 'disabled nil)
(with-eval-after-load 'init.el (mapc 'funcall finalize))
(provide 'init.el)
;;; init.el ends here
