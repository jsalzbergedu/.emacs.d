(defvar finalize (list) "A list of functions called after emacs initializes")
(defvar finalized nil "Whether the initialization has finalized")

(defvar bootstrap-version)
(setq straight-find-flavor 'gnu/bsd)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(push "lib" straight-default-files-directive)

(straight-use-package 'use-package)

;; Set up essential extensions to elisp
(use-package subr-x
  :demand t
  :straight nil)

(use-package cl
  :straight t
  :demand t)

(use-package seq
  :straight t
  :demand t)

(use-package cl-lib
  :straight t
  :demand t)


(use-package dash
  :straight (dash :type git
                  :host github
                  :repo "magnars/dash.el")
  :demand t)

(use-package dash-functional
  :straight nil
  :demand t)

(use-package monitor
  :straight (monitor :type git
                     :host github
                     :repo "GuiltyDolphin/monitor")
  :demand t)

(use-package ht
  :straight (ht :type git
                :host github
                :repo "Wilfred/ht.el")
  :demand t)

(use-package s
  :straight (s :type git
               :host github
               :repo "magnars/s.el")
  :demand t)

(use-package f
  :straight (f :type git
               :host github
               :repo "rejeep/f.el")
  :demand t)

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
 '(ccls-sem-highlight-method (quote overlay) t)
 '(cquery-sem-highlight-method (quote overlay))
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "15348febfa2266c4def59a08ef2846f6032c0797f001d7b9148f30ace0d08bcf" default)))
 '(display-battery-mode t)
 '(fci-rule-color "#f1c40f" t)
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(package-selected-packages (quote (## column-marker ps-ccrypt js3 eclimd)))
 '(safe-local-variable-values
   (quote
    ((checkdoc-package-keywords-flag)
     (eval font-lock-add-keywords nil
           (\`
            (((\,
               (concat "("
                       (regexp-opt
                        (quote
                         ("sp-do-move-op" "sp-do-move-cl" "sp-do-put-op" "sp-do-put-cl" "sp-do-del-op" "sp-do-del-cl"))
                        t)
                       "\\_>"))
              1
              (quote font-lock-variable-name-face)))))
     (eval ignore-errors
           (push
            (quote
             ("Tests" "(\\(\\<ert-deftest\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?" 2))
            imenu-generic-expression)
           (when
               (string-match-p "test"
                               (buffer-file-name))
             (setq-local emojify-inhibit-emojify-in-current-buffer-p t)))
     (org-confirm-babel-evaluate)
     (rainbow-mode . 1)
     (my-other-other-hotkey-mode . t)
     (geiser-default-implementation . chicken)
     (geiser-default-implementation . racket)
     (org-latex-image-default-width . "")
     (java-gradle-run-jar . "./build/libs/WolfScheduler-all.jar")
     (idris-load-packages "contrib")
     (idris-interpreter-flags "-i" "/usr/share/idris/libs/idrisjvmffi")
     (idris-interpreter-flags "-i /usr/share/idris/libs/idrisjvmffi")
     (idris-interpreter-flags . "-i /usr/share/idris/libs/idrisjvmffi")
     (c-basic-offset-set . 2)
     (idris-load-packages "effects")
     (idris-load-packages quote
                          ("effects"))
     (idris-load-packages . "effects"))))
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
 '(ccls-code-lens-face ((t (:inherit shadow :height 0.8))))
 '(fixed-pitch ((t (:family "Inconsolata"))))
 '(lsp-ui-sideline-code-action ((t (:foreground "#2ecc71"))))
 '(lsp-ui-sideline-current-symbol ((t (:foreground "white" :box (:line-width -1 :color "white") :weight ultra-bold :height 0.95))))
 '(lsp-ui-sideline-symbol ((t (:foreground "#7f8c8d" :box (:line-width -1 :color "#7f8c8d") :height 0.9)))))

(put 'upcase-region 'disabled nil)
(with-eval-after-load 'init.el (mapc 'funcall finalize))
(setq finalized t)

(provide 'init.el)
;;; init.el ends here
