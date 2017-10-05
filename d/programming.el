;; -*- lexical-binding: t -*-
;;; Programming packages and setup

;; Relevant to all programming before language packages are setup:
(use-package flycheck
  :ensure t)
(require 'company)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)
(defun nlinum-mode1 () "Sets nlinum mode to 1" (nlinum-mode 1))

;; Python:
(elpy-enable)

;; Javascript:
(use-package js3
  :defer t)


;; Java:

;; Scala:
(use-package ensime
  :ensure t
  :pin melpa-stable)
(setq
  ensime-sbt-command "/bin/sbt"
  sbt:program-name "/bin/sbt")

;; Elisp:
(require 'cl-lib)
(add-hook 'emacs-lisp-mode-hook 'nlinum-mode1)
(defun eval-region-advice (eval-region-orig start end &optional printflag read-function)
  (funcall eval-region-orig start end t read-function))

(advice-add 'eval-region :around #'eval-region-advice)

;; Common Lisp:
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime-autoloads)
(use-package slime
  :defer t
  :config (progn (require 'cl)
		 (add-to-list 'slime-contribs 'slime-fancy)
		 (if (file-exists-p (expand-file-name "/usr/lib/quicklisp/slime-helper.el"))
		 (load (expand-file-name "/usr/lib/quicklisp/slime-helper.el"))
		 (when (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
		   (load (expand-file-name "~/quicklisp/slime-helper.el"))))
		 (setq inferior-lisp-program "/usr/bin/sbcl")))
(evil-leader/set-key-for-mode 'emacs-lisp-mode-hook (kbd "e") 'eval-region)
(evil-leader/set-key-for-mode 'slime-lisp-mode-hook (kbd "e") 'slime-eval-region)

;; Scheme
(setq scheme-program-name "csi -:c")
(use-package scheme-complete)
(use-package geiser)
(setq geiser-active-implementations '(chicken))
(add-hook 'scheme-mode-hook 'geiser-mode)

;; Rust:
(use-package rust-mode
  :defer t
  :config (progn (require 'racer)
		 (require 'company-racer)
		 (add-hook 'rust-mode #'racer-mode)
		 (add-hook 'racer-mode-hook #'eldoc-mode)
		 (evil-leader/set-key-for-mode 'rust-mode "." 'racer-find-definition)))

(defun toggle-mut ()
  "Toggles the mutability of the variable defined on the current line."
  (interactive)
  (save-excursion
    (back-to-indentation)
    (forward-word)
    (if (string= " mut" (buffer-substring (point) (+ (point) 4)))
        (delete-region (point) (+ (point) 4))
      (insert " mut"))))

;; Redox:
(when (file-exists-p "~/.emacs.d/rdxmk/") (progn (add-to-list 'load-path "~/.emacs.d/rdxmk/")
					       (require 'rdxmk)))

;; Toml:
(require 'toml-mode)

;; TeX:

;; HTML:
(add-hook 'sgml-mode-hook 'nlinum-mode1)

;; Markdown
(setq markdown-command "/usr/bin/pandoc")

;; Shell
(add-to-list 'auto-mode-alist '("\\PKGBUILD\\'" . sh-mode))

;; Chroot
(require 'schroot-mode "~/.emacs.d/schroot-mode/schroot-mode.el")
(progn (schroot-mode-global-mode 1)
		 (setq schroot-mode-files-loc (expand-file-name "~/.emacs.d/schroot-mode/"))
		 (schroot-mode-add-dir-config "libseawolf" "ubuntu")
		 (schroot-mode-add-dir-config "seawolf" "ubuntu")
		 (schroot-mode-add-dir-config "swpycv" "ubuntu")
		 (schroot-mode-add-dir-config "svr" "ubuntu"))

;; C/C++
(use-package cmake-mode
  :defer t
  :config (add-hook 'cmake-mode-hook 'nlinum-mode))
(use-package meson-mode
  :defer t)
(load-file "~/sources/rtags/src/rtags.elc")
(set-variable 'rtags-path (expand-file-name "~/sources/rtags/bin/"))
(add-hook 'schroot-mode-hook (lambda () "Set rtags to the correct installation" '()))

;; Scratch (ugh)
(use-package json-mode
  :defer t)


;; Relevant to all after setup:
(global-company-mode t)
(add-hook 'prog-mode-hook 'nlinum-mode1)
(use-package indent-tools
  :load-path "indent-tools"
  :init (global-set-key (kbd "C-c i") 'indent-tools/hydra-body))
(setq nlinum-format "%4d │ ")
