;; -*- lexical-binding: t -*-
;;; Programming packages and setup

;; Relevant to all programming before language packages are setup:
(defvar prog-minor-modes-common (list) "A common hook for programming minor modes")
(defun prog-minor-modes-common ()
  "A common hook for programming minor modes"
  (interactive)
  (mapc 'funcall prog-minor-modes-common))
;; Prettify symbols
(use-package prettify-utils
  :load-path "prettify-utils.el/"
  :defer t
  :config (progn (add-hook 'prettify-symbols-mode-hook '(lambda ()
							  "Sets the list of symbols"
							  (setq prettify-symbols-alist
								(prettify-utils-generate
								 ("lambda" "λ")
								 ("delta" "∆")
								 ("nu" "𝜈")
								 ("Reals" "ℝ")
								 ("reals" "ℝ")
								 ("<="     "≤")
								 (">="     "≥")
								 ("pi" "𝜋")
								 ("->"     "→ "))))))

  :commands prettify-symbols-mode
  :init (add-hook 'prog-minor-modes-common 'prettify-symbols-mode))

;; Smartparens, for () {} '' "" []
(use-package smartparens
  :defer t
  :config (progn (use-package smartparens-config
		   :demand t)
		 (add-hook 'prog-minor-modes-common 'show-paren-mode))
  :commands (smartparens-mode sp-forward-slurp-sexp)
  :init (add-hook 'prog-minor-modes-common 'smartparens-mode)
  :bind (:map evil-normal-state-map
	      ("SPC s" . sp-forward-slurp-sexp)))

;; Rainbow delimiters, a visual hint of nest depth
(use-package rainbow-delimiters
  :defer t
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-minor-modes-common 'rainbow-delimiters-mode))

;; LSP mode, a common interface to programs
;; The lsp modes don't play nice with lazy loading
(use-package lsp-mode
  :demand t
  :load-path "lsp-mode/"
  :config (setq lsp-inhibit-message t))
;; LSP's completion package
(use-package company-lsp
  :demand t
  :load-path "company-lsp/"
  :config (add-to-list 'company-backend 'company-lsp)
  :after company)

;; Flycheck
(use-package flycheck)

;; Company mode for autocompletion
(use-package company
  :demand t
  :init (add-hook 'finalize (lambda () (global-company-mode 1)))
  :config (setq company-idle-delay 0.2
		company-minimum-prefix-length 1)
  :commands global-company-mode)

(use-package nlinum
  :defer t
  :init (add-hook 'prog-minor-modes-common 'nlinum-mode)
  :config (progn (setq nlinum-format "%4d │ "))
  :commands nlinum-mode)

;; All c-likes
(use-package google-c-style
  :defer t
  :commands google-set-c-style)

;; Python:
(elpy-enable)

; Javascript:

;; Java:
(defun is-dirlink (d)
  "Returns nil if the file does not end in /. or /.."
  (let ((len (length d)))
    (or (and (>= len 3)
	     (string= "/.." (substring d (- (length d) 3))))
	(and (>= len 2)
	     (string= "/." (substring d (- (length d) 2)))))))

(defun get-subdirs (dir)
  (cl-remove-if-not (lambda (f) (and (file-directory-p f) (not (is-dirlink f)))) (directory-files dir "")))

(use-package elisp-checkstyle
  :load-path "elisp-checkstyle"
  :defer t
  :config (setq checkstyle-executable "~/cs-checkstyle/checkstyle")
  :commands (checkstyle-curr-p checkstyle-output-curr))

(use-package gradle-mode
  :load-path "emacs-gradle-mode/"
  :defer t
  :commands (gradle-build gradle-test)
  :after elisp-checkstyle)

(defun checkstyle-compile ()
  "If checkstyle likes the current file, compile it.
Otherwise, display the checkstyle buffer"
  (interactive)
  (if (checkstyle-curr-p)
      (gradle-build)
    (checkstyle-output-curr)))

(defun checkstyle ()
  (interactive)
  (checkstyle-output-curr))

(use-package lsp-java
  :demand t
  :load-path "lsp-java/"
  :config
  (progn (add-hook 'java-mode-hook 'prog-minor-modes-common)
	 (add-hook 'java-mode-hook 'lsp-java-enable)
	 (add-hook 'java-mode-hook (lambda ()
				     (google-set-c-style)
				     (google-make-newline-indent)
				     (setq indent-tabs-mode nil)
				     (setq tab-width 4)
				     (setq c-basic-offset 4)))
	 (setq lsp-java--workspace-folders (get-subdirs "~/Programming/")))
  (defhydra hydra-java (:color blue :hint nil)
    "
^Check style^          ^Build^
^^^^^^^^^^^^^--------------------------
_c_ checkstyle-compile _b_ gradle-build
_s_ checkstyle         _t_ gradle-test
"
    ("c" checkstyle-compile)
    ("s" checkstyle)
    ("b" gradle-build)
    ("t" gradle-test))
  (evil-define-key 'normal 'java-mode-map (kbd "SPC p") 'hydra-java/body)
  :commands lsp-java-enable)


;; Elisp:
(use-package cl-lib
  :demand t)
(use-package exec-path-from-shell)
(add-hook 'emacs-lisp-mode-hook 'prog-minor-modes-common)
(defun eval-region-advice (eval-region-orig start end &optional printflag read-function)
  (funcall eval-region-orig start end t read-function))
(evil-define-key 'visual emacs-lisp-mode-map "SPC e" 'eval-region)

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
		 (setq inferior-lisp-program "/usr/bin/sbcl")
		 (evil-define-key 'normal slime-mode-map "SPC e" 'slime-eval-region)
		 (add-hook 'slime-lisp-mode-hook 'prog-minor-modes-common)))

;; Scheme
(setq scheme-program-name "csi -:c")
(use-package scheme-complete)
(use-package geiser
  :defer t
  :init (add-to-list 'auto-mode-alist '("\\.scm\\'" . (lambda ()
							(scheme-mode 1)
							(geiser-mode 1))))
  :config (progn (setq geiser-active-implementations '(chicken))
		 (add-hook 'scheme-mode-hook 'prog-minor-modes-common))
  :commands geiser-mode)

;; Rust:
(use-package rust-mode
  :defer t
  :config (progn (require 'racer)
		 (require 'company-racer)
		 (add-hook 'rust-mode #'racer-mode)
		 (add-hook 'racer-mode-hook #'eldoc-mode)
		 (add-hook 'rust-mode 'prog-minor-modes-common)))

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
;; I'll get back to rdxmk later
;; with (use-package rdxmk)
;; (when (file-exists-p "~/.emacs.d/rdxmk/") (progn (add-to-list 'load-path "~/.emacs.d/rdxmk/")
;; 					       (require 'rdxmk)))

;; Toml:
(use-package toml-mode
  :defer t
  :config (progn (add-hook 'toml-mode-hook 'prog-minor-modes-common)))

;; TeX:

;; HTML:
(use-package sgml-mode
  :defer t
  :config (progn (add-hook 'sgml-mode-hook 'prog-minor-modes-common)))

;; Markdown
(use-package markdown-mode
  :defer t
  :config (progn (setq markdown-command "/usr/bin/pandoc")))

;; Shell
(use-package sh-script
  :defer t
  :config (progn (add-to-list 'auto-mode-alist '("\\PKGBUILD\\'" . sh-mode))
		 (add-hook 'sh-mode-hook 'prog-minor-modes-common))
  :commands sh-mode)


;; Chroot
;; (require 'schroot-mode "~/.emacs.d/schroot-mode/schroot-mode.el")
;; (progn (schroot-mode-global-mode 1)
;; 		 (setq schroot-mode-files-loc (expand-file-name "~/.emacs.d/schroot-mode/")) ;; 		 (schroot-mode-add-dir-config "libseawolf" "ubuntu")
;; 		 (schroot-mode-add-dir-config "seawolf" "ubuntu")
;; 		 (schroot-mode-add-dir-config "swpycv" "ubuntu")
;; 		 (schroot-mode-add-dir-config "svr" "ubuntu"))

;; C/C++
;; (use-package cmake-mode
;;   :defer t
;;   :config (add-hook 'cmake-mode-hook 'nlinum-mode))
;; (use-package meson-mode
;;   :defer t)
;; (load-file "~/sources/rtags/src/rtags.elc")
;; (set-variable 'rtags-path (expand-file-name "~/sources/rtags/bin/"))
;; (add-hook 'schroot-mode-hook (lambda () "Set rtags to the correct installation" '()))

;; Scratch (ugh)
(use-package json-mode
  :defer t
  :config (progn (add-hook 'json-mode-hook 'prog-minor-modes-common)))
