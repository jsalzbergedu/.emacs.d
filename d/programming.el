;; -*- lexical-binding: t -*-
;;; Programming packages and setup

;; Remove stupid tabs
(setq-default indent-tabs-mode nil)

;; Relevant to all programming before language packages are setup:
(defvar prog-minor-modes-common (list)
  "A common hook for programming minor modes")
(defun prog-minor-modes-common ()
  "A common hook for programming minor modes"
  (interactive)
  (mapc 'funcall prog-minor-modes-common))
(defun add-prog-minor-modes-common (&rest mode-hooks)
  "Add prog-minor-modes-common to MODE-HOOKS"
  (mapc (lambda (a) (add-hook a 'prog-minor-modes-common)) mode-hooks))
;; Prettify symbols
(use-package prettify-utils
  :load-path "prettify-utils.el/"
  :demand t
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
	      ("SPC s" . sp-forward-slurp-sexp)
	      :map evil-motion-state-map
	      ("SPC s" . sp-forward-slurp-sexp)))

;; Rainbow delimiters, a visual hint of nest depth
(use-package rainbow-delimiters
  :defer t
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-minor-modes-common 'rainbow-delimiters-mode))

;; 80 char rule
(defun highlight-80-char ()
  "Highlight all lines over 80 chars."
  (interactive)
  (highlight-lines-matching-regexp ".\\{81\\}" 'hi-green))

(add-hook 'prog-minor-modes-common 'highlight-80-char)

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
  :config (add-to-list 'company-backend 'company-lsp))
;; LSP integration with flycheck
(use-package lsp-ui
  :demand t
  :load-path "lsp-ui/"
  :config (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Project management
(use-package personal-info
  :load-path "personal-info/")

(use-package project-init
  :load-path "project-init/"
  :config (progn
            (setq project-init-author-email (personal-info-get 'email)
                  project-init-author-name (personal-info-get 'name))))

(use-package projectile
  :demand t
  :config (projectile-global-mode 1))

(use-package counsel-projectile
  :demand t)

;; Project hyrdra for generating hydras for projects
(use-package project-hydra
  :demand t
  :load-path "project-hydra/")

;; Magit
(use-package magit
  :defer t
  :config (require 'evil-magit)
  :bind
  (:map magit-mode-map
              ("SPC" . nil))
  (:map magit-diff-mode-map
        ("SPC" . nil))
  :init (defhydra hydra-magit (:hint nil :color blue)
	  "
^Commands^
----------------
_s_ magit-status _p_ magit-pull
_b_ magit-branch _c_ magit-checkout
"
	  ("s" magit-status)
	  ("p" magit-pull)
	  ("b" magit-branch)
	  ("c" magit-chekcout)))

;; Flycheck
(use-package flycheck
  :defer t
  :config (setq flycheck-idle-change-delay 2))

;; Company mode for autocompletion
(use-package company
  :demand t
  :init (add-hook 'finalize (lambda () (global-company-mode 1)))
  :config (setq company-idle-delay 0.2
		company-minimum-prefix-length 1)
  :commands global-company-mode)

;; Nlinum to display the line
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
(use-package lsp-python
  :demand t
  :load-path "lsp-python/"
  :config (progn (add-hook 'python-mode-hook #'lsp-python-enable)
		 (add-hook 'python-mode-hook 'prog-minor-modes-common)))

; Javascript:

;; JVM languages
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
  :demand t
  :config (setq checkstyle-executable "~/cs-checkstyle/checkstyle")
  :commands (checkstyle-curr-p checkstyle-output-curr))

(use-package gradle-mode
  :load-path "emacs-gradle-mode/"
  :demand t ;; I don't like it, but lsp-mode doesn't play nice with lazy loading
  :config
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

(defun counsel-ag/java (&rest args)
  "Go up to the root of a java project, and search the files"
  (interactive)
  (counsel-ag nil (locate-dominating-file default-directory "build.gradle")))

(use-package f
  :demand t)

(use-package lsp-java
  :demand t
  :load-path "lsp-java/"
  :config
  (progn (add-hook 'java-mode-hook 'prog-minor-modes-common)
	 (add-hook 'java-mode-hook 'lsp-java-enable)
	 (add-hook 'java-mode-hook (lambda ()
				     (flycheck-mode 1)
				     (google-set-c-style)
				     (google-make-newline-indent)
				     (setq indent-tabs-mode nil
					   tab-width 4
					   c-basic-offset 4)))
         (setq lsp-java--workspace-folders (get-subdirs "~/Programming/")))
  (project-hydra hydra-java
    :test gradle-test
    :compile gradle-build
    :stylecheck checkstyle
    :search counsel-ag/java
    :git hydra-magit/body
    :and ("c" checkstyle-compile))
  (evil-define-key 'normal java-mode-map (kbd "SPC p") 'hydra-java/body)
  :commands lsp-java-enable)

;; Groovy
(use-package groovy-mode
  :config (progn (add-hook 'groovy-mode-hook 'prog-minor-modes-common)
		 (add-to-list 'auto-mode-alist '("\\build.gradle\\'" . groovy-mode)))
  :defer t)

;; Scala
(use-package ensime
  :defer t
  :commands ensime
  :config (setq ensime-startup-notification nil))

(use-package sbt-mode)

(use-package scala-mode
  :defer t
  :config (progn (add-hook 'scala-mode-hook 'prog-minor-modes-common)
		 (setq scala-indent:add-space-for-scaladoc-asterisk nil)))

;; Elisp:
(use-package cl-lib
  :demand t)
(use-package exec-path-from-shell)
(add-hook 'emacs-lisp-mode-hook 'prog-minor-modes-common)
(defun eval-region-advice (eval-region-orig start end &optional printflag read-function)
  (funcall eval-region-orig start end t read-function))
(advice-add 'eval-region :around 'eval-region-advice)
(evil-define-key 'visual emacs-lisp-mode-map "SPC e" 'eval-region) ;; This doesn't work, for some reason.

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
		 (add-prog-minor-modes-common 'lisp-mode-hook 'slime-repl-mode-hook)))

;; Scheme
(setq scheme-program-name "csi -:c")
(use-package scheme-complete)
(use-package geiser
  :defer t
  :init (add-hook 'scheme-mode-hook (lambda ()
				      (geiser-mode)
				      (prog-minor-modes-common)))
  :config (progn (setq geiser-active-implementations '(chicken))
		 (add-prog-minor-modes-common 'scheme-mode-hook 'geiser-repl-mode-hook))
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
  :config (progn (setq markdown-command "/usr/bin/pandoc")
		 (add-hook 'markdown-mode-hook 'prog-minor-modes-common)))

;; Shell
(use-package sh-script
  :defer t
  :config (progn (add-to-list 'auto-mode-alist '("\\PKGBUILD\\'" . sh-mode))
		 (add-hook 'sh-mode-hook 'prog-minor-modes-common))
  :commands sh-mode)


;; Chroot
;; (require 'schroot-mode "~/.emacs.d/schroot-mode/schroot-mode.el")
;; (progn (schroot-mode-global-mode 1)
;; 		 (setq schroot-mode-files-loc (expand-file-name "~/.emacs.d/schroot-mode/"))
;; 		 (schroot-mode-add-dir-config "libseawolf" "ubuntu")
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

;; Forth
(use-package forth-mode
  :config (add-hook 'forth-mode-hook 'prog-minor-modes-common)
  :defer t)

;; Idris
(use-package idris-mode
  :defer t
  :config (add-prog-minor-modes-common 'idris-mode-hook
				       'idris-repl-mode-hook
				       'idris-ipkg-mode-hook))
