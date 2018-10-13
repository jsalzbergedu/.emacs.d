;; -*- lexical-binding: t -*-
;;; Programming packages and setup

;; Remove tabs
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

;; Eglot, a simple interface to some LSP providers
(use-package eglot
  :defer t
  :commands eglot-ensure)

;; LSP mode, a common interface to LSP providers
;; The lsp modes don't play nice with lazy loading
(use-package lsp-mode
  :demand t
  :load-path "lsp-mode/"
  :config (progn (setq lsp-inhibit-message t
                       lsp-print-io nil)))
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

;; Realgud, an interface to debuggers in general
(use-package realgud
  :defer t
  :commands realgud:gdb realgud:jdb)

;; Project management
(use-package personal-info
  :load-path "personal-info/")

(use-package project-init
  :load-path "project-init/"
  :init (progn
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
  :commands (google-set-c-style google-make-newline-indent))

;; Python:
(use-package python-mode
  :defer t
  :hook (python-mode . eglot-ensure)
  :init (add-hook 'python-mode-hook 'prog-minor-modes-common))

; Javascript:

;; JVM languages
;; Java:

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


(defvar c-basic-offset-set nil "What c-basic-offset is set to.")
(defun c-basic-offset-fetch ()
  (if c-basic-offset-set
      c-basic-offset-set
    4))
(defun set-c-basic-offset ()
  (setq-local c-basic-offset (c-basic-offset-fetch)))

(defun find-gradle-build ()
  (locate-dominating-file default-directory "build.gradle"))

(defvar java-gradle-run-jar nil
  "The jar to run in a java project")

;; (defun get-with-ext (ext dir)
;;   "Get all the files with ext from dir and its subdirectories."
;;   (f-files dir (lambda (file) (equal (f-ext file) "class")) t))

;; (defvar junit-executable nil "Where the junit executable is stored")

;; (setq junit-executable "/usr/share/java/gradle/lib/plugins/junit-4.12.jar")

;; (defun generate-gradle-junit-classpath ()
;;   "Generate a classpath that allows running a test."
;;   (let* ((built-classes (f-files (find-gradle-build)
;;                            (lambda (file) (equal (f-ext file) "class") t)
;;                            t)))
;;     (-reduce-from (lambda (x y) )
;;                   junit-executable)))

(defun java-gradle-jar-run ()
  "Run the jar for this project"
  (interactive)
  (let ((run-jar java-gradle-run-jar))
    (with-temp-buffer
      (cd (find-gradle-build))
      (compile (format "java -jar %s" run-jar)))))

(use-package lsp-java
  :demand t
  :load-path "lsp-java/"
  :init (setq lsp-java--workspace-folders (append (f-directories "~/Programming/") (f-directories "~/eclipse-workspace/"))
              lsp-java-server-install-dir (concat "~/.emacs.d/eclipse.jdt.ls/"
                                                  "org.eclipse.jdt.ls.product/"
                                                  "target/repository/"))
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
         (add-hook 'java-mode-hook 'set-c-basic-offset))
  (project-hydra hydra-java
    :test gradle-test
    :compile gradle-build--daemon
    :stylecheck checkstyle
    :search counsel-ag/java
    :git hydra-magit/body
    :run java-gradle-jar-run
    :and ("c" checkstyle-compile)
    :and ("p" counsel-projectile-find-file))
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
(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

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
		 (evil-define-key 'visual slime-mode-map "SPC e" 'slime-eval-region)
		 (add-prog-minor-modes-common 'lisp-mode-hook 'slime-repl-mode-hook))
  :init (add-hook 'lisp-mode-hook 'prog-minor-modes-common))

;; Scheme
(setq scheme-program-name "csi -:c")
(use-package scheme-complete)
(use-package geiser
  :defer t
  :init (add-hook 'scheme-mode-hook (lambda ()
				      (geiser-mode)
				      (prog-minor-modes-common)))
  :config (progn (setq geiser-active-implementations '(chicken))
		 (add-prog-minor-modes-common 'scheme-mode-hook 'geiser-repl-mode-hook)
                 (add-to-list 'auto-mode-alist '("\\.setup\\'" . scheme-mode))
                 (add-to-list 'auto-mode-alist '("\\.meta\\'" . scheme-mode)))
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
(use-package cmake-mode
  :defer t
  :config
  (add-hook 'cmake-mode-hook 'prog-minor-modes-common))

(use-package lsp-clangd
  :load-path "lsp-clangd/"
  :init (progn (add-hook 'c++-mode-hook (lambda ()
                                          (google-set-c-style)
                                          (google-make-newline-indent)
                                          (setq indent-tabs-mode nil
					        tab-width 2
                                                c-basic-offset 2)
                                          (prog-minor-modes-common)))
               (setq lsp-clangd-executable "/usr/bin/clangd"
                     cmake-style t)))



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

;; Ruby
(use-package enh-ruby-mode
  :defer t
  :init (progn (add-to-list 'auto-mode-alist
                              '("\\.rb\\'" . enh-ruby-mode))
                 (add-to-list 'auto-mode-alist
                              '("\\Rakefile\\'" . enh-ruby-mode)))
  :config (add-hook 'enh-ruby-mode-hook 'prog-minor-modes-common)
  :commands enh-ruby-mode)

(use-package inf-ruby
  :defer t
  :config (add-hook 'inf-ruby-mode-hook 'prog-minor-modes-common))

;; Xml
(use-package nxml-mode
  :defer t
  :config (add-hook 'nxml-mode-hook 'prog-minor-modes-common))

;; Uml
(use-package plantuml-mode
  :defer t
  :init
  (setq plantuml-jar-path "/opt/plantuml/plantuml.jar"))

;; Coq
(use-package proof-general
  :defer t)

;; Haskell
(use-package haskell-mode
  :init (setq haskell-process-type 'stack-ghci)
  :defer t)

(use-package inf-haskell
  :defer t)

;; F*
(use-package fstar-mode
  :defer t)
