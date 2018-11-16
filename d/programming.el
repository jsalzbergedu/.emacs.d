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

;; Ansi coloring
(use-package ansi-color
  :demand t
  :straight nil)

;; Compile for... compilation
(use-package compile
  :after ansi-color
  :defer t
  :straight nil
  :config
  (defun colorize-compilation-buffer ()
    (toggle read-only)
    (ansi-color-apply-on-region compilation-filter-start (point))
    (toggle-read-only))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

  :bind (:map compilation-mode-map
	      ("SPC" . nil)))

;; Prettify symbols
(use-package prettify-utils
  :demand t
  :straight (prettify-utils :type git
                            :host github
                            :repo "Ilazki/prettify-utils.el"
                            :fork (:host github
                                         :repo "jsalzbergedu/prettify-utils.el"))
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
  :straight (smartparens :type git
                         :host github
                         :repo "Fuco1/smartparens")
  :defer t
  :config
  (require 'smartparens-config)
  (add-hook 'prog-minor-modes-common 'show-paren-mode)
  :commands (smartparens-mode sp-forward-slurp-sexp)
  :init (add-hook 'prog-minor-modes-common 'smartparens-mode)
  :bind (:map evil-normal-state-map
	      ("SPC s" . sp-forward-slurp-sexp)
	      :map evil-motion-state-map
	      ("SPC s" . sp-forward-slurp-sexp)))


(use-package smartparens-config
  :straight nil
  :after smartparens
  :demand t)

;; Rainbow delimiters, a visual hint of nest depth
(use-package rainbow-delimiters
  :defer t
  :straight (rainbow-delimeters :type git
                                :host github
                                :repo "Fanael/rainbow-delimiters")
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-minor-modes-common 'rainbow-delimiters-mode))

;; Yasnippet, a snippet manager
(use-package yasnippet
  :defer t
  :straight (yasnippet :type git
                       :host github
                       :repo "joaotavora/yasnippet")
  :commands yas-insert-snippet)

(use-package yasnippet-snippets
  :after yasnippet
  :demand t
  :straight (yasnippet-snippets :type git
                                :host github
                                :repo "AndreaCrotti/yasnippet-snippets"
                                :files ("yasnippet-snippets.el" "snippets")))

(add-hook 'prog-minor-modes-common 'yas-minor-mode)
(add-hook 'prog-minor-modes-common 'evil-normalize-keymaps)

;; 80 char rule
(defun highlight-80-char ()
  "Highlight all lines over 80 chars."
  (interactive)
  (highlight-lines-matching-regexp ".\\{81\\}" 'hi-green))

(add-hook 'prog-minor-modes-common 'highlight-80-char)

;; Eglot, a simple interface to some LSP providers
(use-package eglot
  :defer t
  :straight (eglot :type git
                   :host github
                   :repo "joaotavora/eglot")
  :commands eglot-ensure)

;; Company mode for autocompletion
(defhydra company-hydra (:hint nil :color blue)
  "
Navigation
^^^^^^^^^^^--------------
_k_: company-select-previous-or-abort
_j_: company-select-next-or-abort
"
  ("k" company-select-previous-or-abort :color 'pink)
  ("j" company-select-next-or-abort :color 'pink))

(use-package company
  :demand t
  :straight (company :type git
                     :host github
                     :repo "company-mode/company-mode")
  :init (add-hook 'finalize (lambda () (global-company-mode 1)))
  :config
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 1)
  (define-key company-active-map (kbd "C-o") 'company-hydra/body)
  :commands global-company-mode)

;; LSP mode, a common interface to LSP providers
;; The lsp modes don't play nice with lazy loading
(use-package lsp-mode
  :demand t
  :straight (lsp-mode :type git
                      :host github
                      :repo "emacs-lsp/lsp-mode")
  :config (setq lsp-inhibit-message t
                lsp-print-io nil))

;; LSP's completion package
(use-package company-lsp
  :demand t
  :straight (company-lsp :type git
                         :host github
                         :repo "tigersoldier/company-lsp")
  :config (add-to-list 'company-backend 'company-lsp))

;; Required by dap-mode
(use-package bui
  :straight (bui :type git
                 :host github
                 :repo "alezost/bui.el")
  :demand t)

;; Required by dap-mode
(use-package tree-mode
  :straight t
  :demand t)

;; Like LSP, but for debuggers
(use-package dap-mode
  :demand t
  :straight (dap-mode :type git
                      :host github
                      :repo "yyoncho/dap-mode"
                      :files ("*.el" "icons/eclipse"))
  :config
  (defvar is-dap-on nil "Whether dap is enabled")

  (defun dap-on ()
    (interactive)
    (dap-mode 1)
    (dap-ui-mode 1)
    (fringe-mode (cons nil 0))
    (setq is-dap-on t))

  (defun dap-off ()
    (interactive)
    (dap-mode -1)
    (dap-ui-mode -1)
    (fringe-mode (cons 0 0))
    (setq is-dap-on t))

  (defun toggle-dap ()
    (interactive)
    (if is-dap-on
        (dap-off)
      (dap-on)))
  (defvar-local dap-java-test-run-directory nil
    "When set, the directory from which dap-java will run a test.")
  (put 'dap-java-test-run-directory 'safe-local-variable #'stringp))

(use-package dap-ui
  :after dap-mode
  :straight nil)

(use-package dap-ui-repl
  :after dap-mode
  :straight nil)

(use-package dap-hydra
  :after dap-mode
  :straight nil)

;; Project management
(use-package personal-info
  :load-path "personal-info/"
  :straight nil)

(use-package project-init
  :straight (project-init :type git
                          :host github
                          :repo "jsalzbergedu/project-init")
  :init (progn
            (setq project-init-author-email (personal-info-get 'email)
                  project-init-author-name (personal-info-get 'name))))

;; Projectile for helping emacs organize projects
(use-package epl
  :defer t
  :straight (epl :type git
                 :host github
                 :repo "cask/epl"))

(use-package pkg-info
  :defer t
  :straight (pkg-info :type git
                      :host github
                      :repo "lunaryorn/pkg-info.el"))

(use-package projectile
  :demand t
  :straight (projectile :type git
                        :host github
                        :repo "bbatsov/projectile")
  :config (projectile-global-mode 1))

(use-package counsel-projectile
  :straight (counsel-projectile :type git
                                :host github
                                :repo "ericdanan/counsel-projectile")
  :demand t)

;; Project hyrdra for generating hydras for projects
(use-package project-hydra
  :demand t
  :straight (project-hydra :type git
                           :host github
                           :repo "jsalzbergedu/project-hydra"))

;; Magit
(use-package graphql
  :straight (graphql :type git
                     :host github
                     :repo "vermiculus/graphql.el"
                     :files ("graphql.el"))
  :defer t)

(use-package treepy
  :straight (treepy :type git
                    :host github
                    :repo "volrath/treepy.el")
  :defer t)

(use-package async
  :straight (async :type git
                   :host github
                   :repo "jwiegley/emacs-async")
  :defer t)

(use-package ghub
  :straight (ghub :type git
                  :host github
                  :repo "magit/ghub")
  :defer t)

(use-package with-editor
  :straight (with-editor :type git
                         :host github
                         :repo "magit/with-editor")
  :defer t)

(use-package magit-popup
  :straight (magit-popup :type git
                         :host github
                         :repo "magit/magit-popup")
  :defer t)


(use-package magit
  :defer t
  :straight (magit :type git
                   :host github
                   :repo "magit/magit")
  :bind
  (:map magit-mode-map
              ("SPC" . nil))
  (:map magit-diff-mode-map
        ("SPC" . nil))
  :init
  (defhydra hydra-magit (:hint nil :color blue) "
^Commands^
----------------
_s_ magit-status _p_ magit-pull
_b_ magit-branch _c_ magit-checkout
"
	  ("s" magit-status)
	  ("p" magit-pull)
	  ("b" magit-branch)
	  ("c" magit-chekcout)))

(use-package evil-magit
  :straight (evil-magit :type git
                        :host github
                        :repo "emacs-evil/evil-magit")
  :after magit)

;; Flycheck
(use-package flycheck
  :straight (flycheck :type git
                      :host github
                      :repo "flycheck/flycheck")
  :defer t
  :config
  (setq flycheck-idle-change-delay 2)

  (defun flycheck-scroll-err-next ()
    "Go to the next error, wrapping around if necessary"
    (interactive)
    (condition-case nil
        (flycheck-next-error)
      (error (flycheck-next-error 1 t))))
  
  
  (defun flycheck-scroll-err-prev ()
    "Go to the next error, reversing if necessary"
    (interactive)
    (condition-case nil
        (flycheck-previous-error)
      (error (flycheck-previous-error -1))))

  (defhydra hydra-flycheck-error  (:hint nil :color blue) "
_n_: flycheck-next-error
_N_: flycheck-previous-error
_e_: flycheck-list-errors
"
    ("n" flycheck-scroll-err-next :color pink)
    ("N" flycheck-scroll-err-prev :color pink)
    ("e" flycheck-list-errors :color blue)))

;; LSP integration with flycheck
(use-package lsp-ui
  :demand t
  :straight (lsp-ui :type git
                    :host github
                    :repo "emacs-lsp/lsp-ui")
  :config (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; Nlinum to display the line
(use-package nlinum
  :straight t
  :init (add-hook 'prog-minor-modes-common 'nlinum-mode)
  :config (progn (setq nlinum-format "%4d │ "))
  :commands nlinum-mode)

(use-package nlinum-hl
  :straight (emacs-nlinum-hl :type git
                             :host github
                             :repo "hlissner/emacs-nlinum-hl")
  :after nlinum
  :demand t)

;; All c-likes
(use-package google-c-style
  :defer t
  :straight t
  :commands (google-set-c-style google-make-newline-indent))

;; Python:
(use-package python-mode
  :defer t
  :straight nil
  :hook (python-mode . eglot-ensure)
  :init (add-hook 'python-mode-hook 'prog-minor-modes-common))

; Javascript:

;; JVM languages
;; Java:

(use-package output-buffer
  :straight (output-buffer :type git
                           :host github
                           :repo "jsalzbergedu/output-buffer")
  :defer t)

(use-package elisp-checkstyle
  :straight (elisp-checkstyle :type git
                              :host github
                              :repo "jsalzbergedu/elisp-checkstyle")
  :defer t
  :config (setq checkstyle-executable "~/cs-checkstyle/checkstyle")
  :commands (checkstyle-curr-p checkstyle-output-curr))

(use-package gradle-mode
  :defer t
  :straight (emacs-gradle-mode :type git
                               :host github
                               :repo "jacobono/emacs-gradle-mode")

  :after elisp-checkstyle)

(defun checkstyle ()
  (interactive)
  (checkstyle-output-curr))

(use-package dap-java
  :after (dap-mode lsp-java)
  :init
  (defhydra dap-java-testrun-hydra (:hint nil :color blue)
    "
^Debug^                         ^Run Test^                          ^Other^
-----------------------------------------------------------------------------------
_d_: dap-debug-java             _r c_: dap-java-run-test-class     _h_ dap-hydra
_c_: dap-java-run-test-class    _r m_: dap-java-run-test-method
_m_: dap-java-run-test-method"
    ("d" dap-debug-java)
    ("c" dap-java-debug-test-class)
    ("m" dap-java-debug-test-method)
    ("r c" dap-java-run-test-class)
    ("r m" dap-java-run-test-method)
    ("h" dap-hydra)))

;; Required nowaday for lsp-java
(defvar material-design-icons-repo-location "~/sources/material-design-icons/")
(defvar treemacs-material-design-icons-alist '())
(setq treemacs-material-design-icons-alist
      (-map (lambda (x) (cons (car x) (expand-file-name (cdr x) material-design-icons-repo-location)))
            '((dir-open . "file/svg/production/ic_folder_open_24px.svg")
              (dir-closed . "file/svg/production/ic_folder_24px.svg")
              (node-closed . "navigation/svg/production/ic_expand_less_18px.svg")
              (node-open . "navigation/svg/production/ic_chevron_right_18px.svg"))))


(defvar material-design-icons-community-repo-location "~/sources/MaterialDesign/")

(defvar material-design-icons-community-repo-location "~/sources/MaterialDesign/")
(defvar treemacs-material-design-icons-community-alist '())
(setq treemacs-material-design-icons-community-alist
      (-map (lambda (x) (cons (car x) (expand-file-name (cdr x) material-design-icons-community-repo-location)))
            '((package . "icons/svg/package.svg")
              (leaf . "icons/svg/circle-small.svg")
              (java . "~/sources/MaterialDesign/icons/svg/language-java.svg"))))

(defun cdr-assoc (&rest args)
  "Get the CDR of the ASSOC'd list"
  (cdr (apply #'assoc args)))

;; (defmacro setup-icons (&rest icon-list)
;;   "Set up all the icons, turning a into the icon repo location
;; and b into the community repo location.
;; Set up in triples of variables: (vairable-to-set 'some-key <a or b>)"
;;   (-map (lambda (x)
;;           `(treemacs--setup-icon ,(car x) (cdr-assoc ,(cadr x) ,(if (equalp (caddr x) 'a)
;;                                                                     material-design-icons-community-repo-location
;;                                                                   material-design-icons-repo-location)))))

(use-package treemacs
  :straight t
  :config
  (treemacs--setup-icon treemacs-icon-tag-node-closed-png (cdr-assoc 'node-closed treemacs-material-design-icons-alist))
  (treemacs--setup-icon treemacs-icon-tag-node-open-png (cdr-assoc 'node-open treemacs-material-design-icons-alist))
  (treemacs--setup-icon treemacs-icon-open-png (cdr-assoc 'dir-open treemacs-material-design-icons-alist))
  (treemacs--setup-icon treemacs-icon-closed-png (cdr-assoc 'dir-closed treemacs-material-design-icons-alist))
  (treemacs--setup-icon treemacs-icon-tag-leaf-png (cdr-assoc 'leaf treemacs-material-design-icons-community-alist))
  (treemacs--setup-icon treemacs-icon-root-png (cdr-assoc 'package treemacs-material-design-icons-community-alist))
  (treemacs--setup-icon treemacs-icon-java (cdr-assoc 'java
                                                      treemacs-material-design-icons-community-alist) "java")
  (setq treemacs-git-mode 'simple)
  (add-hook 'treemacs-mode-hook (lambda () (setq-local truncate-lines t))))


(define-minor-mode java-project-mode
  "A mode which is activated whenever the java-project variable is t."
  :lighter ""
  :keymap (make-sparse-keymap))

(add-hook 'java-project-mode-hook 'evil-normalize-keymaps)

(evil-define-key 'normal java-project-mode-map (kbd "SPC p") 'hydra-java/body)

(defvar-local is-java-project nil "A local variable that, when set to t,
allows java-project-mode-global to be activated.")

(put 'is-java-project 'safe-local-variable #'booleanp)

(define-globalized-minor-mode java-project-mode-global java-project-mode
  (lambda ()
    (when (or (locate-dominating-file default-directory "pom.xml")
              (locate-dominating-file default-directory ".classpath")
              is-java-project)
        (java-project-mode 1))))


(use-package lsp-java
  :demand t
  :straight (lsp-java :type git
                      :host github
                      :repo "emacs-lsp/lsp-java"
                      :files ("*.el" "icons" "install"))
  :config
  (java-project-mode-global 1)
  (progn (add-hook 'java-mode-hook 'prog-minor-modes-common)
	 (add-hook 'java-mode-hook 'lsp-java-enable)
	 (add-hook 'java-mode-hook (lambda ()
				                 (flycheck-mode 1)
				                 (google-set-c-style)
				                 (google-make-newline-indent)
				                 (setq indent-tabs-mode nil
					               tab-width 4
                                                       c-basic-offset 4))))

  (project-hydra hydra-java
    :test dap-java-testrun-hydra/body
    :compile lsp-java-build-project
    :stylecheck checkstyle
    :search counsel-projectile-rg
    :git hydra-magit/body
    :run dap-java-debug
    :and ("p" counsel-projectile-find-file)
    :and ("e" hydra-flycheck-error/body)
    :and ("a" lsp-ui-sideline-apply-code-actions)
    :and ("d" xref-find-definitions-other-window)
    :and ("f" xref-find-definitions)
    :and ("o" toggle-dap))

  (evil-define-key 'normal java-mode-map (kbd "SPC p") 'hydra-java/body)
  :commands lsp-java-enable)

(use-package lsp-java-treemacs
  :demand t
  :straight nil)

;; Groovy
(use-package groovy-mode
  :straight (groovy-mode :type git
                         :host github
                         :repo "Groovy-Emacs-Modes/groovy-emacs-modes")
  :config (progn (add-hook 'groovy-mode-hook 'prog-minor-modes-common)
		 (add-to-list 'auto-mode-alist '("\\build.gradle\\'" . groovy-mode)))
  :defer t)

;; Scala
(use-package ensime-sbt-mode
  :straight (ensime-sbt-mode
             :type git
             :host github
             :repo "ensime/emacs-sbt-mode")
  :defer t)


(use-package scala-mode
  :defer t
  :straight (scala-mode :type git
                        :host github
                        :repo "ensime/emacs-scala-mode")
  :config (progn (add-hook 'scala-mode-hook 'prog-minor-modes-common)
		 (setq scala-indent:add-space-for-scaladoc-asterisk nil)))

(use-package ensime
  :straight (ensime-emacs :type git
                          :host github
                          :repo "ensime/ensime-emacs")
  :defer t
  :commands ensime
  :config (setq ensime-startup-notification nil))

;; Elisp:
(use-package exec-path-from-shell
  :straight (exec-path-from-shell :type git
                                  :host github
                                  :repo "purcell/exec-path-from-shell")
  :demand t
  :config (exec-path-from-shell-initialize))

(add-hook 'emacs-lisp-mode-hook 'prog-minor-modes-common)

(defun eval-region-advice (eval-region-orig start end &optional printflag read-function)
  (funcall eval-region-orig start end t read-function))
(advice-add 'eval-region :around 'eval-region-advice)

;; Common Lisp:
(use-package slime-autoloads
  :demand t
  :straight (slime-autoloads :type git
                             :host github
                             :repo "slime/slime"
                             :files ("slime-autoloads.el")))

(use-package slime
  :defer t
  :straight (slime :type git
                   :host github
                   :repo "slime/slime"
                   :files ("*"))
  :config
  (add-to-list 'slime-contribs 'slime-fancy)
  (if (file-exists-p (expand-file-name "/usr/lib/quicklisp/slime-helper.el"))
      (load (expand-file-name "/usr/lib/quicklisp/slime-helper.el"))
    (when (file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
      (load (expand-file-name "~/quicklisp/slime-helper.el"))))
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (evil-define-key 'visual slime-mode-map "SPC e" 'slime-eval-region)
  (add-prog-minor-modes-common 'lisp-mode-hook 'slime-repl-mode-hook)
  :init
  (add-hook 'lisp-mode-hook 'prog-minor-modes-common))

;; Scheme
(use-package scheme-complete
  :straight t
  :defer t)

(use-package geiser
  :defer t
  :straight t
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
  :straight (rust-mode :type git
                       :host github
                       :repo "rust-lang/rust-mode"
                       :files ("rust-mode.el"))
  :config
  (add-hook 'rust-mode-hook 'prog-minor-modes-common))

(use-package lsp-rust
  :defer t
  :straight (lsp-rust :type git
                      :host github
                      :repo "emacs-lsp/lsp-rust")
  :init
  (add-hook 'rust-mode-hook #'lsp-rust-enable)
  (add-hook 'rust-mode-hook #'flycheck-mode))

;; Redox:
;; I'll get back to rdxmk later

;; Toml:
(use-package toml-mode
  :defer t
  :straight (toml-mode :type git
                       :host github
                       :repo "dryman/toml-mode.el")
  :config (progn (add-hook 'toml-mode-hook 'prog-minor-modes-common)))

;; TeX:

;; HTML:
(use-package sgml-mode
  :defer t
  :straight nil
  :config (progn (add-hook 'sgml-mode-hook 'prog-minor-modes-common)))

;; Markdown
(use-package markdown-mode
  :defer t
  :straight t
  :config (progn (setq markdown-command "/usr/bin/pandoc")
		 (add-hook 'markdown-mode-hook 'prog-minor-modes-common)))

;; Shell
(use-package sh-script
  :defer t
  :straight nil
  :config (progn (add-to-list 'auto-mode-alist '("\\PKGBUILD\\'" . sh-mode))
		 (add-hook 'sh-mode-hook 'prog-minor-modes-common))
  :commands sh-mode)


;; Chroot
;; TODO

;; C/C++
(use-package cmake-mode
  :defer t
  :straight t
  :config
  (add-hook 'cmake-mode-hook 'prog-minor-modes-common))


(use-package lsp-clangd
  :straight (lsp-clangd :type git
                        :host github
                        :repo "emacs-lsp/lsp-clangd")
  :init (progn (add-hook 'c++-mode-hook (lambda ()
                                          (google-set-c-style)
                                          (google-make-newline-indent)
                                          (setq indent-tabs-mode nil
					        tab-width 2
                                                c-basic-offset 2)
                                          (prog-minor-modes-common)))
               (setq lsp-clangd-executable "/usr/bin/clangd"
                     cmake-style t)))



;; Json
(use-package json-mode
  :defer t
  :straight (json-mode :type git
                       :host github
                       :repo "joshwnj/json-mode")
  :config (progn (add-hook 'json-mode-hook 'prog-minor-modes-common)))

;; Forth
(use-package forth-mode
  :straight (forth-mode :type git
                        :host github
                        :repo "larsbrinkhoff/forth-mode")
  :config (add-hook 'forth-mode-hook 'prog-minor-modes-common)
  :defer t)

(use-package forth-block-mode
  :straight nil
  :demand t
  :after forth-mode)

(use-package forth-interaction-mode
  :straight nil
  :demand t
  :after forth-mode)

;; Idris
(use-package idris-mode
  :defer t
  :straight (idris-mode :type git
                        :host github
                        :repo "idris-hackers/idris-mode")
  :config (add-prog-minor-modes-common 'idris-mode-hook
				       'idris-repl-mode-hook
				       'idris-ipkg-mode-hook))

;; Ruby
(use-package enh-ruby-mode
  :defer t
  :straight (enh-ruby-mode :type git
                           :host github
                           :repo "jacott/Enhanced-Ruby-Mode")
  :init (progn (add-to-list 'auto-mode-alist
                              '("\\.rb\\'" . enh-ruby-mode))
                 (add-to-list 'auto-mode-alist
                              '("\\Rakefile\\'" . enh-ruby-mode)))
  :config (add-hook 'enh-ruby-mode-hook 'prog-minor-modes-common)
  :commands enh-ruby-mode)

(use-package inf-ruby
  :defer t
  :straight (inf-ruby :type git
                      :host github
                      :repo "nonsequitur/inf-ruby")
  :config (add-hook 'inf-ruby-mode-hook 'prog-minor-modes-common))

;; Xml
(use-package nxml-mode
  :defer t
  :straight nil
  :config (add-hook 'nxml-mode-hook 'prog-minor-modes-common))

;; Uml
(use-package plantuml-mode
  :defer t
  :straight (plantuml-mode :type git
                           :host github
                           :repo "skuro/plantuml-mode")
  :init
  (setq plantuml-jar-path "/opt/plantuml/plantuml.jar"))

;; Coq
(use-package proof-general
  :straight t
  :defer t)

;; Haskell
(use-package haskell-mode
  :straight (haskell-mode :type git
                          :host github
                          :repo "haskell/haskell-mode")
  :init (setq haskell-process-type 'stack-ghci)
  :defer t)

(use-package inf-haskell
  :straight nil
  :demand t
  :after haskell-mode)

;; F*
(use-package fstar-mode
  :straight (fstar-mode :type git
                        :host github
                        :repo "FStarLang/fstar-mode.el")
  :defer t)
