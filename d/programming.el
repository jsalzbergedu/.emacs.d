;;; Programming packages and setup

;; Relevant to all programming before language packages are setup:
(use-package flycheck
  :ensure t)
(require 'company)
(setq company-idle-delay 0.2)
(setq company-minimum-prefix-length 1)

;; Python:
(elpy-enable)

;; Javascript:
(use-package js3
  :defer t)

;; Elisp:
(require 'cl)
(add-hook 'emacs-lisp-mode 'linum-mode)

;; Java:
(add-to-list 'load-path "~/.emacs.d/emacs-eclim/")
(use-package eclim
  :defer t
  :config (progn (global-eclim-mode)))
(use-package eclimd
  :defer t
  :config (progn (require 'company-emacs-eclim)
		 (company-emacs-eclim-setup)
		 (evil-set-initial-state 'eclim-project-mode 'emacs)
		 (defun eclimd-wkspace ()
		   "My convenience function for starting eclimd with a set workspace."
		   (interactive)
		   (start-eclimd "~/workspace/"))))

;; Common Lisp:
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
(require 'slime-autoloads)
(use-package slime
  :defer t
  :config (progn (require 'cl)
		 (add-to-list 'slime-contribs 'slime-fancy)
		 (load (expand-file-name "/usr/lib/quicklisp/slime-helper.el"))
		 (setq inferior-lisp-program "/usr/bin/sbcl")))

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
(add-to-list 'load-path "~/.emacs.d/rdxmk/")
(require 'rdxmk)

;; Toml:
(require 'toml-mode)

;; TeX:

;; HTML:
(add-hook 'sgml-mode-hook 'linum-mode)

;; Relevant to all after setup:
(global-company-mode t)
(add-hook 'prog-mode-hook 'linum-mode)
(use-package indent-tools
  :bind ("C-c i" . indent-tools-hydra/body))
