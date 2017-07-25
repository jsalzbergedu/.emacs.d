;;; Evil
(require 'evil-leader)
(global-evil-leader-mode)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-leader/set-leader "<SPC>")
(defun testfun () "a test function" (interactive) (message "sucess"))
(evil-leader/set-key
  ;; General emacs functions
  "l" 'windmove-right
  "h" 'windmove-left
  "k" 'windmove-up
  "j" 'windmove-down
  "q" 'kill-this-buffer
  "f" 'find-file
  "a" 'switch-to-buffer
  ;; Org Mode
  "<SPC> l" 'org-store-link
  "<SPC> c" 'org-capture
  "<SPC> a" 'org-agenda
  "<SPC> b" 'org-iswitchb)
(global-evil-leader-mode)
(evil-mode 1)
(global-undo-tree-mode)
(setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
(setq evil-motion-state-modes nil)
(use-package evil-surround
  :defer t
  :config (global-evil-surround-mode 1))
(require 'evil-surround)
(global-evil-surround-mode 1)

;;; Evil bindings for other modes and packages

;; Info
(evil-define-key 'normal Info-mode-map
  (kbd "p") 'Info-prev)

;; Hooking term mode to emacs state and char mode to allow TUI programs
(evil-set-initial-state 'term-mode 'emacs)
(add-hook 'term-mode 'term-char-mode)

;;; Ivy, Swiper and Council, and flx:

(require 'flx)
(require 'ivy)
(setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy)))
(ivy-mode 1)
(require 'ivy-hydra)
(require 'counsel)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "C-h u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;;; Other keybindings

;; Mouse scroll through buffers:
(defun mwheel-scroll (event)
  "Scroll up or down according to the EVENT.
This should be bound only to mouse buttons 4 and 5 on non-Windows
systems."
  (interactive (list last-input-event))
  (let ((button (mwheel-event-button event)))
    (cond ((eq button mouse-wheel-down-event)
           (next-buffer))
          ((eq button mouse-wheel-up-event)
           (previous-buffer)))))

;; q in evil:
(global-set-key (kbd "C-c a q") 'quit-window)


;; Bind `C-c g` to the backtick character
(global-set-key (kbd "C-c g") (kbd "`"))
