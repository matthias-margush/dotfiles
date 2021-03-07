;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package general
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
  (defconst leader "SPC")
  (defconst local-leader ","))

(use-package evil
  :custom
  (evil-undo-system 'undo-redo)

  :init
  (setq evil-want-keybinding nil)
  (general-define-key :keymaps 'help-map "F" #'describe-face)
  (general-define-key :states 'normal :prefix leader "h" help-map)
  (general-define-key :states 'normal
           "]e" #'flymake-goto-next-error
           "[e" #'flymake-goto-prev-error
           ",d" #'flymake-show-diagnostics-buffer
           "]q" #'next-error            ; compile window / search results
           "[q" #'previous-error)       ; compile window / search results

  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer nil ; off for selectrum
        evil-collection-company-use-tng t
        evil-collection-term-sync-state-and-mode-p t
        evil-collection-want-unimpaired-p nil
	evil-kill-on-visual-paste nil
        evil-collection-key-blacklist '("SPC")
        evil-collection-setup-debugger-keys t)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :general
  (:states 'normal "gc" #'evil-commentary)
  (:states 'normal "gy" #'evil-commentary-yank)
  ("s-/" #'evil-commentary-line)

  :config
  (evil-commentary-mode))

(use-package multi-line
  :demand t                             ; sets up hooks in various modes
  :general
  (:states 'normal "gs" #'multi-line))

(provide 'evil-config)
