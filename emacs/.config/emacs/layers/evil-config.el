;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package general
  :demand t
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
  :demand t
  :custom
  (evil-undo-system 'undo-redo)

  :general
  (:keymaps 'help-map "F" #'describe-face)
  (:states 'normal :prefix leader "h" help-map)
  (:states 'normal
    "]e" #'flymake-goto-next-error
    "[e" #'flymake-goto-prev-error
    ",d" #'flymake-show-diagnostics-buffer
    "]q" #'next-error       ; compile window / search results
    "[q" #'previous-error)

  :init
  (setq evil-want-keybinding nil)
                                        ; compile window / search results

  :config
  (evil-mode))

(use-package evil-collection
  :after evil

  :custom
  ((evil-collection-company-use-tng t))

  :init
  (setq evil-collection-setup-minibuffer nil ; off for selectrum
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
  :demand t                         ; sets up hooks in various modes
  :general
  (:states 'normal "gs" #'multi-line))

(use-package iedit
  :general
  (:keymaps 'global-map "C-;" #'iedit-mode)
  (:keymaps 'isearch-mode-map "C-;" #'iedit-mode-from-isearch)
  (:keymaps 'esc-map "C-;" #'iedit-execute-last-modification)
  (:keymaps 'help-map "C-;" #'iedit-mode-toggle-on-function)

  :init
  (setq iedit-toggle-key-default nil))

(use-package hercules :commands hercules-def)

(use-package drag-stuff
  :general
  (:states '(normal visual)
    "M-k" #'drag-stuff-up
    "M-j" #'drag-stuff-down
    "M-l" #'drag-stuff-right
    "M-h" #'drag-stuff-left))

(provide 'evil-config)
