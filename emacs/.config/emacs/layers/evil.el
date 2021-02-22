;; -*- lexical-binding: t; -*-

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

  :general
  (:states 'normal :prefix leader "h" help-map)
  (:states 'normal
           "]e" #'flymake-goto-next-error
           "[e" #'flymake-goto-prev-error
           ",d" #'flymake-show-diagnostics-buffer)

  :init
  (setq evil-want-keybinding nil)

  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t
        evil-collection-company-use-tng nil
        evil-collection-term-sync-state-and-mode-p t
        evil-collection-want-unimpaired-p nil
        ;evil-collection-key-blacklist '("SPC")
        evil-collection-setup-debugger-keys t)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))
