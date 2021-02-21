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
  :general
  (:states '(normal) :prefix leader "h" help-map)
  :init
  (setq evil-want-keybinding nil)

  :config
  (evil-mode))

(use-package evil-collection
  :demand t
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t
        evil-collection-company-use-tng nil
        evil-collection-term-sync-state-and-mode-p t
        evil-collection-setup-debugger-keys t
        ;evil-collection-key-blacklist '("SPC")
	)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))
