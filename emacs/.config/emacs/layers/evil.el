;; -*- mode: emacs-lisp; lexical-binding: t; -*-

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
  :after general
  :custom
  (evil-undo-system 'undo-redo)

  :general
  (:states 'normal :prefix leader "h" help-map)
  (:states 'normal
           "]e" #'flymake-goto-next-error
           "[e" #'flymake-goto-prev-error
           ",d" #'flymake-show-diagnostics-buffer)

  :bind (:map help-map ("F" . describe-face))

  :init
  (setq evil-want-keybinding nil)

  :config
  ;; clipboard
  (setq x-select-enable-clipboard t)
  (define-key evil-visual-state-map (kbd "s-c") (kbd "\"+y"))
  (define-key evil-insert-state-map (kbd "s-v") (kbd "C-r +"))
  (define-key evil-ex-completion-map (kbd "s-v") (kbd "C-r +"))
  (define-key evil-normal-state-map (kbd "s-v") (kbd "\"+p"))
  (define-key evil-ex-search-keymap (kbd "s-v") (kbd "C-r +"))

  (evil-mode))

(use-package evil-collection
  :demand t
  :after evil
  :init
  (setq evil-collection-setup-minibuffer t
        evil-collection-company-use-tng nil
        evil-collection-term-sync-state-and-mode-p t
        evil-collection-want-unimpaired-p nil
	evil-kill-on-visual-paste nil
        evil-collection-key-blacklist '("SPC")
        evil-collection-setup-debugger-keys t)
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode))
