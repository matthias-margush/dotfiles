;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package helpful
  :general
  (:keymaps 'help-map
            "f" #'helpful-callable
            "v" #'helpful-variable
            "k" #'helpful-key
            "C" #'helpful-command))

(use-package which-key
  :init
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 2.0
        which-key-allow-evil-operators t)

  :config
  (which-key-mode))

(provide 'help-config)
