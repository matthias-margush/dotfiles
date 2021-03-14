;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(mm/package 'lispy)

(add-hook 'emacs-lisp-mode-hook #'lispy-mode)
(add-hook 'lisp-mode-hook #'lispy-mode)

(setq me/lisp-map (make-sparse-keymap))
(setq lispy-move-after-commenting nil
      lispy-safe-actions-ignore-comments t
      lispy-safe-actions-ignore-strings t
      lispy-compat '(edebug cider)
      lispy-close-quotes-at-end-p t
      lispy-safe-copy t
      lispy-safe-delete t
      lispy-safe-paste t
      lisy-safe-actions-no-pull-delimiters-into-comments t)

(lispy-set-key-theme '(parinfer))

(provide 'configure-lisp)
