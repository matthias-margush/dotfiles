;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package lispy
  :hook
  ((emacs-lisp-mode . lispy-mode)
   (lisp-mode . lispy-mode)
   (clojure-mode . lispy-mode))

  :commands lispy-mode

  :init
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
  :config
  (lispy-set-key-theme '(parinfer)))


(use-package lispyville
  :general
  (:states 'normal
           :keymaps '(lisp-mode-map emacs-lisp-mode-map)
           ">" #'me/lispyville->
           "<" #'me/lispyville-<
           ",t(" #'me/toggle-lispyville
           ",t)" #'me/toggle-lispyville
           :keymaps '(lisp-mode-map emacs-lisp-mode-map)
           ",eb" #'eval-buffer
           ",ee" #'eval-last-sexp
           ",ef" #'eval-defun)

  (:states 'visual
           :keymaps '(lisp-mode-map emacs-lisp-mode-map)
           ",er" #'eval-region)

  :hook
  ((emacs-lisp-mode . lispyville-mode)
   (lisp-mode . lispyville-mode)
   (clojure-mode . lispyville-mode))

  :init
  (setq
   lispyville-insert-states nil
   lispyville-key-theme '(operators    ; evil ops like yank, delete
                          c-w         ; delete backward word
                          additional-motions
                          additional
                          additional-insert
                          escape))

  (defun me/toggle-lispyville ()
    (interactive)
    (call-interactively 'lispyville-mode)
    (call-interactively 'lispy-mode))

  (evil-define-command me/lispyville-> (count)
    (interactive "<c>")
    (setq count (or count 1))
    (cond ((looking-at lispy-left)
           (lispy-barf count))
          ((looking-at lispy-right)
           (forward-char)
           (lispy-slurp count)
           (backward-char))
          (t (call-interactively 'evil-shift-right)))
    (lispyville--maybe-enter-special t))

  (evil-define-command me/lispyville-< (count)
    (interactive "<c>")
    (setq count (or count 1))
    (cond ((looking-at lispy-left)
           (lispy-slurp count))
          ((looking-at lispy-right)
           (forward-char)
           (lispy-barf count)
           (backward-char))
          (t (call-interactively evil-shift-left)))
    (lispyville--maybe-enter-special t)))

(provide 'lisp-config)
