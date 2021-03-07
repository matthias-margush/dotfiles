;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)
(require 'evil-config)

(use-package origami
  :general
  (:keymaps 'prog-mode-map :states '(normal) "zo" #'me/forward-open-node)
  (:keymaps 'prog-mode-map :states '(normal) "zc" #'me/forward-close-node)
  (:keymaps 'prog-mode-map :states '(normal) "za" #'me/forward-toggle-node)
  (:keymaps 'prog-mode-map :states '(normal) "<tab>" #'me/forward-toggle-node)
  (:keymaps 'prog-mode-map :states '(normal) "S-<tab>" #'origami-recursively-toggle-node)
  (:keymaps 'prog-mode-map :states '(normal) "zA" #'origami-recursively-toggle-node)
  (:keymaps 'prog-mode-map :states '(normal) "zO" #'origami-recursively-toggle-node)
  (:keymaps 'prog-mode-map :states '(normal) "zC" #'origami-recursively-toggle-node)
  (:keymaps 'prog-mode-map :states '(normal) "zM" #'origami-toggle-all-nodes)
  (:keymaps 'prog-mode-map :states '(normal) "zR" #'origami-toggle-all-nodes)

  :config
  (defun me/forward-open-node ()
    (interactive)
    (save-excursion
      (goto-char (point-at-bol))
      (goto-char (point-at-eol))
      (origami-open-node (current-buffer) (point))))

  (defun me/forward-close-node ()
    (interactive)
    (save-excursion
      (origami-forward-fold)
      (goto-char (point-at-bol))
      (goto-char (point-at-eol))
      (origami-close-node (current-buffer) (point))))

  (defun me/forward-toggle-node ()
    (interactive)
    (save-excursion
      (origami-forward-toggle-node (current-buffer) (point))))

  (global-origami-mode))

(provide 'folding-config)
