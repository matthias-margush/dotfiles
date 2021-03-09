;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(setq vc-follow-symlinks t)

(use-package magit
  :defer 3
  :commands (magit-status magit-blame-addition)

  :general
  (:keymaps 'magit-mode-map "SPC" nil)  ; magit overrides leader
  (:keymaps 'with-editor-mode-map :states 'normal ",cc" #'with-editor-finish)
  (:keymaps 'with-editor-mode-map :states 'normal ",ck" #'with-editor-cancel)
  (:states 'normal :prefix leader "g" git-map)

  (:keymaps 'git-map
    "g" #'magit-status
    "b" #'magit-blame-addition)

  :init
  (setq git-map (make-sparse-keymap))
  (remove-hook 'magit-section-highlight-hook #'magit-section-highlight)
  (remove-hook 'magit-section-highlight-hook #'magit-diff-highlight)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
    magit-diff-refine-hunk 'all))

(use-package forge :after markdown)

(use-package git-link
  :commands (git-link)
  :general
  (:states 'visual :prefix leader "gl" #'git-link)

  :bind
  (:map git-map ("l" . git-link)))

(use-package diff-hl
  :general
  (:keymaps 'normal
    "]g" 'diff-hl-next-hunk
    "[g" 'diff-hl-previous-hunk
    "gh" 'diff-hl-diff-goto-hunk)

  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
    (magit-post-refresh . diff-hl-magit-post-refresh)
    (dired-mode . diff-hl-dired-mode))

  :init
  (setq diff-hl-draw-borders nil
    diff-hl-side 'right)

  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))

(provide 'git-config)
