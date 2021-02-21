(use-package magit
    :commands (magit-status magit-blame-addition git-link)
    :general (:states '(normal) :prefix leader "g" git-map)
    :bind (:map git-map
                ("g" . magit-status)
                ("b" . magit-blame-addition))
    :init
    (setq git-map (make-sparse-keymap))
    (remove-hook 'magit-section-highlight-hook #'magit-section-highlight)
    (remove-hook 'magit-section-highlight-hook #'magit-diff-highlight)
    (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
          magit-diff-refine-hunk 'all))

(use-package forge)

(use-package git-link
    :bind (:map git-map
                ("l" . git-link)))

(use-package diff-hl
  :general
  (:keymaps 'normal
            "]g" 'diff-hl-next-hunk
            "[g" 'diff-hl-previous-hunk)

  :hook
  ((magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh)
   (dired-mode . diff-hl-dired-mode))

  :init
  (setq diff-hl-draw-borders nil)

  :config
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode))
