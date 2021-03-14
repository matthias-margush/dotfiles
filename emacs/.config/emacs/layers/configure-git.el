;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(setq vc-follow-symlinks t)

(mm/package 'magit)
(mm/package 'git-link)
(mm/package 'diff-hl)

;; keys
(setq git-map (make-sparse-keymap))

(define-key git-map (kbd "s-k") #'magit-status)
(define-key git-map (kbd "s-b") #'magit-blame-addition)
(define-key git-map (kbd "s-l") #'git-link)

(global-set-key (kbd "s-k") git-map)


;; magit
(remove-hook 'magit-section-highlight-hook #'magit-section-highlight)
(remove-hook 'magit-section-highlight-hook #'magit-diff-highlight)
(setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
      magit-diff-refine-hunk 'all)


;; diff-hl
(add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)
(add-hook 'dired-mode-hook #'diff-hl-dired-mode)
(setq diff-hl-draw-borders nil)
(setq diff-hl-side 'right)

(provide 'configure-git)
