;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(setq vc-follow-symlinks t)
(setq ediff-window-setup-function #'ediff-setup-windows-plain)

(use-package pinentry
  :config
  (pinentry-start))

(use-package magit
  :commands (magit-status magit-blame-addition)

  :general
  (:keymaps 'magit-mode-map "SPC" nil)  ; magit overrides leader
  (:states 'normal :prefix leader "g" git-map)
  (:keymaps 'magit-mode-map :states 'normal :prefix local-leader
            "r" #'github-review-forge-pr-at-point)

  (:keymaps 'with-editor-mode-map :states 'normal ",cc" #'with-editor-finish)
  (:keymaps 'with-editor-mode-map :states 'normal ",ck" #'with-editor-cancel)

  ;;  smerge
  (:keymaps 'smerge-mode-map :states 'normal "]n" #'smerge-next)
  (:keymaps 'smerge-mode-map :states 'normal "[n" #'smerge-prev)
  (:keymaps 'smerge-mode-map :states 'normal "]]" #'smerge-next)
  (:keymaps 'smerge-mode-map :states 'normal "[[" #'smerge-prev)
  (:keymaps 'smerge-mode-map :prefix local-leader "u" #'smerge-keep-upper)
  (:keymaps 'smerge-mode-map :prefix local-leader "l" #'smerge-keep-lower)
  (:keymaps 'smerge-mode-map :prefix local-leader "a" #'smerge-keep-all)

  (:keymaps 'git-map
            "g" #'magit-status
            "b" #'magit-blame-addition)

  :init
  (defun me/worktree (branch start-point &optional force)
    ""
    (interactive
     `(,@(magit-branch-read-args "Create and checkout branch")
       ,current-prefix-arg))
    (interactive)
    (let ((path (expand-file-name (concat (me/project-root) "../" branch))))
      (magit-worktree-branch path branch start-point force)
      (me/project-notes)))

  (setq git-map (make-sparse-keymap))
  (remove-hook 'magit-section-highlight-hook #'magit-section-highlight)
  (remove-hook 'magit-section-highlight-hook #'magit-diff-highlight)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-diff-refine-hunk 'all))

(use-package forge :after magit
  :init
  (setq magit-filenotify-mode nil
        magit-refresh-status-buffer nil
        forge-pull-notifications nil)

  :config
  (emacsql (forge-db) "PRAGMA journal_mode=WAL")
  (emacsql (forge-db) "PRAGMA synchronous=OFF"))

(use-package github-review)

(use-package git-link
  :commands (git-link)
  :general
  (:states 'visual :map 'git-map  "gl" #'git-link)

  :bind
  (:map git-map ("l" . git-link)))

(use-package diff-hl
  :defer 3
  :init
  (setq diff-hl-flydiff-delay 5)

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
  ;; (diff-hl-flydiff-mode) ; too slow
  )

(provide 'git-config)
