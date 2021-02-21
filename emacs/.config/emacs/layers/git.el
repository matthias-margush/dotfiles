
(setq git-map (make-sparse-keymap))
(define-key git-map (kbd "k") #'magit-status)
(define-key git-map (kbd "s-k") #'magit-status)
(define-key git-map (kbd "s") #'magit-status)
(define-key git-map (kbd "s-s") #'magit-status)
(define-key git-map (kbd "b") #'magit-blame-addition)
(define-key git-map (kbd "s-b") #'magit-blame-addition)
(define-key git-map (kbd "l") #'git-link)
(define-key git-map (kbd "s-l") #'git-link)

;(define-key leader-map (kbd "g") git-map)
(use-package magit
  :demand t
  :commands (magit-status magit-blame-addition git-link)
  :bind-keymap ("s-k" . git-map)
  :init
  (remove-hook 'magit-section-highlight-hook #'magit-section-highlight)
  (remove-hook 'magit-section-highlight-hook #'magit-diff-highlight)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
	magit-diff-refine-hunk 'all))

(use-package forge :demand t)

(use-package git-link)

;; (use-package git-gutter
;;   :config
;;   (global-git-gutter-mode))

;; (use-package diff-hl
;;   :hook
;;   ((magit-pre-refresh . diff-hl-magit-pre-refresh)
;;    (magit-post-refresh . diff-hl-magit-post-refresh)
;;    (prog-mode . diff-hl-margin-mode)
;;    (org-mode . diff-hl-margin-mode)
;;    (dired-mode . diff-hl-dired-mode) )

;;   :Config
;;   (global-diff-hl-mode))


(use-package diff-hl
  :init
  (add-hook 'prog-mode-hook #'diff-hl-mode)
  (add-hook 'org-mode-hook #'diff-hl-mode)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  :config
  (setq diff-hl-fringe-bmp-function 'diff-hl-fringe-bmp-from-type)
  (setq diff-hl-margin-side 'left)
  (diff-hl-mode t))
