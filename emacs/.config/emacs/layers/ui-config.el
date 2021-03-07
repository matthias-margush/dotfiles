;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(global-set-key [wheel-right] (lambda () (interactive) (scroll-left 1)))
(global-set-key [wheel-left] (lambda () (interactive) (scroll-right 1)))

;; History
(savehist-mode 1) ;; minibuffer history
(recentf-mode)

;; (setq header-line-format (me/tab-echo))
;; (setq header-line-format (projectile-project-name))
;; (set-face-attribute 'header-line nil :underline (face-foreground 'default))

;; (defun me/header-line ()
;;   (concat " . " (which-function)))

(defun show-file-name ()
  "Show the full path file name."
  (interactive)
  (message "[%s] %s" (line-number-at-pos) (buffer-file-name)))

(global-set-key (kbd "C-S-G") #'show-file-name)

(setq initial-major-mode 'fundamental-mode)

;; (use-package editorconfig :config (editorconfig-mode))

(use-package default-text-scale
  :bind (("s-=" . default-text-scale-increase)
         ("s-+" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)
         ("s-0" . default-text-scale-reset))
  :config (default-text-scale-mode t))

(use-package rainbow-mode)

(require 'dired-x)
(add-hook 'dired-load-hook
          (lambda ()
            (dired-omit-mode)
            (require 'dired-x)))
(setq dired-omit-mode t)

;;; unbind annoying keys
(unbind-key (kbd "s-m"))
(unbind-key (kbd "s-l"))
(unbind-key (kbd "s-p"))

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 1))

(provide 'ui-config)