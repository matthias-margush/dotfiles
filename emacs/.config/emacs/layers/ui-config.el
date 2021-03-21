;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(global-set-key [wheel-right] (lambda () (interactive) (scroll-left 1)))
(global-set-key [wheel-left] (lambda () (interactive) (scroll-right 1)))

;; History
(run-with-idle-timer
 5 nil
 (lambda ()
   (savehist-mode) ;; minibuffer history
   (recentf-mode)))

(defun show-file-name ()
  "Show the full path file name."
  (interactive)
  (message "[%s] %s" (line-number-at-pos) (buffer-file-name)))

(global-set-key (kbd "C-S-G") #'show-file-name)

(setq initial-major-mode 'fundamental-mode)

(use-package editorconfig
  :defer 3
  :init
  ;; https://github.com/editorconfig/editorconfig-emacs/issues/244#issuecomment-783127682
  (setq editorconfig--enable-20210221-testing t)

  :config (editorconfig-mode))

(use-package default-text-scale
  :bind (("s-=" . default-text-scale-increase)
         ("s-+" . default-text-scale-increase)
         ("s--" . default-text-scale-decrease)
         ("s-0" . default-text-scale-reset))
  :config (default-text-scale-mode t))

(use-package rainbow-mode
    :commands rainbow-mode)

;;; unbind annoying keys
(unbind-key (kbd "s-m"))
(unbind-key (kbd "s-l"))
(unbind-key (kbd "s-p"))

(use-package adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode)
  :config
  (setq-default adaptive-wrap-extra-indent 1))

(provide 'ui-config)
