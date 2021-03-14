;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(global-set-key [wheel-right] (lambda () (interactive) (scroll-left 1)))
(global-set-key [wheel-left] (lambda () (interactive) (scroll-right 1)))

;; History
(savehist-mode) ;; minibuffer history
(recentf-mode)

(defun show-file-name ()
  "Show the full path file name."
  (interactive)
  (message "[%s] %s" (line-number-at-pos) (buffer-file-name)))

(global-set-key (kbd "C-S-G") #'show-file-name)

(setq initial-major-mode 'fundamental-mode)

(mm/package 'default-text-scale)
(global-set-key (kbd "s-=") #'default-text-scale-increase)
(global-set-key (kbd "s-+") #'default-text-scale-increase)
(global-set-key (kbd "s--") #'default-text-scale-decrease)
(global-set-key (kbd "s-0") #'default-text-scale-reset)
(default-text-scale-mode)

(mm/package 'rainbow-mode)

;;; unbind annoying keys
(global-set-key (kbd "s-m") nil)
(global-set-key (kbd "s-l") nil)
;; (unbind-key (kbd "s-p"))

(mm/package 'adaptive-wrap)

(provide 'configure-ui)
