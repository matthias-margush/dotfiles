;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(unless (display-graphic-p)
  (menu-bar-mode -1))

(global-set-key (kbd "s-}") #'ns-prev-frame)
(global-set-key (kbd "s-{") #'ns-next-frame)
(global-set-key (kbd "s-t") #'make-frame)

(setq frame-alist
  '((top . 100)
    (left . 1000)
    (width . 90)
    (height . 60)
    (internal-border-width . 20)
    (vertical-scroll-bars . nil)
    (font . "Hasklug Nerd Font Mono-10")))
(setq default-frame-alist frame-alist
      initial-frame-alist frame-alist)

(add-hook 'after-init-hook
          (lambda ()
            (set-face-attribute 'variable-pitch nil :font "Open Sans Condensed-16")
            (set-face-attribute 'fixed-pitch nil :font "Hasklug Nerd Font Mono-10")))

(setq-default fringe-indicator-alist nil) ; fringe wrap arrows
(scroll-bar-mode -1)
                                        ; scrollbars
(setq initial-scratch-message "")       ; scratch buffer content
(setq ns-use-proxy-icon nil)            ; frame icon
(tool-bar-mode -1)                      ; toolbar

(use-package construction-paper-theme
  :custom
  (window-divider-default-right-width 4)

  :straight (construction-paper-theme :type git :host github :repo "matthias-margush/construction-paper-emacs")

  :init
  (defun me/modeline-style-line (&optional arg)
    "Style the mode line a simple line."
    (let ((box-color "##312E2A"))

      (set-face-attribute
       'window-divider nil
       :background box-color
       :foreground box-color
       :underline nil
       :box `(:line-width 1 :color ,box-color) :height 0.1)


      (set-face-attribute
       'mode-line nil
       :inherit 'default
       :underline t
       :foreground box-color
       :background box-color
       :box `(:line-width 1 :color ,box-color) :height 0.1)

      (set-face-attribute
       'mode-line-inactive nil
       :background box-color
       :foreground box-color
       :underline t
       :box `(:line-width 1 :color ,box-color) :height 0.2)

      (set-face-attribute
       'mode-line-inactive nil
       :inherit 'default
       :foreground box-color
       :underline t
       :box `(:line-width 1 :color ,box-color) :height 0.1)))

  (setq-default mode-line-format '(""))
  (advice-add 'enable-theme :after #'me/modeline-style-line)
  (add-to-list 'after-make-frame-functions #'me/modeline-style-line t)

  :config
  (window-divider-mode)
  (require 'construction-paper-theme)
  (construction-paper-theme-light))

(provide 'theme-config)
