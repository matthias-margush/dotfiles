;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(setq line-spacing 0)
(unless (display-graphic-p)
  (menu-bar-mode -1))

(setq me/variable-pitch "Thasadith-12:antialias=true:hinting=false")
;; (setq me/variable-pitch "Open Sans-12:antialias=none")
;; ;; (setq me/variable-pitch "Open Sans-12")
(setq me/fixed-pitch "Hasklug Nerd Font Mono-10:antialias=true=false")
;; (setq me/fixed-pitch "Operator Mono-10:antialias=false:hinting=false")
;; (setq me/fixed-pitch "DejaVuSansMono Nerd Font-10")

(setq frame-alist
  `((top . 100)
    (left . 1200)
    (width . 90)
    (height . 60)
    (internal-border-width . 26)
    (vertical-scroll-bars . nil)
    (font . ,me/fixed-pitch)))

(setq-default left-margin-width 2
              right-margin-width 2)
(set-window-buffer nil (current-buffer))
(setq default-frame-alist frame-alist
      initial-frame-alist frame-alist)

(add-hook 'after-init-hook
          (lambda ()
            (set-face-attribute 'variable-pitch nil :font me/variable-pitch)
            (set-face-attribute 'default nil :font me/fixed-pitch)
            (set-face-attribute 'default nil :weight 'normal)
            (set-face-attribute 'fixed-pitch nil :font me/fixed-pitch)
            (set-face-attribute 'fixed-pitch nil :weight 'extra-light)))


(setq-default fringe-indicator-alist nil) ; fringe wrap arrows
(scroll-bar-mode -1)
                                        ; scrollbars
(setq initial-scratch-message "")       ; scratch buffer content
(setq ns-use-proxy-icon nil)            ; frame icon
(tool-bar-mode -1)                      ; toolbar

;; (add-to-list 'load-path "~/.config/emacs/straight/repos/accent-theme-emacs/")
;; (use-package accent-theme
;;   :straight (accent-theme :type built-in)
;;   )

(use-package accent-theme
  :straight (accent-theme :type git :host github :repo "matthias-margush/accent-theme-emacs")

  :custom
  (window-divider-default-right-width 1)
  (window-divider-default-bottom-width 1)

  ;; :straight (construction-paper-theme :type git :host github :repo "matthias-margush/construction-paper-emacs")

  :init
  ;; (setq accent-theme-accent "#3c4c55")
  ;; (setq accent-theme-accent "#00FF00")
  ;; (setq accent-theme-accent "#8EAF6B")
  ;; (setq accent-theme-accent "#db6c6c")
  ;; (setq accent-theme-accent "#9A0000")
  (setq accent-theme-accent "#B56764")
  ;; (setq accent-theme-accent "#D86C6C")

  (setq-default mode-line-format '(""))

  (defun me/modeline-style-line (&optional arg)
    "Style the mode line a simple line."
    (require 'accent-theme)
    (let ((box-color accent-theme--background-medium))

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
       :box `(:line-width 1 :color ,box-color) :height 0.1)

      (set-face-attribute
       'mode-line-inactive nil
       :inherit 'default
       :foreground box-color
       :underline t
       :box `(:line-width 1 :color ,box-color) :height 0.1)))

  (advice-add 'enable-theme :after #'me/modeline-style-line)

  :init
  ;; (require 'accent-theme)
  ;; (enable-theme 'accent-light)
  (add-to-list 'after-make-frame-functions #'me/modeline-style-line t)
  (window-divider-mode)
  ;; (require 'accent-theme)
  )

(use-package yascroll
  :demand t
  :init
  (setq yascroll:scroll-bar 'right-fringe)
  :config
  ;; (global-yascroll-bar-mode)
  )

(provide 'theme-early-config)
