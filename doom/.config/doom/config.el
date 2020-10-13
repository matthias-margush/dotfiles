;; -*- tab-width: 2 -*-
;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Matthias Margush"
      user-mail-address "matthias.margush@me.com")

(setq sql-password-wallet "~/.sql-wallet.gpg")

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
      mouse-wheel-progressive-speed nil)

(defun me/scroll-left ()
  (interactive)
  (scroll-left 1))

(defun me/scroll-right ()
  (interactive)
  (scroll-right 1))

;; Turn on horizontal scrolling with mouse wheel
(global-set-key [wheel-right] #'me/scroll-left)
(global-set-key [wheel-left] #'me/scroll-right)


(setq doom-theme 'doom-nord-light)
(doom-themes-set-faces nil
  ;; see: doom-themes--colors
  '(org-block :background (doom-color 'modeline-bg))
  '(org-block-begin-line :background (doom-color 'bg))
  '(markdown-header-face :inherit 'variable-pitch))



;; display current funciton in the modeline
(which-function-mode)

;; (use-package! doom-themes
;;   ;; :init
;;   ;(load-theme 'doom-nord-light t)

;;   :init
;;   (doom-themes-set-faces nil
;;     ;; see: doom-themes--colors
;;     '(org-block :background (doom-color 'modeline-bg))
;;     '(org-block-begin-line :background (doom-color 'bg))))


(setq markdown-header-scaling t
      markdown-hide-markup t)

(orgraphy-mode)

;; (use-package! sacred-theme
;;   :config
;;   (require 'sacredforest-theme)
;;   (require 'sacredpaper-theme)
;;   (sacred-theme-forest))

;; (add-hook! sql-mode
;;   (sh-mode)) ;; ensure sh repl is available in sql-mode

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "AverageMono" :size 11 :weight 'Regular)
      doom-variable-pitch-font (font-spec :family "Open Sans Condensed" :size 12))

(setq forge-pull-notifications nil)

(setq-default
 left-margin-width 4
 right-margin-width 4)
(set-window-buffer nil (current-buffer))

(global-hl-line-mode nil)

(add-hook! after-init
  (set-face-italic-p 'italic nil))

;; Empty window header
(setq header-line-format " ")


;; Default size of new windows
(add-to-list 'default-frame-alist '(internal-border-width . 36))
(add-to-list 'default-frame-alist '(top . 150))
(add-to-list 'default-frame-alist '(left . 1125))
(add-to-list 'default-frame-alist '(width . 90))
(add-to-list 'default-frame-alist '(height . 80))


(defun me/sidebar ()
  "Open the sidebar."
  (interactive)
  (require 'treemacs)
  (pcase-let ((`(,x . ,y) (frame-position))
              (border (* 2 (frame-parameter (selected-frame) 'internal-border-width))))
    (setq frame-resize-pixelwise t)
    (pcase (treemacs-current-visibility)
      (`visible
        (progn

          (set-frame-position
           (selected-frame)
           (+ x (* (default-font-width) treemacs-width) border) y)

          (set-frame-width
           (selected-frame)
           (- (frame-width) treemacs-width (/ border (default-font-width))))

          (+treemacs/toggle)))

      (_ ;; not visible
        (progn
          (set-frame-position
           (selected-frame)
           (- x (* (default-font-width) treemacs-width) border) y)

          (set-frame-width
           (selected-frame)
           (+ (frame-width) treemacs-width (/ border (default-font-width))))

          (+treemacs/toggle))))))

(defun me/sidebar-find-file ()
  "Find the current file in the sidebar."
  (interactive)
  (require 'treemacs)
  (pcase-let ((`(,x . ,y) (frame-position))
              (border (* 2 (frame-parameter (selected-frame) 'internal-border-width))))
    (setq frame-resize-pixelwise t)
    (pcase (treemacs-current-visibility)
      (`visible
        (progn

          (set-frame-position
           (selected-frame)
           (+ x (* (default-font-width) treemacs-width) border) y)

          (set-frame-width
           (selected-frame)
           (- (frame-width) treemacs-width (/ border (default-font-width))))

          (+treemacs/toggle)))

      (_ ;; not visible
        (progn
          (set-frame-position
           (selected-frame)
           (- x (* (default-font-width) treemacs-width) border) y)
          (set-frame-width
           (selected-frame)
           (+ (frame-width) treemacs-width (/ border (default-font-width))))
          (treemacs-find-file))))))

(map! :leader
      :desc "Open sidebar"
      "o p" #'me/sidebar)

(map! :leader
      :desc "Open file in sidebar"
      "o P" #'me/sidebar-find-file)

(defun me/eshell-clear ()
  "Clear `eshell' buffer, comint-style."
  (interactive)
  (let ((input (eshell-get-old-input)))
    (eshell/clear-scrollback)
    (eshell-emit-prompt)
    (insert input)))

(add-hook! eshell-mode
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
  (eshell-smart-initialize)
  (map! :map eshell-mode-map :i "M-r" #'counsel-esh-history)
  (map! :map eshell-mode-map :i "C-l" #'me/eshell-clear))

(use-package! doom-modeline
  :init
  (setq doom-modeline-buffer-state-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-indent-info nil)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-percent-position nil)
  (setq doom-modeline-buffer-file-name-style 'auto))

;; (line-number-mode -1)
;; (column-number-mode -1)

(setq doom-localleader-key ",")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-nord)
;;
;; (setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq frame-title-format "\n")
(setq icon-title-format "\n")
(setq ns-use-proxy-icon nil)

(setq treemacs-no-png-images t
      treemacs-width 24)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-hook! writeroom-mode
  (text-scale-decrease 1))
