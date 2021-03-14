;;; construction-paper-theme.el --- Construction Paper Inspired Theme -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Matthias Margush <matthias.margush@me.com>

;; Author: Matthias Margush <matthias.margush@me.com>
;; URL: https://github.com/matthias-margush/construction-paper-theme
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.4.0"))
;; Keywords: theme, faces

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; construction-paper-theme-inspired theme

;; See the README for more info:
;; https://github.com/matthias-margush/construction-paper-theme

;;; Variables:
(defgroup construction-paper-theme nil
  "Construction paper inspired theme."
  :group 'construction-paper-theme
  :prefix "construction-paper-theme-")

;;; Code:
(defvar construction-paper-theme--shadow-dark "#312E2A")
(defvar construction-paper-theme--shadow-medium "#504F4D")
(defvar construction-paper-theme--shadow-bright "#687A8D")
(defvar construction-paper-theme--light-dark "#E3DCCA")
(defvar construction-paper-theme--light-medium "#f0ecea")
(defvar construction-paper-theme--light-bright "#F9F9F9")
(defvar construction-paper-theme--moss "#557755")
(defvar construction-paper-theme--fire "#884444")
(defvar construction-paper-theme--water "#555577")
(defvar construction-paper-theme--sky "#CFE2FF")
(defvar construction-paper-theme--bark "#c6783e")
(defvar construction-paper-theme--soil "#B2A488")
;; (defvar construction-paper-theme--sun "#C6C200")
(defvar construction-paper-theme--sun "#BBBB69")

(defvar construction-paper-theme--added)
(defvar construction-paper-theme--changed)
(defvar construction-paper-theme--find)
(defvar construction-paper-theme--nav)
(defvar construction-paper-theme--alert)
(defvar construction-paper-theme--background)
(defvar construction-paper-theme--background-medium)
(defvar construction-paper-theme--background-dark)
(defvar construction-paper-theme--inverted-background)
(defvar construction-paper-theme--inverted-background-medium)
(defvar construction-paper-theme--inverted-background-bright)
(defvar construction-paper-theme--warning)
(defvar construction-paper-theme--block)
(defvar construction-paper-theme--deemphasize)
(defvar construction-paper-theme--doc)
(defvar construction-paper-theme--good)
(defvar construction-paper-theme--highlight-background)
(defvar construction-paper-theme--highlight-foreground)
(defvar construction-paper-theme--keyword)
(defvar construction-paper-theme--link)
(defvar construction-paper-theme--removed)
(defvar construction-paper-theme--text)
(defvar construction-paper-theme--inverted-text)

(defun construction-paper-theme (theme-name)
  "Create a construction-paper-theme theme named THEME-NAME."
  (custom-theme-set-faces
   theme-name
   ;; Built-in stuff (Emacs 23)
   `(border ((t (:background ,construction-paper-theme--text))))
   `(internal-border ((t (:background ,construction-paper-theme--background))))
   `(border-glyph ((t (nil))))
   `(cursor ((t (:background ,construction-paper-theme--text))))
   `(default ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--text))))
   `(fringe ((t (:background ,construction-paper-theme--background))))
   `(gui-element ((t (:background ,construction-paper-theme--text :foreground ,construction-paper-theme--added))))
   `(highlight-background ((t (:background ,construction-paper-theme--background))))
   `(link ((t (:foreground ,construction-paper-theme--link :underline t))))
   `(xref-match ((t (:foreground ,construction-paper-theme--link :underline t))))
   `(link-visited ((t (:foreground ,construction-paper-theme--link))))
   `(minibuffer-prompt ((t (:inherit default :inverse-video nil))))
   `(helpful-heading ((t (:inherit variable-pitch :height 1.4 :foreground ,construction-paper-theme--text))))
   `(outline-1 ((t (:weight bold :foreground ,construction-paper-theme--text))))
   `(outline-2 ((t (:weight bold :foreground ,construction-paper-theme--text))))
   `(outline-3 ((t (:weight bold :foreground ,construction-paper-theme--text))))
   `(outline-4 ((t (:weight bold :foreground ,construction-paper-theme--text))))
   `(outline-5 ((t (:weight bold :foreground ,construction-paper-theme--text))))
   `(outline-6 ((t (:weight bold :foreground ,construction-paper-theme--text))))
   `(outline-7 ((t (:weight bold :foreground ,construction-paper-theme--text))))
   `(outline-8 ((t (:weight bold :foreground ,construction-paper-theme--text))))
   `(region ((t (:background ,construction-paper-theme--highlight-background))))
   `(secondary-selection ((t (:background ,construction-paper-theme--text :foreground ,construction-paper-theme--background))))
   `(error ((t (:foreground ,construction-paper-theme--alert :weight bold))))
   `(warning ((t (:foreground ,construction-paper-theme--doc :weight bold))))
   `(success ((t (:foreground ,construction-paper-theme--keyword :weight bold))))
   `(header-line ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--good :box (:color ,construction-paper-theme--background :line-width 10) :underline nil :weight normal))))

   `(escape-glyph ((t (:foreground ,construction-paper-theme--link))))

   `(tab-active ((t (:inherit outline-5 :foreground ,construction-paper-theme--moss :weight bold))))
   `(tab-inactive ((t (:inherit outline-5))))

   `(highlight-indentation-face ((t (:background ,construction-paper-theme--light-dark))))

   ;; mode line
   `(mode-line ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--good :box (:color ,construction-paper-theme--background :line-width 4) :underline nil :weight normal :inverse-video nil))))
   `(mode-line-inactive ((t (:inherit mode-line))))
   `(mode-line-buffer-id ((t (:inherit mode-line))))
   `(mode-line-buffer-id-inactive ((t (:inherit mode-line-inactive))))
   `(mode-line-emphasis ((t (:inherit mode-line :weight bold))))
   `(vertical-border ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--inverted-background :box nil :underline nil :weight normal :inverse-video nil))))
   `(eyebrowse-mode-line-inactive ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--keyword :box nil :weight normal :inverse-video t))))
   `(eyebrowse-mode-line-separator ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--keyword :box nil :weight normal :inverse-video t))))
   `(eyebrowse-mode-line-delimiters ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--keyword :box nil :weight normal :inverse-video t))))
   `(mode-line-highlight-background ((t (:inherit mode-line :weight bold))))
   `(powerline-active1 ((t (:inherit mode-line :inverse-video t))))
   `(powerline-active2 ((t (:inherit mode-line :background ,construction-paper-theme--background-dark))))
   `(powerline-inactive1 ((t (:inherit modeline-inactive :background ,construction-paper-theme--background-medium))))
   `(powerline-inactive2 ((t (:inherit modeline-inactive :background ,construction-paper-theme--background-medium))))
   `(persp-face-lighter-buffer-not-in-persp ((t (:weight bold :foreground ,construction-paper-theme--inverted-text :background ,construction-paper-theme--alert))))


   `(scroll-bar ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))

   ;; Font-lock stuff
   `(font-lock-builtin-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,construction-paper-theme--doc))))
   `(font-lock-comment-face ((t (:foreground ,construction-paper-theme--doc))))
   `(font-lock-constant-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(font-lock-doc-face ((t (:foreground ,construction-paper-theme--doc))))
   `(font-lock-doc-string-face ((t (:foreground ,construction-paper-theme--text))))
   `(font-lock-function-name-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(font-lock-keyword-face ((t (:foreground ,construction-paper-theme--keyword :weight bold))))
   `(font-lock-negation-char-face ((t (:foreground ,construction-paper-theme--alert))))
   `(font-lock-preprocessor-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,construction-paper-theme--link))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,construction-paper-theme--link))))
   `(font-lock-string-face ((t (:foreground ,construction-paper-theme--moss))))
   `(font-lock-type-face ((t (:foreground ,construction-paper-theme--text))))
   `(font-lock-variable-name-face ((t (:foreground ,construction-paper-theme--text))))
   `(font-lock-warning-face ((t (:foreground ,construction-paper-theme--sun))))

   `(which-func ((t (:inherit variable-pitch :foreground ,construction-paper-theme--soil :weight normal :height 0.8))))
   `(header-line-path ((t (:inherit variable-pitch :foreground ,construction-paper-theme--moss :weight normal :height 0.8))))

   ;; web mode
   `(web-mode-html-tag-face ((t (:inherit font-lock-keyword-face))))
   `(web-mode-error-face ((t (:inherit error))))
   `(web-mode-block-attr-name-face ((t (:foreground ,construction-paper-theme--moss))))
   `(web-mode-block-attr-value-face ((t (:foreground ,construction-paper-theme--water))))
   `(web-mode-block-face ((t (:foreground ,construction-paper-theme--sun))))
   `(web-mode-current-element-highlight-face ((t (:foreground ,construction-paper-theme--background :background ,construction-paper-theme--text))))
   `(web-mode-current-column-highlight-face ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--text))))
   `(web-mode-inlay-face ((t (:foreground ,construction-paper-theme--inverted-text :background ,construction-paper-theme--sun))))
   `(web-mode-json-context-face ((t (:foreground ,construction-paper-theme--keyword :background ,construction-paper-theme--background))))
   `(web-mode-json-key-face ((t (:foreground ,construction-paper-theme--keyword :background ,construction-paper-theme--background))))
   `(web-mode-symbol-face ((t (:foreground ,construction-paper-theme--keyword :background ,construction-paper-theme--background))))
   `(web-mode-whitespace-face ((t (:foreground ,construction-paper-theme--alert :background ,construction-paper-theme--alert))))

   ;; transient
   `(transient-disabled-suffix ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(transient-enabled-suffix ((t (:background ,construction-paper-theme--moss :foreground ,construction-paper-theme--inverted-text))))

   ;; tty
   `(tty-menu-disabled-face ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(tty-menu-enabled-face ((t (:background ,construction-paper-theme--water :foreground ,construction-paper-theme--inverted-text))))
   `(tty-menu-selected-face ((t (:background ,construction-paper-theme--moss :foreground ,construction-paper-theme--inverted-text))))

   ;; linum-mode
   `(linum ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--deemphasize))))

   ;; hl-todo
   `(hl-todo ((t (:foreground ,construction-paper-theme--alert))))

   ;; imenu
   `(imenu-list-entry-face ((t (:foreground ,construction-paper-theme--sun))))
   `(imenu-list-entry-face-0 ((t (:foreground ,construction-paper-theme--sun))))
   `(imenu-list-entry-face-1 ((t (:foreground ,construction-paper-theme--moss))))
   `(imenu-list-entry-face-2 ((t (:foreground ,construction-paper-theme--water))))
   `(imenu-list-entry-face-3 ((t (:foreground ,construction-paper-theme--soil))))
   `(imenu-list-entry-subalist-face-0 ((t (:foreground ,construction-paper-theme--sun :weight bold))))
   `(imenu-list-entry-subalist-face-1 ((t (:foreground ,construction-paper-theme--moss :weight bold))))
   `(imenu-list-entry-subalist-face-2 ((t (:foreground ,construction-paper-theme--water :weight bold))))
   `(imenu-list-entry-subalist-face-3 ((t (:foreground ,construction-paper-theme--soil :weight bold))))

   ;; misc
   `(holiday ((t (:foreground ,construction-paper-theme--sun))))
   `(homoglyph ((t (:foreground ,construction-paper-theme--link))))

   ;; auto-highlight-symbol
   `(ahs-definition-face ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--link :underline t))))
   `(ahs-edit-mode-face ((t (:background ,construction-paper-theme--background :box (:line-width 1 :color ,construction-paper-theme--deemphasize)))))
   `(ahs-face ((t (:background ,construction-paper-theme--background :box (:line-width 1 :color ,construction-paper-theme--deemphasize)))))
   `(ahs-plugin-bod-face ((t (:background ,construction-paper-theme--background (:line-width 1 :color ,construction-paper-theme--good)))))
   `(ahs-plugin-defalt-face ((t (:background ,construction-paper-theme--background (:line-width 1 :color ,construction-paper-theme--good)))))
   `(ahs-plugin-whole-buffer-face ((t (:background ,construction-paper-theme--background (:line-width 1 :color ,construction-paper-theme--good)))))
   `(ahs-warning-face ((t (:background ,construction-paper-theme--alert :foreground ,construction-paper-theme--inverted-text))))

   ;; cua
   `(cua-global-mark ((t (:background ,construction-paper-theme--highlight-background :foreground ,construction-paper-theme--highlight-foreground))))
   `(cua-rectangle ((t (:background ,construction-paper-theme--highlight-background :foreground ,construction-paper-theme--highlight-foreground))))
   `(cua-rectangle-no-select ((t (:background ,construction-paper-theme--highlight-background :foreground ,construction-paper-theme--highlight-foreground))))

   ;; anzu
   `(anzu-match-1 ((t (:background ,construction-paper-theme--good :foreground ,construction-paper-theme--inverted-text))))
   `(anzu-match-2 ((t (:background ,construction-paper-theme--keyword :foreground ,construction-paper-theme--inverted-text))))
   `(anzu-match-3 ((t (:background ,construction-paper-theme--link :foreground ,construction-paper-theme--inverted-text))))
   `(anzu-mode-line ((t (:foreground ,construction-paper-theme--text))))
   `(anzu-mode-line-no-match ((t (:foreground ,construction-paper-theme--alert))))
   `(anzu-replace-highlight ((t (:background ,construction-paper-theme--good :foreground ,construction-paper-theme--inverted-text))))
   `(anzu-replace-to ((t (:background ,construction-paper-theme--good :foreground ,construction-paper-theme--inverted-text))))

   ;; spacebar
   `(spacebar-persp ((t (:inherit variable-pitch :background ,construction-paper-theme--background :foreground ,construction-paper-theme--nav :height 1.0))))
   `(spacebar-active ((t (:inherit variable-pitch :background ,construction-paper-theme--background :foreground ,construction-paper-theme--nav :height 1.1 :weight normal))))
   `(spacebar-inactive ((t (:inherit variable-pitch :background ,construction-paper-theme--background :foreground ,construction-paper-theme--deemphasize :weight normal :height 1.1))))

   ;; smerge
   `(smerge-base ((t (:foreground ,construction-paper-theme--background-medium))))
   `(smerge-base-lower ((t (:foreground ,construction-paper-theme--background-medium))))
   `(smerge-markers ((t (:foreground ,construction-paper-theme--deemphasize))))
   `(smerge-refined-added ((t (:background ,construction-paper-theme--added))))
   `(smerge-refined-changed ((t (:background ,construction-paper-theme--changed))))
   `(smerge-refined-removed ((t (:background ,construction-paper-theme--removed))))
   `(smerge-upper ((t (:background ,construction-paper-theme--background-medium))))
   `(smerge-lower ((t (:background ,construction-paper-theme--background-medium))))

   ;; spacemacs
   `(spacemacs-emacs-face ((t (:background ,construction-paper-theme--water :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-evilified-face ((t (:background ,construction-paper-theme--sun :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-helm-navigation-ts-face ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-hybrid-face ((t (:background ,construction-paper-theme--water :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-ido-navigation-ts-face ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-iedit-face ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-iedit-insert-face ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-insert-face ((t (:background ,construction-paper-theme--moss :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-lisp-face ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-micro-state-binding-face ((t (:background ,construction-paper-theme--sun :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-micro-state-header-face ((t (:weight bold :box (:line-width -1 :color ,(plist-get (face-attribute 'mode-line :box) :color) :style nil) :foreground ,construction-paper-theme--sun :background ,construction-paper-theme--inverted-text))))
   `(spacemacs-motion-face ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-normal-face ((t (:background ,construction-paper-theme--inverted-background :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-replace-face ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(spacemacs-visual-face ((t (:background ,construction-paper-theme--text :foreground ,construction-paper-theme--background))))

   ;; symbol overlay
   `(symbol-overlay-face-1 ((t (:background ,construction-paper-theme--water :foreground ,construction-paper-theme--inverted-text))))
   `(symbol-overlay-face-2 ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(symbol-overlay-face-3 ((t (:background ,construction-paper-theme--sun :foreground ,construction-paper-theme--inverted-text))))
   `(symbol-overlay-face-4 ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(symbol-overlay-face-5 ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(symbol-overlay-face-6 ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--inverted-text))))
   `(symbol-overlay-face-7 ((t (:background ,construction-paper-theme--moss :foreground ,construction-paper-theme--inverted-text))))
   `(symbol-overlay-face-8 ((t (:background ,construction-paper-theme--water :foreground ,construction-paper-theme--inverted-text))))

   ;; table.el
   `(table-cell ((t (:background ,construction-paper-theme--water :foreground ,construction-paper-theme--inverted-text))))

   ;; sp
   `(sp-wrap-overlay-closing-pair ((t (:foreground ,construction-paper-theme--fire :background ,construction-paper-theme--sun))))
   `(sp-wrap-overlay-opening-pair ((t (:foreground ,construction-paper-theme--inverted-text :background ,construction-paper-theme--sun))))

   ;; ivy
   `(ivy-posframe-border ((t (:background ,construction-paper-theme--highlight-background))))
   `(ivy-org ((t (:inherit default))))
   `(ivy-grep-info ((t (:inherit default :foreground ,construction-paper-theme--link))))
   `(ivy-grep-line-number ((t (:inherit default :foreground ,construction-paper-theme--link))))
   `(ivy-confirm-face ((t (:inverse-video t))))
   `(ivy-virtual ((t (:inherit default))))
   `(ivy-remote ((t (:inherit default))))
   `(ivy-posframe ((t (:background ,construction-paper-theme--highlight-background :foreground ,construction-paper-theme--text))))
   `(ivy-posframe-cursor ((t (:background ,construction-paper-theme--highlight-background :foreground ,construction-paper-theme--text))))
   `(ivy-prompt-match ((t (:foreground ,construction-paper-theme--good))))
   `(ivy-confirm-face ((t (:inherit minibuffer-prompt :foreground ,construction-paper-theme--text))))
   `(ivy-current-match ((t (:weight bold :background ,construction-paper-theme--highlight-background))))
   `(ivy-match-required-face ((t (:inherit minibuffer-prompt :foreground ,construction-paper-theme--alert))))
   `(ivy-minibuffer-match-face-1 ((t (:weight bold :foreground ,construction-paper-theme--keyword))))
   `(ivy-minibuffer-match-face-2 ((t (:weight bold :foreground ,construction-paper-theme--keyword))))
   `(ivy-minibuffer-match-face-3 ((t (:weight bold :foreground ,construction-paper-theme--keyword))))
   `(ivy-minibuffer-match-face-4 ((t (:weight bold :foreground ,construction-paper-theme--keyword))))
   `(ivy-highlight-face ((t (:underline (:color ,construction-paper-theme--highlight-background)))))
   `(ivy-remote ((t (:foreground ,construction-paper-theme--link))))

   ;; hydra
   `(hydra-face-amaranth ((t (:foregound ,construction-paper-theme--text :weight bold))))
   `(hydra-face-blue ((t (:foreground ,construction-paper-theme--text :weight bold))))
   `(hydra-face-pink ((t (:foreground ,construction-paper-theme--text :weight bold))))
   `(hydra-face-red ((t (:foreground ,construction-paper-theme--text :weight bold))))
   `(hydra-face-teal ((t (:foreground ,construction-paper-theme--text :weight bold))))

   ;; mu4e
   `(mu4e-header-highlight-background-face ((t (:inherit region :weight bold))))
   `(mu4e-title-face ((t (:foreground ,construction-paper-theme--text :weight bold))))
   `(mu4e-highlight-face ((t (:foreground ,construction-paper-theme--text :weight bold))))
   `(mu4e-context-face ((t (:foreground ,construction-paper-theme--text :weight bold))))

   ;; neotree
   `(neo-banner-face ((t (:foreground ,construction-paper-theme--text :weight normal))))
   `(neo-dir-link-face ((t (:foreground ,construction-paper-theme--text))))
   `(neo-expand-btn-face ((t (:foreground ,construction-paper-theme--text))))
   `(neo-file-link-face ((t (:foreground ,construction-paper-theme--text))))
   `(neo-root-dir-face ((t (:foreground ,construction-paper-theme--text :weight normal))))
   `(neo-vc-added-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(neo-vc-conflict-face ((t (:foreground ,construction-paper-theme--alert))))
   `(neo-vc-edited-face ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(neo-vc-ignored-face ((t (:foreground ,construction-paper-theme--deemphasize))))
   `(neo-vc-missing-face ((t (:foreground ,construction-paper-theme--alert))))
   `(neo-vc-needs-merge-face ((t (:foreground ,construction-paper-theme--alert))))
   `(neo-vc-unlocked-changes-face ((t (:background ,construction-paper-theme--link :foreground ,construction-paper-theme--background))))
   `(neo-vc-up-to-date-face ((t (:foreground ,construction-paper-theme--text))))
   `(neo-vc-user-face ((t (:foreground ,construction-paper-theme--alert :slant italic))))

   ;; Search
   `(lazy-highlight-background ((t (:background ,construction-paper-theme--find :weight normal))))
   `(match ((t (:background ,construction-paper-theme--highlight-background))))
   `(isearch ((t (:background ,construction-paper-theme--sky))))
   `(isearch-lazy-highlight-background-face ((t (:box ,construction-paper-theme--added))))
   `(lazy-highlight ((t (:background "#F3F3FF"))))
   `(isearch-fail ((t (:foreground ,construction-paper-theme--alert))))

   ;; evil
   `(evil-search-highlight-background-persist-highlight-background-face ((t (:background ,construction-paper-theme--find :inherit font-lock-warning-face :inverse-video t))))
   `(evil-ex-lazy-highlight ((t (:foreground ,construction-paper-theme--inverted-text :background ,construction-paper-theme--find))))
   `(evil-ex-info ((t (:foreground ,construction-paper-theme--good))))
   `(evil-ex-substitute-replacement ((t (:underline (:color ,construction-paper-theme--moss)))))

   ;; Popups
   `(popup-face ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--added))))
   `(popup-isearch-match ((t (:foreground ,construction-paper-theme--background :background ,construction-paper-theme--highlight-background))))
   `(popup-scroll-bar-background-face ((t (:background ,construction-paper-theme--doc))))
   `(popup-scroll-bar-foreground-face ((t (:background ,construction-paper-theme--text))))
   `(popup-summary-face ((t (:foreground ,construction-paper-theme--text))))
   `(popup-tip-face ((t (:background ,construction-paper-theme--inverted-background :foreground ,construction-paper-theme--inverted-text))))
   `(popup-menu-mouse-face ((t (:foreground ,construction-paper-theme--background :background ,construction-paper-theme--keyword))))
   `(popup-menu-selection-face ((t (:foreground ,construction-paper-theme--background :background ,construction-paper-theme--keyword))))

   ;; Flymake / Flycheck / Flyspell
   `(flymake-error ((t (:underline (:color ,construction-paper-theme--alert :style wave)))))
   `(flycheck-error ((t (:underline (:color ,construction-paper-theme--alert :style wave)))))
   `(flymake-warning ((t (:underline (:color ,construction-paper-theme--deemphasize :style wave)))))
   `(flycheck-error-list-highlight ((t (:background ,construction-paper-theme--changed :foreground ,construction-paper-theme--inverted-text))))
   `(flycheck-info ((t (:underline (:color ,construction-paper-theme--good :style wave)))))
   `(flycheck-warning ((t (:underline (:color ,construction-paper-theme--warning :style wave)))))
   `(flyspell-duplicate ((t (:underline (:color ,construction-paper-theme--warning :style wave)))))
   `(flyspell-incorrect ((t (:underline (:color ,construction-paper-theme--alert :style wave)))))

   ;; Emacs lisp
   `(eval-sexp-fu-flash ((t (:foreground ,construction-paper-theme--good))))
   `(eval-sexp-fu-flash-error ((t (:foreground ,construction-paper-theme--alert))))

   ;; Clojure errors
   `(clojure-test-failure-face ((t (:background nil :inherit flymake-warnline))))
   `(clojure-test-error-face ((t (:background nil :inherit flymake-errline))))
   `(clojure-test-success-face ((t (:background nil :foreground nil :underline ,construction-paper-theme--highlight-background))))

   ;; For Brian Carper's extended clojure syntax table
   `(clojure-keyword ((t (:foreground ,construction-paper-theme--link))))
   `(clojure-parens ((t (:foreground ,construction-paper-theme--added))))
   `(clojure-braces ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(clojure-brackets ((t (:foreground ,construction-paper-theme--link))))
   `(clojure-double-quote ((t (:foreground ,construction-paper-theme--keyword :background nil))))
   `(clojure-special ((t (:foreground ,construction-paper-theme--keyword))))
   `(clojure-java-call ((t (:foreground ,construction-paper-theme--link))))

   ;; Avy
   `(avy-background-face ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--text))))
   `(avy-goto-char-timer-face ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--text))))
   `(avy-lead-face ((t (:background ,construction-paper-theme--link :foreground ,construction-paper-theme--inverted-text :weight bold))))
   `(avy-lead-face-0 ((t (:background ,construction-paper-theme--link :foreground ,construction-paper-theme--inverted-text :weight bold))))
   `(avy-lead-face-1 ((t (:background ,construction-paper-theme--link :foreground ,construction-paper-theme--inverted-text :weight bold))))
   `(avy-lead-face-2 ((t (:background ,construction-paper-theme--link :foreground ,construction-paper-theme--inverted-text :weight bold))))

   ;; ace window
   `(aw-background-face ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(aw-key-face ((t (:foreground ,construction-paper-theme--changed))))
   `(aw-leading-char-face ((t (:background ,construction-paper-theme--changed :foreground ,construction-paper-theme--inverted-text :height 2.5))))
   `(aw-minibuffer-leading-char-face ((t (:background ,construction-paper-theme--changed :foreground ,construction-paper-theme--inverted-text :height 2.5))))
   `(aw-mode-line-face ((t (:background ,construction-paper-theme--changed :foreground ,construction-paper-theme--inverted-text :height 2.5))))

   ;; MMM-mode
   `(mmm-code-submode-face ((t (:background ,construction-paper-theme--text))))
   `(mmm-comment-submode-face ((t (:inherit font-lock-comment-face))))
   `(mmm-output-submode-face ((t (:background ,construction-paper-theme--text))))

   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((t (:foreground ,construction-paper-theme--link))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,construction-paper-theme--link))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,construction-paper-theme--text))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,construction-paper-theme--added))))

   ;; IDO
   `(ido-first-match ((t (:foreground ,construction-paper-theme--text :weight bold))))
   `(ido-indicator ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--highlight-background :width condensed))))
   `(ido-only-match ((t (:foreground ,construction-paper-theme--text :weight bold))))
   `(ido-subdir ((t (:foreground ,construction-paper-theme--text))))
   `(ido-incomplete-regexp ((t (:foreground ,construction-paper-theme--text))))
   `(ido-incomplete-virtual ((t (:foreground ,construction-paper-theme--text))))
   `(ido-virtual ((t (:foreground ,construction-paper-theme--text))))
   `(flx-highlight-face ((t (:foreground ,construction-paper-theme--text :underline t))))

   ;; Company
   `(company-echo-common ((t (:foreground ,construction-paper-theme--text))))
   `(company-preview ((t (:height 1.0 :foreground ,construction-paper-theme--text))))
   `(company-preview-common ((t (:weight bold :foreground ,construction-paper-theme--text))))
   `(company-preview-search ((t (:weight bold :foreground ,construction-paper-theme--text))))
   `(company-scrollbar-bg ((t (:background "#56585A"))))
   `(company-scrollbar-fg ((t (:background "#3a3a3a"))))
   `(company-template-field ((t (:background "#56585A" :foreground ,construction-paper-theme--moss))))
   `(company-tooltip ((t (:background ,construction-paper-theme--background-medium :foreground ,construction-paper-theme--text))))
   `(company-tooltip-mouse ((t (:background ,construction-paper-theme--background-medium :foreground ,construction-paper-theme--text))))
   `(company-tooltip-annotation ((t (:foreground ,construction-paper-theme--soil))))
   `(company-tooltip-common ((t (:inherit company-tooltip :weight bold :foreground ,construction-paper-theme--text))))
   `(company-tooltip-common-selection ((t (:weight bold :foreground  ,construction-paper-theme--text))))
   `(company-tooltip-selection ((t (:background ,construction-paper-theme--background-dark :foreground ,construction-paper-theme--text))))

   ;; which-function
   `(which-func ((t (:foreground ,construction-paper-theme--keyword :background nil :weight bold))))

   `(trailing-whitespace ((t (:background ,construction-paper-theme--fire :foreground ,construction-paper-theme--link))))
   `(whitespace-empty ((t (:foreground ,construction-paper-theme--keyword :background ,construction-paper-theme--link))))
   `(whitespace-hspace ((t (:background ,construction-paper-theme--doc :foreground ,construction-paper-theme--doc))))
   `(whitespace-indentation ((t (:background ,construction-paper-theme--link :foreground ,construction-paper-theme--keyword))))
   `(whitespace-line ((t (:background ,construction-paper-theme--removed))))
   `(whitespace-newline ((t (:foreground ,construction-paper-theme--doc))))
   `(whitespace-space ((t (:foreground ,construction-paper-theme--doc))))
   `(whitespace-space-after-tab ((t (:background ,construction-paper-theme--link :foreground ,construction-paper-theme--keyword))))
   `(whitespace-space-before-tab ((t (:background ,construction-paper-theme--keyword :foreground ,construction-paper-theme--keyword))))
   `(whitespace-tab ((t (:background ,construction-paper-theme--doc :foreground ,construction-paper-theme--doc))))
   `(whitespace-trailing ((t (:background ,construction-paper-theme--keyword :foreground ,construction-paper-theme--link))))

   ;; Parenthesis matching (built-in)
   `(show-paren-match ((t (:weight bold))))
   `(show-paren-mismatch ((t (:background ,construction-paper-theme--alert))))

   ;; Parenthesis matching (mic-paren)
   `(paren-face-match ((t (:foreground nil :background nil :inherit show-paren-match))))
   `(paren-face-mismatch ((t (:foreground nil :background nil :inherit show-paren-mismatch))))
   `(paren-face-no-match ((t (:foreground nil :background nil :inherit show-paren-mismatch))))

   ;; Parenthesis dimming (parenface)
   `(paren-face ((t (:foreground ,construction-paper-theme--doc :background nil))))

   `(sh-heredoc ((t (:foreground nil :inherit font-lock-string-face :weight normal))))
   `(sh-quoted-exec ((t (:foreground nil :inherit font-lock-preprocessor-face))))
   `(slime-highlight-background-edits-face ((t (:weight bold))))
   `(slime-repl-input-face ((t (:weight normal :underline nil))))
   `(slime-repl-prompt-face ((t (:underline nil :weight bold :foreground ,construction-paper-theme--link))))
   `(slime-repl-result-face ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(slime-repl-output-face ((t (:foreground ,construction-paper-theme--keyword :background ,construction-paper-theme--text))))

   `(csv-separator-face ((t (:foreground ,construction-paper-theme--keyword))))

   `(diff-added ((t (:foreground ,construction-paper-theme--moss))))
   `(diff-changed ((t (:foreground ,construction-paper-theme--changed))))
   `(diff-removed ((t (:foreground ,construction-paper-theme--fire))))
   `(diff-header ((t (:background ,construction-paper-theme--background-medium :foreground ,construction-paper-theme--text))))
   `(diff-file-header ((t (:background ,construction-paper-theme--background-medium :foreground ,construction-paper-theme--keyword))))
   `(diff-hunk-header ((t (:background ,construction-paper-theme--background-medium :foreground ,construction-paper-theme--link))))
   `(diff-refine-added ((t (:background "#CCDFFC"))))
   `(diff-refine-changed ((t (:background "#FCFCCC"))))
   `(diff-refine-removed ((t (:background "#F6CCCC"))))

   `(diff-hl-change ((t (:foreground ,construction-paper-theme--sun :background ,construction-paper-theme--sun))))
   `(diff-hl-delete ((t (:foreground "#F6CCCC" :background "#F6CCCC"))))
   `(diff-hl-insert ((t (:foreground "#CCDFFC" :background "#CCDFCC"))))

   `(ediff-even-diff-A ((t (:foreground nil :background nil :inverse-video t))))
   `(ediff-even-diff-B ((t (:foreground nil :background nil :inverse-video t))))
   `(ediff-odd-diff-A  ((t (:foreground ,construction-paper-theme--doc :background nil :inverse-video t))))
   `(ediff-odd-diff-B  ((t (:foreground ,construction-paper-theme--doc :background nil :inverse-video t))))

   `(eldoc-highlight-background-function-argument ((t (:foreground ,construction-paper-theme--highlight-background :weight bold))))

   ;; undo-tree
   `(undo-tree-visualizer-default-face ((t (:foreground ,construction-paper-theme--added))))
   `(undo-tree-visualizer-current-face ((t (:foreground ,construction-paper-theme--highlight-background :weight bold))))
   `(undo-tree-visualizer-active-branch-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(undo-tree-visualizer-register-face ((t (:foreground ,construction-paper-theme--link))))
   `(undo-tree-visualizer-unmodified-face ((t (:foreground ,construction-paper-theme--link))))

   ;; auctex
   `(font-latex-bold-face ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(font-latex-doctex-documentation-face ((t (:background ,construction-paper-theme--text))))
   `(font-latex-italic-face ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(font-latex-math-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(font-latex-sectioning-0-face ((t (:foreground ,construction-paper-theme--link))))
   `(font-latex-sectioning-1-face ((t (:foreground ,construction-paper-theme--link))))
   `(font-latex-sectioning-2-face ((t (:foreground ,construction-paper-theme--link))))
   `(font-latex-sectioning-3-face ((t (:foreground ,construction-paper-theme--link))))
   `(font-latex-sectioning-4-face ((t (:foreground ,construction-paper-theme--link))))
   `(font-latex-sectioning-5-face ((t (:foreground ,construction-paper-theme--link))))
   `(font-latex-sedate-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(font-latex-string-face ((t (:foreground ,construction-paper-theme--link))))
   `(font-latex-verbatim-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(font-latex-warning-face ((t (:foreground ,construction-paper-theme--keyword))))

   ;; dired+
   `(diredp-compressed-file-suffix ((t (:foreground ,construction-paper-theme--keyword))))
   `(diredp-dir-heading ((t (:foreground nil :background nil :inherit heading))))
   `(diredp-dir-priv ((t (:foreground ,construction-paper-theme--keyword :background nil))))
   `(diredp-exec-priv ((t (:foreground ,construction-paper-theme--keyword :background nil))))
   `(diredp-executable-tag ((t (:foreground ,construction-paper-theme--keyword :background nil))))
   `(diredp-file-name ((t (:foreground ,construction-paper-theme--link))))
   `(diredp-file-suffix ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(diredp-flag-mark-line ((t (:background nil :inherit highlight-background))))
   `(diredp-ignored-file-name ((t (:foreground ,construction-paper-theme--doc))))
   `(diredp-link-priv ((t (:background nil :foreground ,construction-paper-theme--link))))
   `(diredp-mode-line-flagged ((t (:foreground ,construction-paper-theme--keyword))))
   `(diredp-mode-line-marked ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(diredp-no-priv ((t (:background nil))))
   `(diredp-number ((t (:foreground ,construction-paper-theme--link))))
   `(diredp-other-priv ((t (:background nil :foreground ,construction-paper-theme--link))))
   `(diredp-rare-priv ((t (:foreground ,construction-paper-theme--keyword :background nil))))
   `(diredp-read-priv ((t (:foreground ,construction-paper-theme--highlight-background :background nil))))
   `(diredp-symlink ((t (:foreground ,construction-paper-theme--link))))
   `(diredp-write-priv ((t (:foreground ,construction-paper-theme--link :background nil))))


    ;; vc
   `(vc-dir-directory ((t (:inherit variable-pitch :foreground ,construction-paper-theme--shadow-bright :height 1.25))))
   `(vc-dir-status-edited ((t (:foreground ,construction-paper-theme--moss))))
   `(vc-dir-file ((t (:foreground ,construction-paper-theme--text :weight bold))))

   ;; magit
   `(magit-bisect-bad ((t (:foreground ,construction-paper-theme--alert))))
   `(magit-bisect-good ((t (:foreground ,construction-paper-theme--keyword))))
   `(magit-bisect-skip ((t (:foreground ,construction-paper-theme--text))))
   `(magit-branch-local ((t (:foreground ,construction-paper-theme--link))))
   `(magit-branch-remote ((t (:foreground ,construction-paper-theme--keyword))))
   `(magit-cherry-equivalent ((t (:foreground ,construction-paper-theme--alert))))
   `(magit-cherry-unmatched ((t (:foreground ,construction-paper-theme--link))))
   `(magit-diff-added ((t (:background ,construction-paper-theme--added :foreground ,construction-paper-theme--highlight-foreground))))
   `(magit-diff-added-highlight ((t (:background ,construction-paper-theme--added :foreground ,construction-paper-theme--highlight-foreground :weight bold))))
   `(magit-diff-base ((t (:background ,construction-paper-theme--block :foreground ,construction-paper-theme--text))))
   `(magit-diff-base-highlight ((t (:background ,construction-paper-theme--text :foreground ,construction-paper-theme--background :weight bold))))
   `(magit-diff-context ((t (:foreground ,construction-paper-theme--text))))
   `(magit-diff-context-highlight ((t (:background ,construction-paper-theme--block))))
   `(magit-diff-file-heading ((t (:weight normal))))
   `(magit-diff-file-heading-highlight ((t (:weight bold))))
   `(magit-diff-file-heading-highlight-background ((t (:weight bold))))
   `(magit-diff-hunk-heading ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--text))))
   `(magit-diff-hunk-heading-highlight ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--text :weight bold))))
   `(magit-diff-removed ((t (:background ,construction-paper-theme--removed :foreground ,construction-paper-theme--highlight-foreground))))
   `(magit-diff-removed-highlight ((t (:background ,construction-paper-theme--removed :foreground ,construction-paper-theme--highlight-foreground :weight bold))))
   `(magit-diff-whitespace-warning ((t (:background ,construction-paper-theme--removed :foreground ,construction-paper-theme--highlight-foreground))))
   `(magit-diffstat-added ((t (:foreground ,construction-paper-theme--keyword))))
   `(magit-diffstat-removed ((t (:foreground ,construction-paper-theme--alert))))
   `(magit-hash ((t (:foreground ,construction-paper-theme--keyword))))
   `(magit-header-line ((t (:foreground ,construction-paper-theme--moss :height 1.0))))
   `(magit-log-author ((t (:foreground ,construction-paper-theme--alert))))
   `(magit-process-ng ((t (:inherit magit-section-heading :foreground ,construction-paper-theme--alert))))
   `(magit-process-ok ((t (:inherit magit-section-heading :foreground ,construction-paper-theme--keyword))))
   `(magit-reflog-amend ((t (:foreground ,construction-paper-theme--alert))))
   `(magit-reflog-checkout ((t (:foreground ,construction-paper-theme--link))))
   `(magit-reflog-cherry-pick ((t (:foreground ,construction-paper-theme--keyword))))
   `(magit-reflog-commit ((t (:foreground ,construction-paper-theme--keyword))))
   `(magit-reflog-merge ((t (:foreground ,construction-paper-theme--keyword))))
   `(magit-reflog-other ((t (:foreground ,construction-paper-theme--link))))
   `(magit-reflog-rebase ((t (:foreground ,construction-paper-theme--alert))))
   `(magit-reflog-remote ((t (:foreground ,construction-paper-theme--link))))
   `(magit-reflog-reset ((t (:foreground ,construction-paper-theme--alert))))
   `(magit-section-heading ((t (:inherit variable-pitch :foreground ,construction-paper-theme--text))))
   `(magit-section-heading-selection ((t (:inherit variable-pitch :foreground ,construction-paper-theme--text))))
   `(magit-section-highlight ((t (:inherit nil :weight normal))))
   `(magit-sequence-drop ((t (:foreground ,construction-paper-theme--alert))))
   `(magit-sequence-head ((t (:foreground ,construction-paper-theme--link))))
   `(magit-sequence-part ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(magit-sequence-stop ((t (:foreground ,construction-paper-theme--keyword))))
   `(magit-signature-bad ((t (:foreground ,construction-paper-theme--alert :weight normal))))
   `(magit-signature-error ((t (:foreground ,construction-paper-theme--alert))))
   `(magit-signature-expired ((t (:foreground ,construction-paper-theme--alert))))
   `(magit-signature-good ((t (:foreground ,construction-paper-theme--keyword))))
   `(magit-signature-revoked ((t (:foreground ,construction-paper-theme--alert))))
   `(magit-signature-untrusted ((t (:foreground ,construction-paper-theme--link))))
   `(magit-tag ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(magithub-deleted-thing ((t (:inherit magit-section-highlight-background :background ,construction-paper-theme--alert))))

   ;; term and ansi-term
   `(term-color-black ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--text))))
   `(term-color-white ((t (:foreground ,construction-paper-theme--added :background ,construction-paper-theme--background))))
   `(term-color-red ((t (:foreground ,construction-paper-theme--keyword :background ,construction-paper-theme--keyword))))
   `(term-color-yellow ((t (:foreground ,construction-paper-theme--link :background ,construction-paper-theme--link))))
   `(term-color-green ((t (:foreground ,construction-paper-theme--highlight-background :background ,construction-paper-theme--highlight-background))))
   `(term-color-cyan ((t (:foreground ,construction-paper-theme--keyword :background ,construction-paper-theme--keyword))))
   `(term-color-blue ((t (:foreground ,construction-paper-theme--keyword :background ,construction-paper-theme--keyword))))
   `(term-color-magenta ((t (:foreground ,construction-paper-theme--link :background ,construction-paper-theme--link))))

   `(widget-button ((t (:underline t))))
   `(widget-field ((t (:background ,construction-paper-theme--background :box (:line-width 1 :color ,construction-paper-theme--text)))))
   `(widget-button-pressed ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--fire))))
   `(widget-documentation ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--moss))))

   ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
   `(compilation-column-number ((t (:foreground ,construction-paper-theme--link))))
   `(compilation-line-number ((t (:foreground ,construction-paper-theme--link))))
   `(compilation-message-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(compilation-mode-line-exit ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(compilation-mode-line-fail ((t (:foreground ,construction-paper-theme--keyword))))
   `(compilation-mode-line-run ((t (:foreground ,construction-paper-theme--keyword))))

   ;; tooltip
   `(tool-tip ((t (:background ,construction-paper-theme--inverted-background-medium :foreground ,construction-paper-theme--inverted-text))))
   `(tooltip ((t (:background ,construction-paper-theme--inverted-background-medium :foreground ,construction-paper-theme--inverted-text))))

   ;; Grep
   `(grep-context-face ((t (:foreground ,construction-paper-theme--doc))))
   `(grep-error-face ((t (:foreground ,construction-paper-theme--keyword :weight bold :underline t))))
   `(grep-hit-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(grep-match-face ((t (:foreground nil :background nil :inherit match))))

   `(regex-tool-matched-face ((t (:foreground nil :background nil :inherit match))))

   ;; mark-multiple
   `(mm/master-face ((t (:inherit region :foreground nil :background nil))))
   `(mm/mirror-face ((t (:inherit region :foreground nil :background nil))))

   ;; info
   `(info-menu-header ((t (:inherit variable-pitch :foreground ,construction-paper-theme--nav))))
   `(info-menu-star ((t (:foreground ,construction-paper-theme--keyword))))

   ;; org-mode
   `(org-agenda-structure ((t (:foreground ,construction-paper-theme--link))))
   `(org-agenda-date ((t (:foreground ,construction-paper-theme--keyword :underline nil))))
   `(org-agenda-done ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(org-agenda-dimmed-todo-face ((t (:foreground ,construction-paper-theme--doc))))
   `(org-block ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--block :box nil))))
   `(org-code ((t (:foreground ,construction-paper-theme--link))))
   `(org-column ((t (:background ,construction-paper-theme--text))))
   `(org-column-title ((t (:inherit org-column :weight bold :underline t))))
   `(org-date ((t (:foreground ,construction-paper-theme--link :underline t))))
   `(org-document-info ((t (:foreground ,construction-paper-theme--keyword))))
   `(org-document-info-keyword ((t (:foreground ,construction-paper-theme--keyword))))
   `(org-document-title ((t (:foreground ,construction-paper-theme--text :weight bold))))
   `(org-done ((t (:inherit default :foreground ,construction-paper-theme--doc :strike-through t))))
   `(org-headline-done ((t (:foreground ,construction-paper-theme--doc :strike-through t))))
   `(org-ellipsis ((t (:foreground ,construction-paper-theme--doc))))
   `(org-footnote ((t (:foreground ,construction-paper-theme--keyword))))
   `(org-formula ((t (:foreground ,construction-paper-theme--keyword))))
   `(org-hide ((t (:foreground ,construction-paper-theme--text))))
   `(org-link ((t (:inherit link))))
   `(org-meta-line ((t (:foreground ,construction-paper-theme--keyword))))
   `(org-block-end-line ((t (:inherit org-meta-line :weight bold))))
   `(org-block-begin-line ((t (:inherit org-meta-line :weight bold))))
   `(org-scheduled ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(org-scheduled-previously ((t (:foreground ,construction-paper-theme--keyword))))
   `(org-scheduled-today ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(org-special-keyword ((t (:foreground ,construction-paper-theme--keyword))))
   `(org-property-value ((t (:foreground ,construction-paper-theme--keyword))))
   `(org-table ((t (:foreground ,construction-paper-theme--link))))
   `(org-tag ((t (:foreground ,construction-paper-theme--doc))))
   `(org-todo ((t (:inherit default :foreground ,construction-paper-theme--alert))))
   `(org-upcoming-deadline ((t (:foreground ,construction-paper-theme--keyword))))
   `(org-warning ((t (:weight bold :foreground ,construction-paper-theme--keyword))))
   `(org-mode-line-clock-overrun ((t (:weight bold :foreground ,construction-paper-theme--inverted-text :background ,construction-paper-theme--alert))))

   ;; asciidoc
   `(markup-gen-face ((t (:foreground ,construction-paper-theme--text))))
   `(markup-title-0-face ((t (:inherit variable-pitch :foreground ,construction-paper-theme--text :height 3.0))))
   `(markup-title-1-face ((t (:inherit variable-pitch :foreground ,construction-paper-theme--text :height 2.4))))
   `(markup-title-2-face ((t (:inherit variable-pitch :foreground ,construction-paper-theme--text :height 1.8))))
   `(markup-title-3-face ((t (:inherit variable-pitch :foreground ,construction-paper-theme--text :height 1.4 :weight bold))))
   `(markup-title-4-face ((t (:inherit variable-pitch :foreground ,construction-paper-theme--text :height 1.2 :slant italic))))

   `(markdown-url-face ((t (:inherit link))))
   `(markdown-header-face ((t (:inherit variable-pitch))))
   `(markdown-link-face ((t (:foreground ,construction-paper-theme--link :underline t))))

   `(hl-sexp-face ((t (:background ,construction-paper-theme--text))))
   `(hl-line ((t (:background ,construction-paper-theme--background-medium))))
   `(highlight-background-80+ ((t (:background ,construction-paper-theme--text))))
   `(highlight ((t (:background ,construction-paper-theme--added))))

   ;; gotest
   `(go-test--ok-face ((t (:foreground ,construction-paper-theme--moss))))
   `(go-test--error-face ((t (:foreground ,construction-paper-theme--fire))))
   `(go-test--warning-face ((t (:foreground ,construction-paper-theme--sun))))
   `(go-test--pointer-face ((t (:foreground ,construction-paper-theme--water))))
   `(go-test--standard-face ((t (:foreground ,construction-paper-theme--text))))

   ;; Python-specific overrides
   `(py-builtins-face ((t (:foreground ,construction-paper-theme--keyword :weight normal))))
   ;; Cider
   `(cider-deprecated-face ((t (:background ,construction-paper-theme--alert))))
   `(cider-enlightened-face ((t (:inherit cider-result-overlay-face :box (:line-width -1 :color ,construction-paper-theme--highlight-background)))))
   `(cider-enlightened-local-face ((t (:foreground ,construction-paper-theme--highlight-background :weight bold))))
   `(cider-error-highlight-background-face ((t (:inherit nil :underline (:color ,construction-paper-theme--alert :style wave)))))
   `(cider-fringe-good-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(cider-instrumented-face ((t (:box (:line-width -1 :color ,construction-paper-theme--alert)))))
   `(cider-result-overlay-face ((t (:background ,construction-paper-theme--keyword :foreground ,construction-paper-theme--background :box (:line-width -2 :color ,construction-paper-theme--keyword) :slant italic :weight bold))))
   `(cider-test-error-face ((t (:weight bold :inverse-video t :foreground ,construction-paper-theme--alert))))
   `(cider-test-failure-face ((t (:weight bold :inverse-video t :foreground ,construction-paper-theme--alert))))
   `(cider-test-success-face ((t (:weight bold :foreground ,construction-paper-theme--good))))
   `(cider-traced-face ((t (:box (:line-width -1 :color ,construction-paper-theme--link)))))
   `(cider-warning-highlight-background-face ((t (:inherit nil :underline (:color ,construction-paper-theme--highlight-background :style wave)))))

   ;; js2-mode
   `(js2-warning ((t (:underline (:color ,construction-paper-theme--sun :style wave)))))
   `(js2-error ((t (:foreground nil :underline (:color ,construction-paper-theme--sun :style wave)))))
   `(js2-external-variable ((t (:foreground ,construction-paper-theme--link))))
   `(js2-function-param ((t (:foreground ,construction-paper-theme--keyword))))
   `(js2-instance-member ((t (:foreground ,construction-paper-theme--keyword))))
   `(js2-object-property ((t (:foreground ,construction-paper-theme--keyword))))
   `(js2-private-function-call ((t (:foreground ,construction-paper-theme--keyword))))

   ;; js3-mode
   `(js3-warning-face ((t (:underline (:color ,construction-paper-theme--sun :style wave)))))
   `(js3-error-face ((t (:foreground nil :underline (:color ,construction-paper-theme--alert :style wave)))))
   `(js3-external-variable-face ((t (:foreground ,construction-paper-theme--link))))
   `(js3-function-param-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(js3-jsdoc-tag-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(js3-jsdoc-type-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(js3-jsdoc-value-face ((t (:foreground ,construction-paper-theme--link))))
   `(js3-jsdoc-html-tag-name-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(js3-jsdoc-html-tag-delimiter-face ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(js3-instance-member-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(js3-private-function-call-face ((t (:foreground ,construction-paper-theme--keyword))))

   ;; nxml
   `(nxml-name-face ((t (:foreground unspecified :inherit font-lock-constant-face))))
   `(nxml-attribute-local-name-face ((t (:foreground unspecified :inherit font-lock-variable-name-face))))
   `(nxml-ref-face ((t (:foreground unspecified :inherit font-lock-preprocessor-face))))
   `(nxml-delimiter-face ((t (:foreground unspecified :inherit font-lock-keyword-face))))
   `(nxml-delimited-data-face ((t (:foreground unspecified :inherit font-lock-string-face))))
   `(rng-error-face ((t (:underline ,construction-paper-theme--keyword))))

   ;; RHTML
   `(erb-delim-face ((t (:background ,construction-paper-theme--text))))
   `(erb-exec-face ((t (:background ,construction-paper-theme--text :weight bold))))
   `(erb-exec-delim-face ((t (:background ,construction-paper-theme--text))))
   `(erb-out-face ((t (:background ,construction-paper-theme--text :weight bold))))
   `(erb-out-delim-face ((t (:background ,construction-paper-theme--text))))
   `(erb-comment-face ((t (:background ,construction-paper-theme--text :weight bold :slant italic))))
   `(erb-comment-delim-face ((t (:background ,construction-paper-theme--text))))

   ;; Message-mode
   `(message-header-other ((t (:foreground nil :background nil :weight normal))))
   `(message-header-subject ((t (:inherit message-header-other :weight bold :foreground ,construction-paper-theme--link))))
   `(message-header-to ((t (:inherit message-header-other :weight bold :foreground ,construction-paper-theme--keyword))))
   `(message-header-cc ((t (:inherit message-header-to :foreground nil))))
   `(message-header-name ((t (:foreground ,construction-paper-theme--keyword :background nil))))
   `(message-header-newsgroups ((t (:foreground ,construction-paper-theme--keyword :background nil :slant normal))))
   `(message-separator ((t (:foreground ,construction-paper-theme--link))))

   ;; Jabber
   `(jabber-chat-prompt-local ((t (:foreground ,construction-paper-theme--link))))
   `(jabber-chat-prompt-foreign ((t (:foreground ,construction-paper-theme--keyword))))
   `(jabber-chat-prompt-system ((t (:foreground ,construction-paper-theme--link :weight bold))))
   `(jabber-chat-text-local ((t (:foreground ,construction-paper-theme--link))))
   `(jabber-chat-text-foreign ((t (:foreground ,construction-paper-theme--keyword))))
   `(jabber-chat-text-error ((t (:foreground ,construction-paper-theme--keyword))))

   `(jabber-roster-user-online ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(jabber-roster-user-xa ((t :foreground ,construction-paper-theme--doc)))
   `(jabber-roster-user-dnd ((t :foreground ,construction-paper-theme--link)))
   `(jabber-roster-user-away ((t (:foreground ,construction-paper-theme--keyword))))
   `(jabber-roster-user-chatty ((t (:foreground ,construction-paper-theme--link))))
   `(jabber-roster-user-error ((t (:foreground ,construction-paper-theme--keyword))))
   `(jabber-roster-user-offline ((t (:foreground ,construction-paper-theme--doc))))

   `(jabber-rare-time-face ((t (:foreground ,construction-paper-theme--doc))))
   `(jabber-activity-face ((t (:foreground ,construction-paper-theme--link))))
   `(jabber-activity-personal-face ((t (:foreground ,construction-paper-theme--keyword))))

   ;; Gnus
   `(gnus-cite-1 ((t (:inherit outline-1 :foreground nil))))
   `(gnus-cite-2 ((t (:inherit outline-2 :foreground nil))))
   `(gnus-cite-3 ((t (:inherit outline-3 :foreground nil))))
   `(gnus-cite-4 ((t (:inherit outline-4 :foreground nil))))
   `(gnus-cite-5 ((t (:inherit outline-5 :foreground nil))))
   `(gnus-cite-6 ((t (:inherit outline-6 :foreground nil))))
   `(gnus-cite-7 ((t (:inherit outline-7 :foreground nil))))
   `(gnus-cite-8 ((t (:inherit outline-8 :foreground nil))))
   ;; there are several more -cite- faces...
   `(gnus-header-content ((t (:inherit message-header-other))))
   `(gnus-header-subject ((t (:inherit message-header-subject))))
   `(gnus-header-from ((t (:inherit message-header-other-face :weight bold :foreground ,construction-paper-theme--keyword))))
   `(gnus-header-name ((t (:inherit message-header-name))))
   `(gnus-button ((t (:inherit link :foreground nil))))
   `(gnus-signature ((t (:inherit font-lock-comment-face))))

   `(gnus-summary-normal-unread ((t (:foreground ,construction-paper-theme--keyword :weight normal))))
   `(gnus-summary-normal-read ((t (:foreground ,construction-paper-theme--added :weight normal))))
   `(gnus-summary-normal-ancient ((t (:foreground ,construction-paper-theme--keyword :weight normal))))
   `(gnus-summary-normal-ticked ((t (:foreground ,construction-paper-theme--keyword :weight normal))))
   `(gnus-summary-low-unread ((t (:foreground ,construction-paper-theme--doc :weight normal))))
   `(gnus-summary-low-read ((t (:foreground ,construction-paper-theme--doc :weight normal))))
   `(gnus-summary-low-ancient ((t (:foreground ,construction-paper-theme--doc :weight normal))))
   `(gnus-summary-high-unread ((t (:foreground ,construction-paper-theme--link :weight normal))))
   `(gnus-summary-high-read ((t (:foreground ,construction-paper-theme--highlight-background :weight normal))))
   `(gnus-summary-high-ancient ((t (:foreground ,construction-paper-theme--highlight-background :weight normal))))
   `(gnus-summary-high-ticked ((t (:foreground ,construction-paper-theme--keyword :weight normal))))
   `(gnus-summary-cancelled ((t (:foreground ,construction-paper-theme--keyword :background nil :weight normal))))

   `(gnus-group-mail-low ((t (:foreground ,construction-paper-theme--doc))))
   `(gnus-group-mail-low-empty ((t (:foreground ,construction-paper-theme--doc))))
   `(gnus-group-mail-1 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-mail-2 ((t (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-mail-3 ((t (:foreground nil :weight normal :inherit outline-3))))
   `(gnus-group-mail-4 ((t (:foreground nil :weight normal :inherit outline-4))))
   `(gnus-group-mail-5 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-mail-6 ((t (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-mail-1-empty ((t (:inherit gnus-group-mail-1 :foreground ,construction-paper-theme--doc))))
   `(gnus-group-mail-2-empty ((t (:inherit gnus-group-mail-2 :foreground ,construction-paper-theme--doc))))
   `(gnus-group-mail-3-empty ((t (:inherit gnus-group-mail-3 :foreground ,construction-paper-theme--doc))))
   `(gnus-group-mail-4-empty ((t (:inherit gnus-group-mail-4 :foreground ,construction-paper-theme--doc))))
   `(gnus-group-mail-5-empty ((t (:inherit gnus-group-mail-5 :foreground ,construction-paper-theme--doc))))
   `(gnus-group-mail-6-empty ((t (:inherit gnus-group-mail-6 :foreground ,construction-paper-theme--doc))))
   `(gnus-group-news-1 ((t (:foreground nil :weight normal :inherit outline-5))))
   `(gnus-group-news-2 ((t (:foreground nil :weight normal :inherit outline-6))))
   `(gnus-group-news-3 ((t (:foreground nil :weight normal :inherit outline-7))))
   `(gnus-group-news-4 ((t (:foreground nil :weight normal :inherit outline-8))))
   `(gnus-group-news-5 ((t (:foreground nil :weight normal :inherit outline-1))))
   `(gnus-group-news-6 ((t (:foreground nil :weight normal :inherit outline-2))))
   `(gnus-group-news-1-empty ((t (:inherit gnus-group-news-1 :foreground ,construction-paper-theme--doc))))
   `(gnus-group-news-2-empty ((t (:inherit gnus-group-news-2 :foreground ,construction-paper-theme--doc))))
   `(gnus-group-news-3-empty ((t (:inherit gnus-group-news-3 :foreground ,construction-paper-theme--doc))))
   `(gnus-group-news-4-empty ((t (:inherit gnus-group-news-4 :foreground ,construction-paper-theme--doc))))
   `(gnus-group-news-5-empty ((t (:inherit gnus-group-news-5 :foreground ,construction-paper-theme--doc))))
   `(gnus-group-news-6-empty ((t (:inherit gnus-group-news-6 :foreground ,construction-paper-theme--doc))))

   `(erc-direct-msg-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(erc-error-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(erc-header-face ((t (:foreground ,construction-paper-theme--added :background ,construction-paper-theme--doc))))
   `(erc-input-face ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(erc-keyword-face ((t (:foreground ,construction-paper-theme--link))))
   `(erc-current-nick-face ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(erc-my-nick-face ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(erc-nick-default-face ((t (:weight normal :foreground ,construction-paper-theme--link))))
   `(erc-nick-msg-face ((t (:weight normal :foreground ,construction-paper-theme--link))))
   `(erc-notice-face ((t (:foreground ,construction-paper-theme--doc))))
   `(erc-pal-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(erc-prompt-face ((t (:foreground ,construction-paper-theme--keyword))))
   `(erc-timestamp-face ((t (:foreground ,construction-paper-theme--keyword))))

   ;; eshell
   `(eshell-ls-archive ((t (:foreground ,construction-paper-theme--deemphasize :weight normal))))
   `(eshell-ls-backup ((t (:foreground ,construction-paper-theme--deemphasize))))
   `(eshell-ls-clutter ((t (:foreground ,construction-paper-theme--alert :weight normal))))
   `(eshell-ls-directory ((t (:foreground ,construction-paper-theme--link :weight normal))))
   `(eshell-ls-executable ((t (:foreground ,construction-paper-theme--text :weight normal))))
   `(eshell-ls-missing ((t (:foreground ,construction-paper-theme--alert :weight normal))))
   `(eshell-ls-product ((t (:foreground ,construction-paper-theme--alert))))
   `(eshell-ls-readonly ((t (:foreground ,construction-paper-theme--alert))))
   `(eshell-ls-special ((t (:foreground ,construction-paper-theme--alert :weight normal))))
   `(eshell-ls-symlink ((t (:foreground ,construction-paper-theme--link :weight normal))))
   `(eshell-ls-unreadable ((t (:foreground ,construction-paper-theme--deemphasize))))
   `(eshell-prompt ((t (:foreground ,construction-paper-theme--keyword :weight normal))))

   ;; custom
   `(custom-variable-tag ((t (:foreground ,construction-paper-theme--keyword))))
   `(custom-group-tag ((t (:foreground ,construction-paper-theme--keyword))))
   `(custom-state ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(custom-button-pressed-unraised ((t (:foreground ,construction-paper-theme--keyword))))
   `(custom-changed ((t (:background ,construction-paper-theme--added :foreground ,construction-paper-theme--inverted-text))))
   `(custom-comment ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--doc))))
   `(custom-group-tag-1 ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--link))))
   `(custom-invalid ((t (:background ,construction-paper-theme--alert :foreground ,construction-paper-theme--inverted-text))))
   `(custom-modified ((t (:background ,construction-paper-theme--added :foreground ,construction-paper-theme--inverted-text))))
   `(custom-link ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--link))))
   `(custom-rogue ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--alert))))
   `(custom-set ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--keyword))))
   `(custom-themed ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--keyword))))

   ;; diary
   `(diary ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--changed))))

   ;; epa
   `(epa-field-body ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--good))))
   `(epa-field-name ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--good))))
   `(epa-mark ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--link))))
   `(epa-string ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--text))))
   `(epa-validity-disabled ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--alert))))
   `(epa-validity-high ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--good))))
   `(epa-validity-low ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--alert))))
   `(epa-validity-medium ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--changed))))

   ;; helm
   `(helm-M-x-key ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--text))))
   `(helm-action ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-bookmark-addressbook ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-bookmark-directory ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-bookmark-file ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-bookmark-file-not-found ((t (:foreground ,construction-paper-theme--alert :background ,construction-paper-theme--background))))
   `(helm-bookmark-gnus ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-bookmark-info ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-bookmark-man ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-bookmark-w3m ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-buffer-directory ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background :weight bold))))
   `(helm-buffer-file ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background :weight bold))))
   `(helm-buffer-not-saved ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-buffer-process ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-buffer-saved-out ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-buffer-size ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-buffer-archive ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-delete-async-message ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-candidate-number ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-candidate-number-suspended ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-ff-directory ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background :weight bold))))
   `(helm-ff-dotted-directory ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background :weight bold))))
   `(helm-ff-dotted-symlink-directory ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background :weight bold))))
   `(helm-ff-pipe ((t (:foreground ,construction-paper-theme--link :background ,construction-paper-theme--background :weight bold))))
   `(helm-ff-socket ((t (:foreground ,construction-paper-theme--link :background ,construction-paper-theme--background :weight bold))))
   `(helm-ff-suid ((t (:foreground ,construction-paper-theme--inverted-text :background ,construction-paper-theme--alert :weight bold))))
   `(helm-ff-denied ((t (:foreground ,construction-paper-theme--alert :background ,construction-paper-theme--background :weight bold))))
   `(helm-ff-executable ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-ff-file ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background :weight bold))))
   `(helm-ff-invalid-symlink ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-ff-prefix ((t (:foreground nil :background nil))))
   `(helm-ff-symlink ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-grep-cmd-line ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-grep-file ((t (:inverse t :foreground ,construction-paper-theme--text :background ,construction-paper-theme--background :weight bold))))
   `(helm-grep-finish ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-grep-lineno ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-grep-match ((t (:foreground ,construction-paper-theme--highlight-foreground :background ,construction-paper-theme--background :weight bold))))
   `(helm-header ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--text :height 0.75))))
   `(helm-header-line-left-margin ((t (:background ,construction-paper-theme--background :foreground ,construction-paper-theme--text :height 0.75))))
   `(helm-history-deleted ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-history-remote ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-lisp-completion-info ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-locate-finish ((t (:foreground ,construction-paper-theme--good :background ,construction-paper-theme--background))))
   `(helm-mode-prefix ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-non-file-buffer ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-prefarg ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-resume-need-update ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(helm-match ((t (:background ,construction-paper-theme--highlight-background :foreground ,construction-paper-theme--text :weight bold :weight bold))))
   `(helm-moccur-buffer ((t (:foreground ,construction-paper-theme--text, :background ,construction-paper-theme--background))))
   `(helm-selection ((t (:background ,construction-paper-theme--text :foreground ,construction-paper-theme--background :weight bold :height 1.0))))
   `(helm-selection-line ((t (:background ,construction-paper-theme--highlight-background :foreground ,construction-paper-theme--text :weight bold :height 1.0))))
   `(helm-separator ((t (:foreground ,construction-paper-theme--text))))
   `(helm-source-header ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background :weight bold :underline t))))
   `(helm-visible-mark ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))

   ;;  lsp
   `(lsp-ui-sideline-code-action ((t (:foreground ,construction-paper-theme--highlight-background))))
   `(lsp-ui-doc-header ((t (:foreground ,construction-paper-theme--good :background ,construction-paper-theme--background))))
   `(lsp-ui-peek-filename ((t (:foreground ,construction-paper-theme--alert))))
   `(lsp-ui-peek-header ((t (:foreground ,construction-paper-theme--keyword :background ,construction-paper-theme--background))))
   `(lsp-ui-peek-highlight ((t (:inverse-video t :foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(lsp-ui-peek-line-number ((t (:foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(lsp-ui-peek-list ((t (:inverse-video t :foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(lsp-ui-peek-peek ((t (:inverse-video t :foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(lsp-ui-peek-selection ((t (:inverse-video t :foreground ,construction-paper-theme--text :background ,construction-paper-theme--background))))
   `(lsp-ui-sideline-code-action ((t (:foreground ,construction-paper-theme--deemphasize))))
   `(lsp-ui-sideline-current-symbol ((t (:foreground ,construction-paper-theme--deemphasize :height 0.99 :weight black :box (:box (:line-width -1 :color ,construction-paper-theme--keyword))))))
   `(lsp-ui-sideline-symbol ((t (:foreground ,construction-paper-theme--deemphasize :height 0.99 :weight black :box (:box (:line-width -1 :color ,construction-paper-theme--keyword))))))
   `(lsp-ui-sideline-symbol-info ((t (:height 0.99 :slant italic)))))

  (custom-theme-set-variables
   theme-name
   `(ansi-color-names-vector
     ;; black, keyword, highlight, link, keyword, magenta, cyan, white
     [,construction-paper-theme--text ,construction-paper-theme--keyword ,construction-paper-theme--highlight-foreground ,construction-paper-theme--link ,construction-paper-theme--keyword ,construction-paper-theme--link ,construction-paper-theme--keyword ,construction-paper-theme--added])
   `(ansi-term-color-vector
     ;; black, keyword, highlight, link, keyword, magenta, cyan, white
     [,construction-paper-theme--text ,construction-paper-theme--keyword ,construction-paper-theme--highlight-foreground ,construction-paper-theme--link ,construction-paper-theme--keyword ,construction-paper-theme--link ,construction-paper-theme--keyword ,construction-paper-theme--added])))

(defun construction-paper-theme-light ()
  "Enable the construction paper light theme."
  (interactive)
  (require 'construction-paper-light-theme)
  (enable-theme 'construction-paper-light))

(provide 'construction-paper-theme)

;;; construction-paper-theme.el ends here
