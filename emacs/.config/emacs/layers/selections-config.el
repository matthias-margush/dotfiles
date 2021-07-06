;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(require 'theme-config)
(require 'accent-theme)

(use-package posframe)

(use-package mini-frame
  :init
  (set-face-attribute
   'child-frame-border nil
   :background accent-theme--background-dark)

  (setq mini-frame-resize t
        mini-frame-resize-max-height 1
        mini-frame-resize-min-height 20
        mini-frame-show-parameters
        `((top . 1)
          (left . 0.5)
          (width . 0.94)
          (height . 0.5)
          (background-color . ,accent-theme--background-dark)
          (internal-border-width . 30)
          ;; (alpha . (95 . 95))
          (child-frame-border-width . nil)
          (vertical-scroll-bars . nil)
          (font . ,me/fixed-pitch)))

  :config
  ;; (mini-frame-mode)
  )

(use-package selectrum
  :demand t
  :init
  ;; (setq selectrum-display-action
  ;;       '(display-buffer-in-side-window
  ;;         (side . bottom)
  ;;         (slot . -1)))

  (setq selectrum-count-style nil)
  (setq selectrum-num-candidates-displayed 100)
  (setq selectrum-fix-vertical-window-height 30)
  (setq selectrum-completing-read-multiple-show-help t)
  (setq selectrum-max-window-height nil)
  (setq selectrum-extend-current-candidate-highlight t)
  (setq magit-completing-read-function #'selectrum-completing-read)

  (defun me/adjust-selectrum-window-size (original-fun window &optional height &rest args)
    "Increase the size to account for the header line."
    (let ((dheight (+ 50 (or height (cdr
                                     (window-text-pixel-size
                                      window nil nil nil nil t))))))
      (apply original-fun window dheight args)))

  ;; (advice-add
  ;;  #'selectrum--set-window-height
  ;;  :around #'me/adjust-selectrum-window-size)

  :config
  (selectrum-mode))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode))

(defun me/messages ()
  (interactive)
  (split-window (selected-window) 20 'below)
  (switch-to-buffer "*Messages*"))

(use-package consult
  :general
  (:states 'normal :prefix leader "SPC" #'consult-buffer)
  (:states 'normal :prefix leader "/" #'consult-ripgrep)
  (:states '(normal insert) "s-:" #'execute-extended-command)
  (:states 'normal :prefix leader "m" #'consult-mode-command)
  (:states 'normal :prefix leader "j" #'consult-imenu)
  (:states 'normal :prefix leader ":" #'consult-goto-line)

  :bind
  ("s-F" . counsel-git-grep)

  :init
  (setq consult-find-command
        "fd --color=never --full-path ARG OPTS")

  (setq consult-ripgrep-command
        "rg --trim --null --line-buffered --color=ansi --max-columns=1000 --hidden --no-heading --line-number . -e ARG OPTS")

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (setq consult-narrow-key (kbd "TAB"))

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (define-key consult-narrow-map (vconcat consult-narrow-key (kbd "TAB")) #'consult-narrow-help)

  (setq consult-buffer-sources
        '(consult--source-hidden-buffer
          consult--source-bookmark
          consult--source-project-buffer
          consult--source-project-file
          consult--source-file
          consult--source-buffer))

  (dolist (src consult-buffer-sources)
    (if (or (eq src 'consult--source-project-buffer)
            (eq src 'consult--source-bookmark)
            (eq src 'consult--source-project-file)
            (eq src 'consult--source-file))
        (set src (plist-put (symbol-value src) :hidden nil))
      (set src (plist-put (symbol-value src) :hidden t))))

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (expand-file-name (car (project-roots project)))))))

(use-package marginalia
  :config
  (setq marginalia-annotator-registry
        '((command marginalia-annotate-command marginalia-annotate-binding builtin none)
          (embark-keybinding marginalia-annotate-embark-keybinding builtin none)
          (customize-group marginalia-annotate-customize-group builtin none)
          (variable marginalia-annotate-variable builtin none)
          (face marginalia-annotate-face builtin none)
          (color marginalia-annotate-color builtin none)
          (unicode-name marginalia-annotate-char builtin none)
          (minor-mode marginalia-annotate-minor-mode builtin none)
          (symbol marginalia-annotate-symbol builtin none)
          (environment-variable marginalia-annotate-environment-variable builtin none)
          (input-method marginalia-annotate-input-method builtin none)
          (coding-system marginalia-annotate-coding-system builtin none)
          (charset marginalia-annotate-charset builtin none)
          (package marginalia-annotate-package builtin none)
          (imenu marginalia-annotate-imenu builtin none)
          (bookmark marginalia-annotate-bookmark builtin none)
          (buffer marginalia-annotate-buffer builtin none)
          (consult-multi marginalia-annotate-consult-multi builtin none)))
  (marginalia-mode))

(use-package embark
  :bind
  ("C-S-a" . embark-act)
  (:map minibuffer-local-map
        ("C-c C-o" . embark-export)
        ("C-c C-a" . embark-act)))

(use-package wgrep)

(use-package embark-consult
  :after (embark consult)
  :demand t                ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-at-point-mode))

(provide 'selections-config)
