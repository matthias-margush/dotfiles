;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(use-package selectrum
  :init
  (setq selectrum-count-style nil)
  (setq selectrum-display-action
        '(display-buffer-in-side-window
          (side . bottom)
          (slot . -1)))
  (setq selectrum-max-window-height 15)
  (setq selectrum-extend-current-candidate-highlight t)
  (setq magit-completing-read-function #'selectrum-completing-read)

  :config
  (selectrum-mode))

(use-package selectrum-prescient
  :config
  (selectrum-prescient-mode)
  (prescient-persist-mode))

(use-package consult
  :general
  (:states 'normal :prefix leader "SPC" #'consult-buffer)
  (:states 'normal :prefix leader "/" #'consult-git-grep)
  (:states 'normal "s-:" #'execute-extended-command)
  (:states 'normal :prefix leader "m" #'consult-mode-command)
  (:states 'normal :prefix leader "j" #'consult-imenu)
  (:states 'normal :prefix leader ":" #'consult-goto-line)

  :bind
  ("s-F" . counsel-git-grep)

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  (setq consult-narrow-key "<")

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :config
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :bind
  ("C-S-a" . embark-act)
  (:map minibuffer-local-map
        ("C-c C-o" . embark-export)
        ("<tab>" . embark-act)))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))
