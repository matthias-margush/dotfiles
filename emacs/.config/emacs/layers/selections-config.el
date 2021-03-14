;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package posframe)

(use-package selectrum
  :demand t

  :init
  (setq selectrum-display-action '(display-buffer-show-in-posframe))

  (defun me/posframe-poshandler-frame-bottom-center (info)
    "Posframe's position handler.

 Get a position which let posframe stay onto its parent-frame's
 bottom center.  The structure of INFO can be found in docstring of
 `posframe-show'."
    (cons (/ (- (plist-get info :parent-frame-width)
                (plist-get info :posframe-width))
             2)
          (- (plist-get info :parent-frame-height)
             (plist-get info :posframe-height)
             (plist-get info :header-line-height))))

  (defun display-buffer-show-in-posframe (buffer _alist)
    (frame-root-window
     (posframe-show
      buffer
      :min-height 15
      :min-width (frame-width)
      :border-color "#dddddd"
      :fit-frame-to-buffer t
      :border-width 2
      :left-fringe 8
      :respect-header-line t
      :lines-truncate t
      :right-fringe 8
      :poshandler #'me/posframe-poshandler-frame-bottom-center)))

  (add-hook 'minibuffer-exit-hook 'posframe-delete-all)

  (setq selectrum-count-style nil)
  (setq selectrum-num-candidates-displayed 100)
  (setq selectrum-fix-vertical-window-height t)
  (setq selectrum-completing-read-multiple-show-help t)
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
  (:states 'normal :prefix leader "/" #'me/grep)
  (:states 'normal "s-:" #'execute-extended-command)
  (:states 'normal :prefix leader "m" #'consult-mode-command)
  (:states 'normal :prefix leader "j" #'consult-imenu)
  (:states 'normal :prefix leader ":" #'consult-goto-line)

  :bind
  ("s-F" . counsel-git-grep)

  :init
  (defun me/grep ()
    "Git or rip grep"
    (interactive)
    (if (or (vc-root-dir)
            (and (me/project-name)
                 (file-exists-p (concat (me/project-name) ".git"))))
        (consult-git-grep)
      (consult-ripgrep)))

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

  (dolist (src consult-buffer-sources)
    (if (or (eq src 'consult--source-project-buffer)
            (eq src 'consult--source-bookmark)
            (eq src 'consult--source-project-file))
        (set src (plist-put (symbol-value src) :hidden nil))
      (set src (plist-put (symbol-value src) :hidden t))))

  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (expand-file-name (car (project-roots project)))))))

(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :bind
  ("C-S-a" . embark-act)
  (:map minibuffer-local-map
        ("C-c C-o" . embark-export)))

(use-package embark-consult
  :after (embark consult)
  :demand t              ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

(provide 'selections-config)
