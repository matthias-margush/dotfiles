;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(mm/package 'selectrum)

(setq selectrum-display-action
      '(display-buffer-in-side-window
        (side . bottom)
        (slot . -1)))

(setq selectrum-count-style nil)
(setq selectrum-num-candidates-displayed 100)
(setq selectrum-fix-vertical-window-height nil)
(setq selectrum-completing-read-multiple-show-help t)
(setq selectrum-max-window-height nil)
(setq selectrum-extend-current-candidate-highlight t)
(setq magit-completing-read-function #'selectrum-completing-read)

(selectrum-mode)


(mm/package 'selectrum-prescient)
(selectrum-prescient-mode)
(prescient-persist-mode)

(mm/package 'consult)
(global-set-key (kbd "s-F") #'me/grep)
(global-set-key (kbd "s-o") #'consult-buffer)
(global-set-key (kbd "s-O") #'me/project-switch-or-open)
(global-set-key (kbd "s-j") #'consult-imenu)

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

(define-key consult-narrow-map (vconcat consult-narrow-key (kbd "TAB")) #'consult-narrow-help)

(setq me/consult-source-project-files
      `(:name     "Project Files"
                  :narrow   ?p
                  :category project
                  :face     consult-file
                  :history  file-name-history
                  :items    ,#'me/project-files
                  :enabled   ,(lambda () (and consult-project-root-function
                                              recentf-mode))
                  :action   ,#'consult--file-action))

(setq consult-buffer-sources
      '(consult--source-hidden-buffer
        consult--source-bookmark
        consult--source-project-buffer
        me/consult-source-project-files
        consult--source-buffer))

(dolist (src consult-buffer-sources)
  (if (or (eq src 'consult--source-project-buffer)
          (eq src 'consult--source-bookmark)
          (eq src 'me/consult-source-project-files))
      (set src (plist-put (symbol-value src) :hidden nil))
    (set src (plist-put (symbol-value src) :hidden t))))

(add-to-list 'consult-buffer-sources #'me/consult-source-project-files)


(defun me/project-files ()
  (let ((project (project-current)))
    (project-files project (list (project-root project))))   )

(setq consult-project-root-function
      (lambda ()
        (when-let (project (project-current))
          (expand-file-name (car (project-roots project))))))

(mm/package 'marginalia)
(marginalia-mode)

(mm/package 'embark)
(define-key minibuffer-local-map
  (kbd "C-c C-o") #'embark-export)

(mm/package 'embark-consult)
(add-hook 'embark-collect-mode-hook #'embark-consult-preview-minor-mode)

(provide 'configure-selections)
