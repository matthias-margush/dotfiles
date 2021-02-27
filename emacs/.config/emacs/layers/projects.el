;;; init.el -*- lexical-binding: t; -*-

(use-package imenu-list
  :bind ("C-'" . #'imenu-list-smart-toggle))

(use-package counsel
  :demand t                             ; get all overrides
  :bind (("C-x b" . counsel-switch-buffer)
         ("s-:" . counsel-M-x))

  :general
  (:states 'normal
           ",cc" #'counsel-compile)
  :init
  (setq ivy-count-format ""
        ivy-use-virtual-buffers t
        ivy-read-action-format-function #'ivy-read-action-format-columns
        ivy-re-builders-alist '((t . ivy--regex-plus)))

  :config
  (counsel-mode)
  (defun counsel-more-chars ()))

(global-set-key (kbd "M-x") 'counsel-M-x)

(use-package counsel-projectile
  :bind ("C-x C-f" . counsel-find-file)

  :config
  (counsel-projectile-mode)
  (add-to-list 'counsel-projectile-switch-project-action
               '("so" me/project-switch-or-open "switch to or open") t))

(defun me/project-notes ()
  "Open a project notes file when opening projectile."
  (interactive)
  (when (projectile-project-p)
    (let ((notes (expand-file-name "project.org" (projectile-project-root))))
      (if (and notes (file-exists-p notes))
          (find-file notes)
        (let ((notes (locate-file
                      "README"
                      `(,(projectile-project-root))
                      '(".org" ".md" ".markdown" ".txt" ".adoc" ""))))
          (if (and notes (file-exists-p notes))
              (find-file notes)))))))

(defun me/project-open-notes (project)
  "Open a project notes file when opening projectile."
  (interactive)
  (let ((notes (expand-file-name "project.org" project)))
    (if (and notes (file-exists-p notes))
        (find-file notes)
      (let ((notes (locate-file
                    "README"
                    `(,(expand-file-name project))
                    '(".org" ".md" ".markdown" ".txt" ".adoc" ""))))
        (if (and notes (file-exists-p notes))
            (find-file notes)
          (counsel-projectile-switch-project-action-find-file project))))))


(defun me/project-switch-or-open (project)
  "Switch project"
  (interactive "P")
  (let ((found nil))
    (dolist (frame (frame-list))
      (when-let (frame-project (frame-parameter frame 'me/project))
        (when (and (not found) (string= project frame-project))
          (setq found t)
          (make-frame-visible frame)
          (raise-frame frame)
          (select-frame frame))))

    (unless found
      (let ((new-frame (make-frame)))
        (set-frame-parameter new-frame 'me/project project)
        (me/project-open-notes project)))))

(defun me/switch-project (project)
  "Switch project"
  (interactive "P")
  (counsel-projectile-switch-project "so"))

(unbind-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC C-SPC") #'me/switch-project)

(use-package projectile
  :demand t
  :general
  (:states '(normal) :prefix leader "p" 'projectile-command-map)
  (:states '(normal) :prefix leader "/" #'counsel-git-grep)
  (:states '(normal) :prefix leader "SPC" #'counsel-projectile-switch-to-buffer)
  (:states '(normal) "s-\\" #'projectile-run-eshell)

  :bind
  ("s-F" . counsel-git-grep)
  (:map projectile-command-map
        ("p" . me/switch-project)
        ("r" . projectile-recentf)
        ("f" . counsel-projectile-find-file)
        ("b" . counsel-projectile-switch-to-buffer)
        ("n" . me/project-notes)
        ("s r" . counsel-projectile-rg)
        ("s a" . counsel-projectile-ag)
        ("c" . counsel-compile)
        ("s s" . counsel-projectile-ag)
        ("s g" . counsel-git-grep)
        ("t" . neotree-toggle)
        ("x f" . vterm))
  :init
  (setq
   projectile-completion-system 'ivy
   projectile-current-project-on-switch 'move-to-end
   projectile-dynamic-mode-line nil
   projectile-switch-project-action #'counsel-projectile-find-file
   projectile-project-search-path '("~/.config/emacs/straight/repos" "~/code"))

  :config
  (projectile-mode))

(use-package yasnippet
  :bind ("s-y" . yas-insert-snippet)
  :init
  (setq yas-verbosity 0)
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package multi-line
  :general
  (:states 'normal "gs" #'multi-line))

(setq frame-title-format
      '(""
        (:eval
         (if (fboundp 'projectile-project-name)
             (let ((project-name (projectile-project-name)))
               (if (not (string= "" project-name))
                   (format project-name)
                 (format (frame-parameter nil 'me/projectile-project-name))))))))
