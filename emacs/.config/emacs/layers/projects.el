(use-package imenu-list
  :bind ("C-'" . #'imenu-list-smart-toggle))

(use-package counsel
  :bind (("C-x b" . counsel-switch-buffer)
           ("s-:" . counsel-M-x))

  :general
  (:states 'normal
           ",cc" #'counsel-compile)
  :init
  (setq ivy-count-format ""
        ivy-use-virtual-buffers t
        ivy-read-action-format-function #'ivy-read-action-format-columns
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)))

  :config
  (counsel-mode)
  (defun counsel-more-chars ()))

(global-set-key (kbd "M-x") 'counsel-M-x)

(use-package counsel-projectile
  :bind ("C-x C-f" . counsel-find-file)

  
  :config
  (counsel-projectile-mode)
  (add-to-list 'counsel-projectile-switch-project-action
               '("n" me/project-notes "open project notes") t))

(defun me/project-notes ()
  "Open a project notes file when opening projectile."
  (interactive)
  (when (projectile-project-p)
    (set-frame-parameter nil 'me/projectile-project-name projectile-project-name)
    (let ((notes (expand-file-name "project.org" (projectile-project-root))))
      (if (and notes (file-exists-p notes))
          (find-file notes)
          (let ((notes (locate-file
                        "README"
                        `(,(projectile-project-root))
                        '(".org" ".md" ".markdown" ".txt" ".adoc" ""))))
            (if (and notes (file-exists-p notes))
                (find-file notes)
                (projectile-find-file)))))))

(defun me/switch-project (args)
  "Switch project"
  (interactive "P")
  (counsel-projectile-switch-project))

(use-package projectile
  :general
  (:states '(normal) :prefix leader "p" 'projectile-command-map)
  (:states '(normal) :prefix leader "/" #'counsel-git-grep)
  (:states '(normal) :prefix leader "SPC" #'counsel-projectile-switch-to-buffer)

  :bind
  ("s-F" . counsel-git-grep)
  (:map projectile-command-map
        ("p" . me/switch-project)
        ("f" . counsel-projectile-find-file)
        ("b" . counsel-projectile-switch-to-buffer)
        ("n" . me/project-notes)
        ("s r" . counsel-projectile-rg)
        ("s a" . counsel-projectile-ag)
        ("s s" . counsel-projectile-ag)
        ("s g" . counsel-git-grep)
        ("t" . me/neotree-toggle)
        ("x f" . vterm))
  :init
  (setq
   projectile-current-project-on-switch 'move-to-end
   projectile-dynamic-mode-line nil
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

(setq frame-title-format
    '(""
      "%b"
      (:eval
       (if (fboundp 'projectile-project-name)
           (let ((project-name (projectile-project-name)))
             (if (not (string= "" project-name))
                 (format " in [%s]" project-name)
               (format " in [%s]" (frame-parameter nil 'me/projectile-project-name))))))))
