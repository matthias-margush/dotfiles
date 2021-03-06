;;; init.el -*- lexical-binding: t; -*-

(use-package imenu-list
  :general
  (:states 'normal :prefix leader "v" #'imenu-list-smart-toggle))

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

(defun me/project-notes-file ()
  ""
  (let ((project-notes (concat
                        (file-name-as-directory
                         (cdr-safe (project-current)))
                        "project.org")))
    (if (file-exists-p project-notes)
        project-notes
      "~/Notes/projects.org")))

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
          (projectile-find-file project))))))


(defun me/project-switch-or-open (&optional project)
  "Switch project"
  (interactive "P")
  (let ((found nil)
        (project (or project (cdr-safe (project-current)))))
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

(unbind-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC C-SPC") #'projectile-switch-project)

(use-package projectile
  :demand t
  :general
  (:states '(normal) :prefix leader "p" 'projectile-command-map)
  (:states '(normal) "s-\\" #'projectile-run-eshell)

  :bind
  (:map projectile-command-map
        ("n" . me/project-notes)
        ("t" . me/sidebar))

  :init
  (setq
   projectile-current-project-on-switch 'move-to-end
   projectile-dynamic-mode-line nil
   projectile-switch-project-action #'me/project-switch-or-open
   projectile-project-search-path '("~/code"))

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
  :demand t                             ; sets up hooks in various modes
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
