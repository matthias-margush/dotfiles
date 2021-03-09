;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package imenu-list
  :general
  (:states 'normal :prefix leader "v" #'imenu-list-smart-toggle))

(defun me/project-name ()
  (cdr-safe (project-current)))

(defun me/project-root ()
  (file-name-as-directory
   (cdr-safe (project-current))))

(defun me/project-p ()
  (project-current))

(defun me/project-notes ()
  "Open a project notes file when opening a project."
  (interactive)
  (when (me/project-p)
    (let ((notes (expand-file-name "project.org" (me/project-root))))
      (if (and notes (file-exists-p notes))
          (find-file notes)
        (let ((notes (locate-file
                      "README"
                      `(,(me/project-root))
                      '(".org" ".md" ".markdown" ".txt" ".adoc" ""))))
          (if (and notes (file-exists-p notes))
            (find-file notes)))))))

(defun me/project-open-notes (project)
  "Open a project notes file when opening a project."
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
          (project-find-file project))))))

(defun me/project-switch-or-open (&optional project)
  "Switch project"
  (interactive (list (project-prompt-project-dir)))
  (let ((found nil)
        (project (or project (cdr-safe (project-current)))))
    (dolist (frame (frame-list))
      (when-let (frame-project (frame-parameter frame 'me/project))
        (when (and frame-project (not found) (string= project frame-project))
          (setq found t)
          (make-frame-visible frame)
          (raise-frame frame)
          (select-frame frame))))

    (unless found
      (let ((new-frame (make-frame)))
        (set-frame-parameter new-frame 'me/project project)
        (me/project-open-notes project)))))

(unbind-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC C-SPC") #'me/project-switch-or-open)

(general-define-key
 :states 'normal
 :prefix leader
 "p" project-prefix-map)

(general-define-key
 :keymaps 'project-prefix-map
 "p" #'me/project-switch-or-open
 "n" #'me/project-notes
 "t" #'me/sidebar)

(setq frame-title-format
      '(""
        (:eval
         (if-let ((project-name (me/project-name)))
             (format "%s" project-name)
           "%b"))))

(provide 'projects-config)
