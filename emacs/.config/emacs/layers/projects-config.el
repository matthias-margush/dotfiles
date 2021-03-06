;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package side-hustle
  :bind ("C-'" . side-hustle-toggle)

  :init
  (setq side-hustle-display-alist
        '((side . right)
          (slot . 0)
          (window-width . 30))))

(defun me/project-name ()
  (cdr-safe (project-current)))

(defun me/project-root ()
  (when-let ((current-project (cdr-safe (project-current))))
    (file-name-as-directory
     current-project)))

(defun me/project-p ()
  (project-current))

;; Declare directories with ".project" as a project
(cl-defmethod project-root ((project (head local)))
  (or (cdr project) default-directory))

(defun me/project-try-local (dir)
  "Determine if DIR is a non-Git project.
DIR must include a .project file to be considered a project."
  (let ((root (locate-dominating-file dir ".project")))
    (cons 'local root)))

(require 'project)
(setq project-find-functions '(project-try-vc me/project-try-local))

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
          (find-file project))))))

(defun me/project-switch-or-open (&optional project)
  "Switch project"
  (interactive (list (project-prompt-project-dir)))
  (let ((project (or project (cdr-safe (project-current)))))
    (me/project-open-notes project))
  ;; (let ((found nil)
  ;;       (project (or project (cdr-safe (project-current)))))
  ;;   (me/project-open-notes project)
  ;;   (dolist (frame (frame-list))
  ;;     (when-let (frame-project (frame-parameter frame 'me/project))
  ;;       (when (and frame-project (not found) (string= project frame-project))
  ;;         (setq found t)
  ;;         (make-frame-visible frame)
  ;;         (raise-frame frame)
  ;;         (select-frame frame))))

  ;;   (unless found
  ;;     (let ((new-frame (make-frame)))
  ;;       (set-frame-parameter new-frame 'me/project project)
  ;;       (me/project-open-notes project))))
  )

(unbind-key (kbd "C-SPC"))
(global-set-key (kbd "C-SPC C-SPC") #'me/project-switch-or-open)

(general-define-key
 :states 'normal
 :prefix leader
 "p" project-prefix-map)

(global-set-key (kbd "s-p") project-prefix-map)
(define-key project-prefix-map (kbd "n") #'me/project-notes)

(setq me/shell-map (make-sparse-keymap))
(define-key me/shell-map "v" #'me/project-vterm)
(define-key me/shell-map "e" #'project-eshell)
(define-key me/shell-map "s" #'project-shell)
(define-key me/shell-map "t" #'me/project-term)
(global-set-key (kbd "s-\\") #'me/project-vterm)

(setq explicit-shell-file-name "/bin/zsh")

(defun me/project-vterm (&optional arg)
  (interactive "P")
  (require 'vterm)
  (let* ((default-directory (project-root (project-current t)))
         (vterm-buffer-name
          (concat "*" (file-name-nondirectory
                       (directory-file-name
                        (file-name-directory default-directory)))
                  "-vterm*")))
    (vterm--internal #'pop-to-buffer arg)))

(defun me/project-term (program)
  (interactive (list (read-from-minibuffer "Run program: "
                                           (or explicit-shell-file-name
                                               (getenv "ESHELL")
                                               shell-file-name))))
  (let* ((default-directory (project-root (project-current t)))
         (default-project-term-name
           (concat "*" (file-name-nondirectory
                        (directory-file-name
                         (file-name-directory default-directory)))
                   "-term*"))
         (term-buffer (make-term default-project-term-name program)))
    (if (and term-buffer (not current-prefix-arg))
        (pop-to-buffer term-buffer)
      (set-buffer term-buffer)
      (term-mode)
      (term-char-mode)
      (switch-to-buffer term-buffer))))

(general-define-key
 :states 'normal
 :prefix leader
 "x" me/shell-map
 "n" #'me/project-notes)

(general-define-key
 :keymaps 'project-prefix-map
 "p" #'me/project-switch-or-open
 "t" #'me/neotree-toggle
 "x" me/shell-map)

(setq frame-title-format
      '(""
        (:eval
         (if-let ((project-name (me/project-name)))
             (format "%s" project-name)
           "%b"))))

(provide 'projects-config)
