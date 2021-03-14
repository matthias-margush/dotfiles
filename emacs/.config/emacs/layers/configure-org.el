;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(add-to-list 'load-path
  (file-name-as-directory
    (expand-file-name "orgraphy" user-emacs-directory)))

(require 'orgraphy)
(orgraphy--init)

(mm/package 'org)

(setq org-hide-leading-stars t)
(setq org-hide-emphasis-markers t)

(add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
(add-hook 'org-mode #'visual-line-mode)

(defun me/emphasize-bold () (interactive) (org-emphasize ?*))
(defun me/emphasize-italic () (interactive) (org-emphasize ?/))
(defun me/emphasize-underline () (interactive) (org-emphasize ?_))

(setq
 org-startup-folded nil
 org-startup-indented nil
 org-startup-with-inline-images t
 org-adapt-indentation nil
 org-agenda-files '("~/Notes")
 org-babel-clojure-backend 'cider
 org-babel-default-header-args:sh '((:prologue . "exec 2>&1") (:epilogue . ":"))
 org-confirm-babel-evaluate nil
 org-confirm-elisp-link-function nil
 org-ditaa-jar-path "/usr/local/Cellar/ditaa/0.11.0_1/libexec/ditaa-0.11.0-standalone.jar"
 org-ellipsis " ... "
 org-enable-github-support t
 org-fontify-done-headline t
 org-fontify-quote-and-verse-blocks t
 org-fontify-whole-heading-line nil
 org-goto-interface 'outline-path-completion
 org-outline-path-complete-in-steps nil
 org-plain-list-ordered-item-terminator t
 org-plantuml-jar-path "/usr/local/Cellar/plantuml/1.2020.2/libexec/plantuml.jar"
 org-pretty-entities t
 org-projectile-file "TODO.org"
 org-src-fontify-natively t
 org-src-window-setup 'current-window
 org-want-todo-bindings t
 plantuml-jar-args '("-charset" "UTF-8" "-config" "~/code/dot/plantuml.txt")
 plantuml-jar-path "/usr/local/Cellar/plantuml/1.2021.1/libexec/plantuml.jar")

(add-to-list 'org-file-apps '(t . emacs) t)

(setq org-capture-templates
      `(("c" "Code")

        ("cl" "snippet" entry
         (file+headline me/project-notes-file "Snippets")
         "%(ha/org-capture-code-snippet \"%F\")"
         :empty-lines 1 :immediate-finish t)

        ("cf" "snippet with notes" entry
         (file+headline me/project-notes-file "Snippets")
         "%(ha/org-capture-code-snippet \"%F\")\n\n%?"
         :empty-lines 1)))

(defun ha/org-capture-clip-snippet (f)
  "Given a file, F, this captures the currently selected text
     within an Org EXAMPLE block and a backlink to the file."
  (with-current-buffer (find-buffer-visiting f)
    (ha/org-capture-fileref-snippet f "EXAMPLE" "" nil)))

(defun ha/org-capture-code-snippet (f)
  "Given a file, F, this captures the currently selected text
     within an Org SRC block with a language based on the current mode
     and a backlink to the function and the file."
  (with-current-buffer (find-buffer-visiting f)
    (let ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
          (func-name (which-function)))
      (ha/org-capture-fileref-snippet f "SRC" org-src-mode func-name))))

(defun ha/org-capture-fileref-snippet (f type headers func-name)
  (let* ((code-snippet
          (buffer-substring-no-properties (mark) (- (point) 1)))
         (file-name (buffer-file-name))
         (file-base (file-name-nondirectory file-name))
         (line-number (line-number-at-pos (region-beginning)))
         (initial-txt (if (null func-name)
                          (format "* Code Snippet\n+From: [[file:%s::%s][%s]]:"
                                  file-name line-number file-base)
                        (format "* Code Snippet\n+From [[file:%s::%s][%s]]\n In ~%s~:"
                                file-name line-number file-base
                                func-name))))
    (format "%s

#+BEGIN_%s %s
%s
#+END_%s" initial-txt type headers code-snippet type)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (plantuml . t)
   (gnuplot . t)
   (shell . t)
   (clojure . t)))
(require 'ox-md)

;; deft
(mm/package 'deft)
(global-set-key (kbd "s-d") #'me/notes-switch-or-open)
(setq deft-directory "~/Notes")
(setq deft-auto-save-interval 0)
(setq deft-extensions '("org"))
(setq deft-default-extension "org")
(setq deft-use-filename-as-title nil)
(setq deft-use-filter-string-for-filename t)
(defun me/notes-switch-or-open (_)
  "Switch to notes"
  (interactive "P")
  (let ((found nil))
    (dolist (frame (frame-list))
      (when-let (frame-project (frame-parameter frame 'me/project))
        (when (and (not found) (string= "me/notes" frame-project))
          (setq found t)
          (make-frame-visible frame)
          (raise-frame frame)
          (select-frame frame))))

    (unless found
      (let ((new-frame (make-frame)))
        (set-frame-parameter new-frame 'me/project "me/notes")))
    (deft)))

(mm/package 'org-superstar)
(add-hook 'org-mode-hook #'org-superstar-mode)

(provide 'configure-org)
