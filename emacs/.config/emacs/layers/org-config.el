;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)
(require 'evil-config)

(use-package org
  :general
  (:states '(normal visual) :prefix leader "c" #'org-capture)
  (:keymaps 'global :states 'normal "s-h" #'evil-jump-backward)
  (:keymaps 'org-mode-map :states 'normal :prefix local-leader "tt" #'org-todo)
  (:keymaps 'org-mode-map
            "s-i" #'me/emphasize-italic
            "s-_" #'me/emphasize-underline
            "s--" #'me/emphasize-strike
            "s-`" #'me/emphasize-code
            "s-=" #'me/emphasize-literal
            "s-b" #'me/emphasize-bold)
  (:states 'normal :keymaps 'org-mode-map
           "C-<return>" #'me/org-insert-heading
           "s-<return>" #'me/org-insert-todo
           "s-j" #'org-next-link
           "s-k" #'org-previous-link
           "s-l" #'org-open-at-point
           "<tab>" #'org-cycle
           "S-<tab>" #'org-shifttab)
  (:states 'insert :keymaps 'org-mode-map
           "C-<return>" #'me/org-insert-heading
           "s-<return>" #'me/org-insert-todo)

  :custom
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)

  :hook
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . visual-line-mode)

  :init
  (defun me/org-insert-heading ()
    (interactive)
    (org-insert-heading-respect-content)
    (evil-append-line 1))

  (defun me/org-insert-todo (arg &optional force-heading)
    (interactive "P")
    (goto-char (point-at-bol))
    (org-insert-todo-heading arg force-heading)
    (evil-append-line 1))

  (defvar +org-babel-load-functions ()
    "A list of functions executed to load the current executing src block. They
take one argument (the language specified in the src block, as a string). Stops
at the first function to return non-nil.")

  (defun +org--babel-lazy-load (lang)
    (cl-check-type lang symbol)
    (or (run-hook-with-args-until-success '+org-babel-load-functions lang)
        (require (intern (format "ob-%s" lang)) nil t)
        (require lang nil t)))

  (defun +org--src-lazy-load-library-a (lang)
    "Lazy load a babel package to ensure syntax highlighting."
    (or (cdr (assoc lang org-src-lang-modes))
        (+org--babel-lazy-load lang)))
  (advice-add #'org-src--get-lang-mode :before #'+org--src-lazy-load-library-a)

  (defun +org--babel-lazy-load-library-a (info)
    "Load babel libraries lazily when babel blocks are executed."
    (let* ((lang (nth 0 info))
           (lang (cond ((symbolp lang) lang)
                       ((stringp lang) (intern lang))))
           (lang (or (cdr (assq lang +org-babel-mode-alist))
                     lang)))
      (when (and lang
                 (not (cdr (assq lang org-babel-load-languages)))
                 (+org--babel-lazy-load lang))
        (when (assq :async (nth 2 info))
          ;; ob-async has its own agenda for lazy loading packages (in the
          ;; child process), so we only need to make sure it's loaded.
          (require 'ob-async nil t))
        (add-to-list 'org-babel-load-languages (cons lang t)))
      t))


  (defun me/emphasize-bold () (interactive) (org-emphasize ?*))
  (defun me/emphasize-code () (interactive) (org-emphasize ?~))
  (defun me/emphasize-italic () (interactive) (org-emphasize ?/))
  (defun me/emphasize-literal () (interactive) (org-emphasize ?=))
  (defun me/emphasize-strike () (interactive) (org-emphasize ?+))
  (defun me/emphasize-underline () (interactive) (org-emphasize ?_))

  (setq org-modules '())
  (setq org-agenda-inhibit-startup t)
  (setq org-agenda-use-tag-inheritance t)

  (setq
   org-link-frame-setup '((vm . vm-visit-folder-other-frame)
                          (vm-imap . vm-visit-imap-folder-other-frame)
                          (gnus . org-gnus-no-new-news)
                          (file . find-file)
                          (wl . wl-other-frame)))
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

  :config
  (defadvice org-babel-execute-src-block (around load-language nil activate)
    "Load language if needed"
    (let ((language (org-element-property :language (org-element-at-point))))
      (unless (cdr (assoc (intern language) org-babel-load-languages))
        (add-to-list 'org-babel-load-languages (cons (intern language) t))
        (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
      ad-do-it))

  (evil-add-command-properties #'org-open-at-point :jump t)
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
  (require 'ox-md))

(use-package ox-pandoc
  :defer t
  :init
  (setq org-pandoc-options '((standalone . t))))

(use-package orgraphy
  :straight (orgraphy :type git :host github :repo "matthias-margush/orgraphy")
  :config
  (orgraphy--init))

(use-package deft
  :commands (deft)

  :general
  (:map 'global-map
        "s-d" #'me/notes-switch-or-open)
  :init
  (setq deft-directory "~/Notes")
  (setq deft-auto-save-interval 0)
  (setq deft-extensions '("org"))
  (setq deft-default-extension "org")
  (setq deft-use-filename-as-title nil)
  (setq deft-use-filter-string-for-filename t)

  :config
  (defun me/deft ()
    (interactive)
    (deft)
    (deft-filter-clear)
    (evil-insert-state))

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
      (me/deft))))

(provide 'org-config)
