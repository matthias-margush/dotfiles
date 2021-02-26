;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(defun me/deft ()
  (interactive)
  (if (fboundp 'spacebar-deft)
      (spacebar-deft)
      (deft))
  (deft-filter-clear)
  (evil-insert-state))

(with-eval-after-load 'org
  (general-define-key
   :states 'normal
   :keymaps '(org-mode-map)
   "s-j" #'org-next-link
   "s-k" #'org-previous-link
   "s-l" #'org-open-at-point))

(evil-add-command-properties #'org-open-at-point :jump t)
(evil-define-key 'normal 'global (kbd "s-h") #'evil-jump-backward)

;; Notes
(use-package org
  :custom
  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)

  :after
  exec-path-from-shell

  :hook
  (org-babel-after-execute . org-redisplay-inline-images)
  (org-mode . visual-line-mode)

  :init
  (setq
   org-link-frame-setup '((vm . vm-visit-folder-other-frame)
			  (vm-imap . vm-visit-imap-folder-other-frame)
			  (gnus . org-gnus-no-new-news)
			  (file . find-file)
			  (wl . wl-other-frame))) ()
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
   org-ellipsis "  "
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
  (add-to-list 'org-file-apps '(t . emacs) t)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)
     (plantuml . t)
     (gnuplot . t)
     (shell . t)
     (clojure . t)))
  (require 'ox-md))

;; (use-package ox-pandoc
;;   :after exec-path-from-shell
;;   :init
;;   (setq org-pandoc-options '((standalone . t))))

(use-package orgraphy
  :straight (orgraphy :type git :host github :repo "matthias-margush/orgraphy")
  :config
  (orgraphy--init))

(use-package deft
  :commands (deft deft-filter-clear)
  :bind
  (:map global-map
	("s-d" . deft))
  :init
  (setq deft-directory "~/Org"
	deft-auto-save-interval 0
	deft-extensions '("org")
	deft-default-extension "org"
	deft-use-filename-as-title nil
	deft-use-filter-string-for-filename t))
