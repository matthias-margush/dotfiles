(use-package imenu-list
  :bind ("C-'" . #'imenu-list-smart-toggle))

(use-package counsel
  :bind (("C-x b" . counsel-switch-buffer)
	 ("s-:" . counsel-M-x))
  :init
  (setq ivy-count-format ""
	ivy-use-virtual-buffers t)

  :config
  (counsel-mode)
  (defun counsel-more-chars ()))

(global-set-key (kbd "M-x") 'counsel-M-x)

(use-package counsel-projectile
  :bind ("C-x C-f" . counsel-find-file)
  )

;(use-package ibuffer-sidebar
;  :commands (ibuffer-sidebar-toggle-sidebar))

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

(use-package projectile
  :demand t
  ;:hook (projectile-after-switch-project . me/project-tab)
  :bind-keymap ("s-p" . projectile-command-map)
  :bind (("s-e" . projectile-run-eshell)
	 ("s-F" . counsel-projectile-rg)
  	 (:map projectile-command-map
	       ("p" . counsel-projectile-switch-project)
	       ("f" . counsel-projectile-find-file)
	       ("b" . counsel-projectile-switch-to-buffer)
  	       ("s-p" . me/project-notes)
  	       ("s r" . counsel-projectile-rg)
  	       ("s a" . counsel-projectile-ag)
  	       ("s s" . counsel-projectile-ag)
  	       ("t" . me/neotree-toggle)
  	       ("x f" . vterm)))
  :init
  (setq projectile-switch-project-action #'me/project-notes)
  (setq ;; projectile-completion-system 'ido
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
       (let ((project-name (projectile-project-name)))
           (if (not (string= "" project-name))
             (format " in [%s]" project-name)
             (format " in [%s]" (frame-parameter nil 'me/projectile-project-name)))))))
