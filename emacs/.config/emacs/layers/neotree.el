(use-package neotree
  :custom
  (neo-smart-open t)

  :init
  (setq neo-theme 'icons))

(use-package all-the-icons)

(defun me/neotree-toggle ()
  "Open NeoTree using the git root."
  (interactive)
  (if (fboundp 'projectile-project-root)
      (let ((project-dir (projectile-project-root))
	    (file-name (buffer-file-name)))
	(neotree-toggle)
	(if project-dir
	    (if (neo-global--window-exists-p)
		(progn
		  (neotree-dir project-dir)
		  (neotree-find file-name)))
	  (message "Could not find git project root.")))
    (neotree-toggle)))
