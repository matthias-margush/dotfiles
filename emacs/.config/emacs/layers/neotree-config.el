;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)

(use-package neotree
  :hook
  (neotree-mode . hl-line-mode)

  :custom
  (neo-smart-open t)

  :init
  (setq neo-theme 'icons
        neo-hidden-regexp-list '("\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.o$")))

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

(defun me/sidebar ()
  "Open the sidebar."
  (interactive)
  (require 'neotree)
  (pcase-let ((`(,x . ,y) (frame-position))
              (`(,left ,top ,right ,bottom) (frame-edges))
               (border (+ (* 2 (frame-parameter (selected-frame) 'internal-border-width))
                         (frame-fringe-width)
                         ))
               (height (+ 0 (frame-height (selected-frame)))))
    ;; (message "%s, %s, %s, %s" left top right bottom)
    (let ((width (- right left))
	         (height (- bottom top (default-font-height) -1)))
      (setq frame-resize-pixelwise t)
      (if (neo-global--window-exists-p) ; then close
        (let ((neo-width (window-total-width (neo-global--get-window))))

          (me/neotree-toggle)

          (set-frame-position
            (selected-frame)
            (+ left neo-width (* 2 border))
            y)

          ;; (set-frame-height (selected-frame) (- 3 height) nil t)

          (set-frame-width
            (selected-frame)
            (- width neo-width (* 3 border))
            nil t))

        (progn                          ; else open
          (me/neotree-toggle)
          (let ((neo-width (window-total-width (neo-global--get-window))))
            (set-frame-position
             (selected-frame)
             (- left (+ neo-width (* 2 border)))
             y)

            ;; (set-frame-height (selected-frame) height nil t)

            (set-frame-width
             (selected-frame)
             (+ width neo-width border)
             nil t))))
      (set-frame-height (selected-frame) height nil t))))

(provide 'neotree-config)
