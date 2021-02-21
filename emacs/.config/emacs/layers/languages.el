;; -*- lexical-binding: t; -*-

(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") #'eval-last-sexp)

(use-package eglot
  :bind-keymap ("s-l" . eglot-mode-map)
  :bind ((:map eglot-mode-map
	       ("r" . eglot-rename)
	       ("f" . eglot-format)
	       ("o" . eglot-code-action-organize-imports))))

(use-package markdown-mode
  :hook ((markdown-mode . visual-line-mode))
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . gfm-mode)
	 ("\\.markdown\\'" . gfm-mode))
  :init
  (setq markdown-header-scaling t
	markdown-fontify-code-blocks-natively t
	markdown-hide-markup t
	markdown-hide-urls t
	markdown-header-scaling t
	markdown-display-remote-images t
	markdown-make-gfm-checkboxes-buttons t
	markdown-asymmetric-header t)
  :config
  (add-to-list 'markdown-code-lang-modes '("yaml" . yaml-mode))
  (add-to-list 'markdown-gfm-additional-languages '("yaml" . yaml-mode)))

(use-package go-mode
  :commands go-mode
  :mode (("\\.go\\'" . go-mode))
  :hook ((go-mode . me/go-editor-settings))
  :init
  (defun me/go-editor-settings ()
    ;; (setq flycheck-checker 'gometalinter)
    (electric-pair-mode)
    (setq indent-tabs-mode t
	  tab-width 2)))

(use-package yaml-mode
  :commands yaml-mode
  :mode (("\\.yaml\\'" . yaml-mode)
	 ("\\.yml\\'" . yaml-mode))
  :hook ((yaml-mode . hs-minor-mode)))

(use-package adoc-mode
  :mode (("\\.adoc\\'" . adoc-mode)))

(use-package vimrc-mode
  :ensure t
  :mode ("\\.vim\\'"))

;; (use-package sql-indent)
(use-package sqlformat
  ;; :hook (sql-mode . sqlformat-on-save-mode)
  :init
  (setq sqlformat-command 'pgformatter))
