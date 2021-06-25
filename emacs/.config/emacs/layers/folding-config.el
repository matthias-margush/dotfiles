;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'package-config)
(require 'evil-config)

(add-hook 'prog-mode-hook #'hs-minor-mode)

;;;  yaml
(add-hook 'yaml-mode-hook #'me/yaml-folding)

(defun me/yaml-folding ()
  (interactive)
  (outline-minor-mode)
  (hs-minor-mode -1)
  (setq outline-regexp
        (rx
         (seq
	  bol
	  (group (zero-or-more "  ")
	         (or (group
		      (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
			       (seq "'" (*? (not (in "'" "\n"))) "'")
			       (*? (not (in ":" "\n"))))
			   ":"
			   (?? (seq
			        (*? " ")
			        (or (seq "&" (one-or-more nonl))
				    (seq ">-")
				    (seq "|"))
			        eol))))
		     (group (seq
			     "- "
			     (+ (not (in ":" "\n")))
			     ":"
			     (+ nonl)
			     eol))))))))

;; (use-package origami
;;   :general
;;   (:keymaps 'prog-mode-map :states '(normal) "zo" #'me/forward-open-node)
;;   (:keymaps 'prog-mode-map :states '(normal) "zc" #'me/forward-close-node)
;;   (:keymaps 'prog-mode-map :states '(normal) "za" #'me/forward-toggle-node)
;;   (:keymaps 'prog-mode-map :states '(normal) "<tab>" #'me/forward-toggle-node)
;;   (:keymaps 'prog-mode-map :states '(normal) "S-<tab>" #'origami-recursively-toggle-node)
;;   (:keymaps 'prog-mode-map :states '(normal) "zA" #'origami-recursively-toggle-node)
;;   (:keymaps 'prog-mode-map :states '(normal) "zO" #'origami-recursively-toggle-node)
;;   (:keymaps 'prog-mode-map :states '(normal) "zC" #'origami-recursively-toggle-node)
;;   (:keymaps 'prog-mode-map :states '(normal) "zM" #'origami-toggle-all-nodes)
;;   (:keymaps 'prog-mode-map :states '(normal) "zR" #'origami-toggle-all-nodes)

;;   :config
;;   (defun me/forward-open-node ()
;;     (interactive)
;;     (save-excursion
;;       (goto-char (point-at-bol))
;;       (goto-char (point-at-eol))
;;       (origami-open-node (current-buffer) (point))))

;;   (defun me/forward-close-node ()
;;     (interactive)
;;     (save-excursion
;;       (origami-forward-fold)
;;       (goto-char (point-at-bol))
;;       (goto-char (point-at-eol))
;;       (origami-close-node (current-buffer) (point))))

;;   (defun me/forward-toggle-node ()
;;     (interactive)
;;     (save-excursion
;;       (origami-forward-toggle-node (current-buffer) (point))))

;;   (global-origami-mode))

(provide 'folding-config)
