;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'evil-config)

(define-key isearch-mode-map (kbd "s-v") #'isearch-yank-kill)
(global-set-key (kbd "S-s-<return>") #'toggle-frame-fullscreen)

(defun me/launch-emacs-instance ()
  (interactive)
  (shell-command "open -n -a Emacs.app"))

(global-set-key (kbd "s-N") #'me/launch-emacs-instance)

(general-define-key
 :states '(normal visual)
 :prefix local-leader
 "," #'narrow-or-widen-dwim)

(winner-mode)
(define-key evil-window-map (kbd "u") #'winner-undo)
(define-key evil-window-map (kbd "R") #'winner-redo)

(defun me/open-messages ()
  (interactive)
  (split-window-below 40)
  (other-window 1)
  (switch-to-buffer "*Messages*"))

(general-define-key
 :states '(normal)
 :prefix leader
 "w" evil-window-map
 "bn" #'bookmark-set
 "bm" #'me/open-messages
 "TAB" #'evil-switch-to-windows-last-buffer)

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(provide 'keys-config)
