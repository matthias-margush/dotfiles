;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(mm/package 'multi-line)
(global-set-key (kbd "C-c s") #'multi-line)

(mm/package 'iedit)

;; (mm/package 'drag-stuff)
;; (drag-stuff-global-mode)
;; (drag-stuff-define-keys)


(global-set-key (kbd "s-.") #'narrow-or-widen-dwim)

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

(mm/package 'editorconfig)
;; https://github.com/editorconfig/editorconfig-emacs/issues/244#issuecomment-783127682
(setq editorconfig--enable-20210221-testing t)
(setq editorconfig-exclude-modes '(org))
(editorconfig-mode)

(mm/package 'multiple-cursors)

(mm/package 'expand-region)
(global-set-key (kbd "C-<up>") #'er/expand-region)
(global-set-key (kbd "C-<down>") #'er/contract-region)
(global-set-key (kbd "C-S-<left>") #'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-S-<right>") #'mc/edit-ends-of-lines)
(global-set-key (kbd "C-S-SPC") #'set-rectangular-region-anchor)

(delete-selection-mode)

(provide 'configure-editing)
