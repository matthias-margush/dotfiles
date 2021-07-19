;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'keys-transient-config)

(delete-selection-mode t)

(define-key transient-map (kbd "<escape>") #'transient-quit-all)

(require 'keys-toggles-config)

(define-transient-command me/transient-leader
  "Shortcuts"
  ["Shortcuts"
   ("t" "Toggles" me/transient-toggles)])

(global-set-key (kbd "s-k") #'me/transient-leader)

(defun switch-to-last-bufer ()
  "Switch to last buffer"
  (interactive)
  (switch-to-buffer nil))

(global-set-key (kbd "C-6") #'switch-to-last-bufer)

(global-set-key [wheel-right] (lambda () (interactive) (scroll-left 1)))
(global-set-key [wheel-left] (lambda () (interactive) (scroll-right 1)))
(global-set-key (kbd "<escape>") #'keyboard-escape-quit)

;; common mac keybindings
(global-set-key (kbd "s-[") #'xref-pop-marker-stack)
(global-set-key (kbd "s-]") #'xref-push-marker-stack)
(global-set-key (kbd "s-o") #'find-file)

;; (global-set-key (kbd "s-f") #'isearch-forward-symbol-at-point)
(global-set-key (kbd "s-G") #'isearch-repeat-backward)

;; (global-set-key (kbd "M-<down>") #'scroll-up-command)
;; (global-set-key (kbd "M-<up>") #'scroll-down-command)
(unbind-key (kbd "M-<right>"))
(unbind-key (kbd "M-<left>"))
(global-set-key (kbd "s-<down>") #'end-of-buffer)
(global-set-key (kbd "s-<up>") #'beginning-of-buffer)

(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;; windows
(define-transient-command me/transient-windows
  "Windows"
  ["Windows"
   ["Move"
    ("h" "Move Left" windmove-left)
    ("j" "Move Right" windmove-down)
    ("k" "Move Up" windmove-up)
    ("l" "Move Down" windmove-right)]
   ["Layout"
    ("s" "Split Horizontally" split-window-below)
    ("v" "Split Vertically" split-window-right)
    ("c" "Close" delete-window)
    ("o" "Close others" delete-other-windows)
    ("u" "Undo" winner-undo)
    ("r" "Redo" winner-redo)]])

(transient-append-suffix #'me/transient-leader '(0 -1) '("w" "Windows" me/transient-windows))
;; (global-set-key (kbd "s-d") #'split-window-below)
;; (global-set-key (kbd "s-D") #'split-window-right)
;; (global-set-key (kbd "C-w") #'me/transient-windows)
(global-set-key (kbd "M-s-/") #'split-window-right)
(global-set-key (kbd "M-s--") #'split-window-below)
(global-set-key (kbd "M-s-<left>") #'windmove-left)
(global-set-key (kbd "M-s-<right>") #'windmove-right)
(global-set-key (kbd "M-s-<up>") #'windmove-up)
(global-set-key (kbd "M-s-<down>") #'windmove-down)
(global-set-key (kbd "s-r") #'query-replace)
(global-set-key (kbd "s-R") #'query-replace-regexp)

(use-package drag-stuff
  :bind
  (("M-<up>" . drag-stuff-up)
   ("M-<down>" . drag-stuff-down)
   ("M-<right>" . drag-stuff-right)
   ("M-<left>" . drag-stuff-left)))

(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
        eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count)))
        )

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))

(global-set-key (kbd "s-D") #'duplicate-line)

(provide 'keys-init-config)
