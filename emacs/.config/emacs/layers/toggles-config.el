;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'keys-config)

(defvar me/pairing nil "Whether in pairing mode")

(defun me/toggle-pairing ()
  "Toggles useful when pairing."
  (interactive)
  (if me/pairing
      (progn
        (setq me/pairing nil)
        (global-linum-mode -1)
        ;; (global-hl-line-mode -1)
        )
    (setq me/pairing t)
    (global-linum-mode)
    ;; (global-hl-line-mode)
    ))

(defun me/word-wrap ()
  "Wrap lines & words"
  (interactive)
  (toggle-word-wrap)
  (toggle-truncate-lines))

(defun me/profiler-start ()
  (interactive)
  (profiler-start 'cpu))

(define-transient-command me/profiler-transient ()
  "Profiler"
  ["Profiler"
   ("b" "Begin" me/profiler-start)
   ("e" "End" profiler-stop)
   ("r" "Report" profiler-report)
   ("R" "Reset" profiler-reset)])

(define-transient-command me/toggles-transient ()
  "Toggles"
  ["Toggles"
   ["Visual"
    ("c" "Color Highlight" rainbow-mode)
    ("v" "Invisible Text" visible-mode)
    ("w" "Word Wrap" me/word-wrap)
    ("W" "Whitespace" whitespace-mode)
    ("o" "Orgraphy" orgraphy-mode)
    ("f" "Fonts" font-lock-mode)]
   ["Troubleshooting"
    ("d" "Debug on Error" toggle-debug-on-error)
    ("p" "Pairing" me/toggle-pairing)
    ("P" "Profiler" me/profiler-transient)]])

(global-set-key (kbd "C-M-s-t") 'me/toggles-transient)

(setq me/toggles-map (make-sparse-keymap))
(define-key me/toggles-map (kbd "r") #'rainbow-mode)
(define-key me/toggles-map (kbd "v") #'visible-mode)
(define-key me/toggles-map (kbd "w") #'toggle-word-wrap)
(define-key me/toggles-map (kbd "W") #'whitespace-mode)
(define-key me/toggles-map (kbd "d") #'toggle-debug-on-error)
(define-key me/toggles-map (kbd "o") #'orgraphy-mode)
(define-key me/toggles-map (kbd "f") #'font-lock-mode)
(define-key me/toggles-map (kbd "m") #'smerge-mode)
(define-key me/toggles-map (kbd "t") #'toggle-truncate-lines)
(define-key me/toggles-map (kbd "p") #'me/toggle-pairing)

(define-key me/toggles-map (kbd "s") #'evil-ex-nohighlight)
(setq me/profiler-map (make-sparse-keymap))
(define-key me/profiler-map (kbd "b") (lambda () (interactive) (profiler-start 'cpu)))
(define-key me/profiler-map (kbd "e") #'profiler-stop)
(define-key me/profiler-map (kbd "r") #'profiler-reset)

(define-key me/toggles-map (kbd "p") me/profiler-map)

(general-define-key :states 'normal :prefix leader "t" me/toggles-map)
(general-define-key :states 'normal :prefix leader "s" #'evil-ex-nohighlight)

(provide 'toggles-config)
