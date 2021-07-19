;; -*- mode: emacs-lisp; lexical-binding: t; -*-

(require 'keys-transient-config)

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

(define-transient-command me/transient-profiler
  "Profiler"
  ["Profiler"
   ("b" "Begin" me/profiler-start)
   ("e" "End" profiler-stop)
   ("r" "Report" profiler-report)
   ("R" "Reset" profiler-reset)])

(define-transient-command me/transient-toggles
  "Toggles"
  ["Toggles"
   ["Visual"
    ("c" "Color Highlight" rainbow-mode)
    ("v" "Invisible Text" visible-mode)
    ("w" "Word Wrap" me/word-wrap)
    ("W" "Whitespace" whitespace-mode)
    ("o" "Orgraphy" orgraphy-mode)
    ("f" "Fonts" font-lock-mode)
    ("p" "Pairing" me/toggle-pairing)]
   ["Troubleshooting"
    ("d" "Debug on Error" toggle-debug-on-error)
    ("P" "Profiler" me/transient-profiler)]])

;; (defhydra me/hydra-profiler (:exit t)
;;   "Profiler"
;;   ("b" me/profiler-start "Begin")
;;   ("e" profiler-stop "End")
;;   ("r" profiler-report "Report")
;;   ("R" profiler-reset "Reset"))

;; (defhydra me/hydra-toggles (:exit t :hint nil)
;;   "
;; ^Toggles^              ^Troubleshooting
;; ^^^^--------------------------------------
;; _p_ Pairing            _d_ Debug on Error
;; _c_ Color Highlight    _P_ Profiler
;; _v_ Invisible Text     
;; _w_ Word Wrap
;; _W_ Whitespace
;; _o_ Orgraphy
;; _f_ Fonts"
;;   ;; Toggles
;;   ("p" me/pairing)
;;   ("c" rainbow-mode)
;;   ("v" visible-mode)
;;   ("w" me/word-wrap)
;;   ("W" whitespace-mode)
;;   ("o" orgraphy-mode)
;;   ("f" font-lock-mode)
;;   ;; Troubleshooting
;;   ("d" toggle-debug-on-error)
;;   ("P" me/hydra-profiler/body))

;; (defhydra+ me/hydra-leader ()
;;   ("t" me/hydra-toggles/body "Toggles" :exit t))

;; (transient-append-suffix 'me/transient-leader '(0 -1)
;;   '("t" "Toggles" me/transient-toggles))

;; (transient-get-suffix 'me/transient-leader '())
;; (transient-get-suffix 'me/transient-leader '([:description "Settings"]))
;; (transient-get-suffix 'me/transient-leader '(0))
;; (transient-get-suffix 'me/transient-leader '([:command me/transient-toggles]))

;; (transient--layout-member '() 'me/transient-settings)
;; (transient--group-member
;;  (transient--kbd '[0])
;;  (get 'me/transient-leader 'transient--layout))


;; (define-key me/toggles-map (kbd "s") #'evil-ex-nohighlight)
;; (general-define-key :states 'normal :prefix leader "s" #'evil-ex-nohighlight)

(provide 'keys-toggles-config)
