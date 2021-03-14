;;; construction-paper-light-theme.el --- Construction Paper Light Theme

;; Copyright (C) 2021 Matthias Margush <matthias.margush@gmail.com>

;; Author: Matthias Margush <matthias.margush@me.com>
;; URL: https://github.com/matthias-margush/construction-paper-theme-emacs
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.4.0"))
;; Keywords: theme, faces

;; This file is NOT part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Construction paper-inspired theme

;; See the README for more info:
;; https://github.com/matthias-margush/construction-paper-theme-emacs

;;; Code:
(deftheme construction-paper-light)

(require 'construction-paper-theme)

(let* ((construction-paper-theme--added "#EFEFFF")
       (construction-paper-theme--alert construction-paper-theme--fire)
       (construction-paper-theme--link construction-paper-theme--water)
       (construction-paper-theme--good construction-paper-theme--moss)
       (construction-paper-theme--removed "#FFEFEF")
       (construction-paper-theme--changed "#FFFFE2")
       (construction-paper-theme--warning construction-paper-theme--sun)
       (construction-paper-theme--find "#EFEFFF")
       (construction-paper-theme--background construction-paper-theme--light-bright)
       (construction-paper-theme--background-medium construction-paper-theme--light-medium)
       (construction-paper-theme--background-dark construction-paper-theme--light-dark)
       (construction-paper-theme--inverted-background construction-paper-theme--shadow-dark)
       (construction-paper-theme--inverted-background-medium construction-paper-theme--shadow-medium)
       (construction-paper-theme--inverted-background-bright construction-paper-theme--shadow-bright)
       (construction-paper-theme--block construction-paper-theme--light-medium)
       (construction-paper-theme--text construction-paper-theme--shadow-dark)
       (construction-paper-theme--highlight-background construction-paper-theme--sky)
       (construction-paper-theme--text construction-paper-theme--shadow-dark)
       (construction-paper-theme--inverted-text construction-paper-theme--light-bright)
       (construction-paper-theme--highlight-foreground construction-paper-theme--shadow-dark)
       (construction-paper-theme--deemphasize construction-paper-theme--shadow-bright)
       (construction-paper-theme--keyword construction-paper-theme--bark)
       (construction-paper-theme--doc construction-paper-theme--shadow-medium)
       (construction-paper-theme--nav construction-paper-theme--bark))

  (construction-paper-theme 'construction-paper-light)
  (provide-theme 'construction-paper-light))

(provide 'construction-paper-light-theme)

;;; constrution-paper-theme-light.el ends here
