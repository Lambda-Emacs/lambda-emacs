;;; lem-setup-modeline.el --- summary -*- lexical-binding: t -*-

;; Author: Colin McLear
;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Setup for modeline.

;;; Code:

;;;; Lambda Line
(use-package lambda-line
  :straight (:local-repo "~/bin/lisp-projects/lambda-line")
  :custom
  (lambda-line-abbrev t)
  (lambda-line-position 'top)
  (lambda-line-hspace "  ")
  (lambda-line-prefix t)
  (lambda-line-prefix-padding nil)
  (lambda-line-status-invert nil)
  (lambda-line-gui-ro-symbol  " ⨂")  ;; ⬤◯⨂
  (lambda-line-gui-mod-symbol " ⬤") ;; ⨀⬤
  (lambda-line-gui-rw-symbol  " ◯")  ;; ◉ ◎ ⬤◯
  (lambda-line-space-top +.50)
  (lambda-line-space-bottom -.50)
  (lambda-line-symbol-position 0.1)
  :config
  (lambda-line-mode)
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))

;;;; Hide Modeline
(use-package emacs-hide-mode-line
  :straight (:type git :host github :repo "hlissner/emacs-hide-mode-line")
  :commands hide-mode-line-mode)

;;; Provide:
(provide 'lem-setup-modeline)
;;; lem-setup-modeline.el ends here
