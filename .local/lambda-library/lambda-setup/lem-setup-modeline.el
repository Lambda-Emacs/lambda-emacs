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

;; Setup for modeline. Currently I just have bespoke-modeline configured.

;;; Code:

;;;; Bespoke Mode line
(use-package bespoke-modeline
  :straight (:type git :host github :repo "mclear-tools/bespoke-modeline")
  :init
  ;; Set header-line
  (setq bespoke-modeline-position 'top)
  ;; Set mode-line height
  (setq bespoke-modeline-size 2)
  ;; Show diff lines in mode-line
  (setq bespoke-modeline-git-diff-mode-line t)
  ;; Set mode-line cleaner
  (setq bespoke-modeline-cleaner t)
  ;; Use mode-line visual bell
  (setq bespoke-modeline-visual-bell t)
  ;; Set vc symbol
  (setq bespoke-modeline-vc-symbol "") ;; 
  :config
  (bespoke-modeline-mode))

;;;; Hide Modeline
(use-package emacs-hide-mode-line
  :straight (:type git :host github :repo "hlissner/emacs-hide-mode-line")
  :commands hide-mode-line-mode)


(provide 'lem-setup-modeline)
;;; lem-setup-modeline.el ends here
