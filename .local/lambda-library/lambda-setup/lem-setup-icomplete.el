;;; lem-setup-icomplete.el --- summary -*- lexical-binding: t -*-

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

;; Vertico is a much better overall completion experience, but icomplete is
;; built-in so use this when running emacs for testing, etc.

;;; Code:

;;;; Icomplete
(use-package icomplete-vertical
  :straight (:type built-in)
  :hook (after-init . icomplete-vertical-mode)
  :bind
  (:map icomplete-minibuffer-map
   ("C-v"   . icomplete-vertical-toggle)
   ("RET"   . icomplete-force-complete-and-exit)
   ("TAB"   . icomplete-force-complete-and-exit)
   ("C-M-i" . minibuffer-complete)
   ("M-RET" . exit-minibuffer)
   ("<down>". icomplete-forward-completions)
   ("C-j"   . icomplete-forward-completions)
   ("<up>"  . icomplete-backward-completions)
   ("C-k"   . icomplete-backward-completions))
  :custom
  (icomplete-show-matches-on-no-input t)
  (icomplete-hide-common-prefix nil)
  (icomplete-compute-delay 0.0)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  (icomplete-scroll t)
  :config
  (icomplete-mode)
  (icomplete-vertical-mode))



(provide 'lem-setup-icomplete)
;;; lem-setup-icomplete.el ends here
