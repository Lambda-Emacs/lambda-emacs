;;; lem-setup-macros.el --- summary -*- lexical-binding: t -*-

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

;; Useful macros

;;; Code:

;;;; Anaphora
;; Useful macro library: Anaphoric expressions implicitly create one or more
;; temporary variables which can be referred to during the expression. This
;; technique can improve clarity in certain cases. It also enables recursion for
;; anonymous functions.
(use-package anaphora :straight t :defer 1)

;;;; Toggle macro

(defun deftoggle-var-doc (name)
  (concat "Non-nil if " name " is enabled.\n\n"
          "See " name
          " command for a description of this toggle."))
(defun deftoggle-fun-doc (name doc)
  (concat "Toggle " name " on or off.\n\n" doc))

(defmacro deftoggle (name doc enabler disabler)
  `(progn
     (defvar ,name nil ,(deftoggle-var-doc (symbol-name name)))
     (defun ,name (&optional enable)
       ,(deftoggle-fun-doc (symbol-name name) doc)
       (interactive)
       (if (called-interactively-p 'interactive)
           (progn
             (if ,name
                 ,disabler
               ,enabler)
             (setq ,name (not ,name)))
         (progn
           (if enable
               ,enabler
             ,disabler)
           (setq ,name enable))))))
;; It's very similar to define-minor-mode, but with all the hooks, keymaps, and lighters stripped out, so it's less verbose. Here I use it to toggle my theme for example:
;; (deftoggle sam-toggle-theme
;;   "Toggle theme between light and dark."
;;   (progn (disable-theme 'dracula)
;;          (load-theme 'spacemacs-light t))
;;   (progn (disable-theme 'spacemacs-light)
;;          (load-theme 'dracula t)))

(provide 'lem-setup-macros)

;;; lem-setup-macros.el ends here
