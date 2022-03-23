;;; lem-setup-windows.el --- summary -*- lexical-binding: t -*-

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

;; Setup for windows

;;; Code:

;;;; Windows
;; Vertical window divider
(use-package frame
  :straight (:type built-in)
  :custom
  (window-divider-default-right-width 10)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t))
;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)

;;;;; Window Movement
;;FIXME: Figure out how best to streamline window movement.
;; Quickly switch windows in Emacs
(use-package ace-window
  :commands (ace-window
             ace-swap-window
             aw-flip-window))

(defun lem/other-window ()
  (interactive)
  (other-window 1))
(bind-key* "C-c C-o" 'lem/other-window)

;; Move by window numbers
(use-package emacs-winum
  :straight (winum :type git :host github :repo "deb0ch/emacs-winum")
  :hook (after-init . winum-mode)
  :custom
  ;; seems to require being set in custom to take effect
  (winum-auto-setup-mode-line nil)
  :config
  (setq window-numbering-scope            'global
        winum-reverse-frame-list          nil
        winum-auto-assign-0-to-minibuffer t
        ;; winum-format                      " %s
        ;; winum-mode-line-position          1
        winum-ignored-buffers             '(" *which-key*")
        winum-ignored-buffers-regexp      '(" \\*Treemacs-.*")))

;; Easy window movement by key
(use-package windmove
  :straight (:type built-in)
  :commands (windmove-up
             windmove-down
             windmove-left
             windmove-right)
  :bind (("C-<left>" . #'windmove-left)
         ("C-<right>" . #'windmove-right)
         ("C-<down>" . #'windmove-down)
         ("C-<up>" . #'windmove-up))
  :config
  (windmove-default-keybindings))

;; Easy split and move functions
(defun lem/split-window-right-and-focus ()
  "Split the window horizontally and focus the new window."
  (interactive)
  (require 'windmove)
  (split-window-right)
  (windmove-right))
(defun lem/split-window-below-and-focus ()
  "Split the window vertically and focus the new window."
  (interactive)
  (require 'windmove)
  (split-window-below)
  (windmove-down))

;;;;; Window Restore
;; Winner mode is a built-in package for restoring window configurations
;; https://www.emacswiki.org/emacs/WinnerMode
(use-package winner
  :straight nil
  :hook (after-init . winner-mode))

;;;; Dialogs, Menus, & Popups

;;;;; Dialogs and popups
;; No file dialog
(setq use-file-dialog nil)
;; No dialog box
(setq use-dialog-box nil)
;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)
;; Set popup windows
(setq-default pop-up-windows t)
;; Set popup frames
(setq-default pop-up-frames nil)

;;;;; Hydra Menus
(use-package hydra :defer 1)

;;;;; Transient Menus
(use-package transient
  :defer 1
  :custom
  (transient-levels-file (concat lem-cache-dir "transient/levels.el"))
  (transient-values-file (concat lem-cache-dir "transient/values.el"))
  (transient-history-file (concat lem-cache-dir "transient/history.el"))
  ;; set transient popop to top of window
  (transient-display-buffer-action '(display-buffer-in-side-window
                                     (side . top)
                                     (dedicated . t)
                                     (inhibit-same-window . t)
                                     (window-parameters (no-other-window . t)))))


(provide 'lem-setup-windows)
;;; lem-setup-windows.el ends here
