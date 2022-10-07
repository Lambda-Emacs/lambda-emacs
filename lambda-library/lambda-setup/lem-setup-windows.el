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

;;;; Window Setup
(use-package window
  :ensure nil
  :custom
  ;; Emacs often opens buffers in new windows. Make window splitting and
  ;; placement more predictable.
  ;; TODO: revisit this setting?
  (display-buffer-base-action nil)
  ;; NOTE: I found the below unhelpful, but not sure what better settings would
  ;; be.
  ;; '((display-buffer-use-least-recent-window
  ;; display-buffer--maybe-pop-up-frame-or-window display-buffer-reuse-window
  ;; display-buffer-reuse-mode-window display-buffer-same-window
  ;; display-buffer-in-previous-window display-buffer-pop-up-frame)))
  )

;;;; Window Division
;; Vertical window divider
(use-package frame
  :ensure nil
  :custom
  (window-divider-default-right-width 1)
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

(defun lem-other-window ()
  (interactive)
  (other-window 1))
(bind-key* "C-c C-o" 'lem-other-window)

;; Move by window numbers
(use-package winum
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
  :ensure nil
  :commands (windmove-up
             windmove-down
             windmove-left
             windmove-right)
  :bind (("C-c C-h" . #'windmove-left)
         ("C-c C-l" . #'windmove-right)
         ("C-c C-j" . #'windmove-down)
         ("C-c C-k" . #'windmove-up))
  :config
  (windmove-default-keybindings))

;;;;; Window Restore
;; Winner mode is a built-in package for restoring window configurations
;; https://www.emacswiki.org/emacs/WinnerMode
(use-package winner
  :ensure nil
  :hook (after-init . winner-mode))


(provide 'lem-setup-windows)
;;; lem-setup-windows.el ends here
