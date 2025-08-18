;;; lem-setup-frames.el --- summary -*- lexical-binding: t -*-

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

;; Setup for frames. Note that some basic settings are placed in early-init for
;; better startup.

;;; Code:

;;;; Frame defaults
(use-package frame
  :ensure nil
  :config
  ;; Make a clean & minimalist frame
  ;; To modify initial frame set `initial-frame-alist`
  (setq-default default-frame-alist
                (append (list
                         '(frame-title-format . nil)
                         '(internal-border-width . 30)
                         '(tool-bar-lines . 0)
                         '(vertical-scroll-bars . nil)
                         '(horizontal-scroll-bars . nil)
                         '(undecorated . t))))
  ;; Resize pixel-wise to avoid gaps
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t)

  ;; Don't show icon in frame
  (setq-default ns-use-proxy-icon nil))

;;;; (Re)Center Frames
(defun lem-frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (modify-frame-parameters
     frame '((user-position . t) (top . 0.5) (left . 0.5)))))

;; un/comment this hook if you want frames recentered
(add-hook 'after-make-frame-functions #'lem-frame-recenter)

;;;; Window Margins for Text Indentation
;; Add margins to windows to maintain text indentation with smaller frame borders
(defun lem-set-window-margins ()
  "Set left and right margins for all windows to maintain text indentation."
  (walk-windows
   (lambda (window)
     (unless (window-minibuffer-p window)
       (set-window-margins window 2 2)))))

;; Apply margins when windows change
(add-hook 'window-configuration-change-hook #'lem-set-window-margins)

;; Apply margins to initial frame
(add-hook 'after-init-hook #'lem-set-window-margins)

;; Apply margins to new frames
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (lem-set-window-margins)))

;;;; Fix titlebar titling colors
;; see also https://github.com/d12frosted/homebrew-emacs-plus/issues/55
(use-package ns-auto-titlebar
  :commands ns-auto-titlebar-mode
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode))

(provide 'lem-setup-frames)
;;; lem-setup-frames.el ends here
