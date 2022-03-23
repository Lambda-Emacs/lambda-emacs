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

;; Setup for frames

;;; Code:

;;;; Frames

;;;;; Frame defaults
(use-package frame
  :straight (:type built-in)
  :config
  ;; Make a clean & minimalist frame
  (setq-default initial-frame-alist
                (append (list
                         '(fullscreen . maximized)
                         '(internal-border-width . 13)
                         '(tool-bar-lines . 0)
                         '(menu-bar-lines . 0)
                         '(vertical-scroll-bars . nil)
                         '(horizontal-scroll-bars . nil)
                         '(height . 45)
                         '(width . 85)
                         )))
  (setq-default default-frame-alist
                (append (list
                         '(frame-title-format . nil)
                         '(internal-border-width . 13)
                         '(tool-bar-lines . 0)
                         '(menu-bar-lines . 0)
                         '(vertical-scroll-bars . nil)
                         '(horizontal-scroll-bars . nil)
                         )))
  ;; Resize pixel-wise to avoid gaps
  (setq-default window-resize-pixelwise t)
  (setq-default frame-resize-pixelwise t)
  ;; Don't show icon in frame
  (setq-default ns-use-proxy-icon nil))

;;;;; Fix titlebar titling colors
;; see also https://github.com/d12frosted/homebrew-emacs-plus/issues/55
(use-package ns-auto-titlebar
  :commands ns-auto-titlebar-mode
  :if (eq system-type 'darwin)
  :init (ns-auto-titlebar-mode))

;;;;; Center Frames
;; https://christiantietze.de/posts/2021/06/emacs-center-window-single-function/
(defun lem/frame-recenter (&optional frame)
  "Center FRAME on the screen.
FRAME can be a frame name, a terminal name, or a frame.
If FRAME is omitted or nil, use currently selected frame."
  (interactive)
  (unless (eq 'maximised (frame-parameter nil 'fullscreen))
    (let* ((frame (or (and (boundp 'frame) frame) (selected-frame)))
           (frame-w (frame-pixel-width frame))
           (frame-h (frame-pixel-height frame))
           ;; frame-monitor-workarea returns (x y width height) for the monitor
           (monitor-w (nth 2 (frame-monitor-workarea frame)))
           (monitor-h (nth 3 (frame-monitor-workarea frame)))
           (center (list (/ (- monitor-w frame-w) 2)
                         (/ (- monitor-h frame-h) 2))))
      (apply 'set-frame-position (flatten-list (list frame center))))))

;; (add-hook 'after-init-hook #'lem/frame-recenter)
(add-hook 'after-make-frame-functions #'lem/frame-recenter)

;;;;; Fringe
(use-package fringe
  :straight (:type built-in)
  :custom
  ;; allow fringe indicators
  (fringe-mode '(0 . 8)))


(provide 'lem-setup-frames)
;;; lem-setup-frames.el ends here
