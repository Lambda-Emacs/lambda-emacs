;;; lem-setup-dashboard.el --- Dashboard setup -*- lexical-binding: t -*-

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

;; All packages and setup related to the dashboard replacement of the splash screen. Further options or defaults may be overridden in the user's config.

;;; Code:

;;;; Dependencies
;; Load dependencies from Lambda-Emacs. Note these packages are usually deferred
;; but they must be loaded with dashboard since it requires icons.
(require 'corfu)
(require 'all-the-icons)

;; dashboard dependency
(use-package page-break-lines
  :defer t
  :config
  (set-fontset-font "fontset-default"
                    (cons page-break-lines-char page-break-lines-char)
                    (face-attribute 'default :family)))

;;;; Dashboard
(use-package dashboard
  :straight t
  ;; Use only with GUI emacs
  :if (display-graphic-p)
  :if (< (length command-line-args) 2)
  :custom-face
  (dashboard-heading ((t (:inherit font-lock-variable-name-face))))
  :custom
  ;; Set info & center
  (dashboard-set-init-info t)
  (dashboard-center-content t)
  ;; Footer
  (dashboard-set-footer t)
  (dashboard-footer-icon (all-the-icons-fileicon "emacs"
                                                 :height 1
                                                 :v-adjust -0.15
                                                 :face 'font-lock-string-face))
  ;; Set project backend
  (dashboard-projects-backend 'project-el)
  ;; Add icons
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((bookmarks . 5)
                     (recents  . 8)
                     (projects . 5)))
  :config/el-patch
  ;; Don't use imagemagick to create banner as it is notably worse in image quality
  (defun dashboard-insert-image-banner (banner)
    "Display an image BANNER."
    (when (file-exists-p banner)
      (let* ((title dashboard-banner-logo-title)
             (spec
              (create-image banner))
             (size (image-size spec))
             (width (car size))
             (left-margin (max 0 (floor (- dashboard-banner-length width) 2))))
        (goto-char (point-min))
        (insert "\n")
        (insert (make-string left-margin ?\ ))
        (insert-image spec)
        (insert "\n\n")
        (when title
          (insert (make-string (max 0 (floor (/ (- dashboard-banner-length
                                                   (+ (length title) 1)) 2))) ?\ ))
          (insert (format "%s\n\n" (propertize title 'face 'dashboard-banner-logo-title)))))))
  :config
  (dashboard-setup-startup-hook))

;;;; Dashboard Functions
;; Functions to call dashboard when it kas been killed or not loaded
(defun lem-dashboard ()
  "Load dashboard and swith to buffer."
  (interactive)
  (let ((buffer "*dashboard*"))
    (when (not (get-buffer buffer))
      (dashboard-insert-startupify-lists))
    (switch-to-buffer buffer))
  (delete-other-windows))

(defun lem-goto-dashboard ()
  "Goto the dashboard."
  (interactive)
  (switch-to-buffer "*dashboard*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lem-setup-dashboard)
