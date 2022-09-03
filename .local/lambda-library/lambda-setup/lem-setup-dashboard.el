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
  :custom
  ;; Set info & center
  (dashboard-set-init-info t)
  (dashboard-center-content t)
  ;; Header
  (dashboard-startup-banner (concat lem-library-dir "lambda-logo.png"))
  (dashboard-banner-logo-title "Welcome to Lambda-Emacs")
  (dashboard-image-banner-max-width 200)
  (dashboard-image-banner-max-height 200)
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
  :config
  (dashboard-setup-startup-hook)
  (add-hook 'dashboard-mode-hook #'lem-dashboard-hide-modeline))

;;;; Dashboard Functions
;; Hide modeline
(defun lem-dashboard-hide-modeline ()
  "Hide modeline in the dashboard buffer."
  (let* ((dash-buffer (get-buffer "*dashboard*")))
    (with-current-buffer dash-buffer
      (setq-local hl-line-mode -1)
      (setq-local mode-line-format nil)
      (setq-local header-line-format nil))))

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
