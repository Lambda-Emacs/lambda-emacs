;;; config.el --- summary -*- lexical-binding: t -*-

;; Maintainer: Colin Mclear
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

;; Personal config file
;; This file contains all user-specific settings

;;; Code:
;;;; Personal Information

;; Give Emacs some personal info
(setq user-full-name ""
      user-mail-address "")

;;;;; Private File
;; where to store private or "secret" info
(let ((private (expand-file-name "private.el" lem-user-dir)))
  (if (file-exists-p private)
	  (load-file private)))

;;;; User Vars

;;;;; Set Fonts
;; Set fonts
;; (custom-set-variables
;;  '(lem-ui-default-font
;;    '(:font "" :height 120)))

;; (custom-set-variables
;;  '(lem-ui-variable-width-font
;;    '(:font "" :height 120)))


;;;;; Org Directories
;; Set these if you're using org
(setq org-directory ""
      org-default-notes-file (concat org-directory "inbox.org")
      org-agenda-files (list org-directory))

;;;;; Straight Package Manager
;; Don't walk straight repos
(push "straight" vc-directory-exclusion-list)
;; Delete .DS_Store before prune
(advice-add 'straight-prune-build :before #'(lambda () (move-file-to-trash (concat lem-var-dir "straight/build/.DS_Store"))))

;;;;; Set Splash Footer
;; Set a footer for the splash screen
(setq lem-splash-footer  "")

;;;; Load Modules
;; Load modules in stages for a shorter init time. We load core modules first,
;; then more expensive modules after init, with the rest loaded after startup
;; has fully completed.

;;;;; Load base modules
(message "
;; ======================================================
;; *Loading 𝛌-Emacs Base Modules*
;; ======================================================")
(measure-time
 (cl-dolist (mod (list
                  ;; Base modules
                  'lem-setup-libraries
                  'lem-setup-settings
                  'lem-setup-functions
                  'lem-setup-macros
                  'lem-setup-scratch
                  'lem-setup-theme

                  ;; Basic UI modules
                  'lem-setup-windows
                  'lem-setup-buffers
                  'lem-setup-frames
                  'lem-setup-fonts
                  'lem-setup-faces))
   (require mod nil t)))

;;;;; Load After-Init Modules
(defun lem-user-config-after-init ()
  "Modules loaded after init."
  (message "
  ;; ======================================================
  ;; *Loading 𝛌-Emacs after-init Modules*
  ;; ======================================================")
  (measure-time (cl-dolist (mod (list
                                 ;; Splash/Dashboard
                                 'lem-setup-splash
                                 ;;'lem-setup-dashboard

                                 ;; Completion & Keybinds
                                 'lem-setup-completion
                                 'lem-setup-keybindings

                                 ;; Navigation & Search modules
                                 'lem-setup-navigation
                                 'lem-setup-dired
                                 'lem-setup-search

                                 ;; Project & Tab/Workspace modules
                                 'lem-setup-vc
                                 'lem-setup-projects
                                 'lem-setup-tabs
                                 'lem-setup-workspaces))
                  (require mod nil t))))
(add-hook 'after-init-hook #'lem-user-config-after-init)

;;;;; Load After-Startup Modules
(defun lem-user-config-after-startup ()
  "Modules loaded after Emacs startup."
  (message "
  ;; ======================================================
  ;; *Loading 𝛌-Emacs after-startup Modules*
  ;; ======================================================")
  (measure-time (cl-dolist (mod (list

                                 ;; Writing modules
                                 'lem-setup-writing
                                 'lem-setup-notes
                                 'lem-setup-citation

                                 ;; Programming modules
                                 'lem-setup-programming
                                 'lem-setup-debug
                                 'lem-setup-skeleton

                                 ;; Shell & Terminal
                                 'lem-setup-shell
                                 'lem-setup-eshell

                                 ;; Org modules
                                 'lem-setup-org-base
                                 'lem-setup-org-settings
                                 'lem-setup-org-extensions

                                 ;; Productivity
                                 'lem-setup-pdf
                                 'lem-setup-elfeed

                                 ;; OS settings
                                 ;; load only if on macos
                                 (when sys-mac
                                   'lem-setup-macos)

                                 ;; Other UI/UX
                                 'lem-setup-help
                                 'lem-setup-colors
                                 'lem-setup-modeline

                                 ;; Server
                                 'lem-setup-server))
                  (require mod nil t))))
(add-hook 'emacs-startup-hook #'lem-user-config-after-startup)

;;;;; Scratch Directory
(customize-set-variable 'lem-scratch-default-dir lem-scratch-save-dir)

;;;; User Keybindings
;; Set this to whatever you wish. All Lambda-Emacs keybinds will be available through this prefix.
(customize-set-variable 'lem-prefix "C-c C-SPC")

;;;; User Functions
;; Put any custom user functions here.

;;; Provide
(provide 'config)
;;; config.el ends here
