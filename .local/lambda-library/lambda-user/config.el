;;; config.el --- summary -*- lexical-binding: t -*-
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
;; This file contains all user-specific configuration settings for 𝛌-Emacs.

;;; Code:

;; Run minimal set of modules (user should configure this how they wish)
;;;; Load Modules

;; Load modules
(measure-time
 (cl-dolist (mod (list

                  ;; Core modules
                  'lem-setup-libraries
                  'lem-setup-settings
                  'lem-setup-functions
                  'lem-setup-macros
                  'lem-setup-server
                  'lem-setup-scratch

                  ;; UI modules
                  'lem-setup-frames
                  'lem-setup-windows
                  'lem-setup-buffers
                  'lem-setup-fonts
                  'lem-setup-completion
                  'lem-setup-keybindings
                  'lem-setup-help
                  'lem-setup-modeline
                  'lem-setup-theme
                  'lem-setup-splash

                  ;; Navigation & Search modules
                  'lem-setup-navigation
                  'lem-setup-dired
                  'lem-setup-search

                  ;; Project & Tab/Workspace modules
                  'lem-setup-vc
                  'lem-setup-projects
                  'lem-setup-tabs
                  'lem-setup-workspaces
                  ;; Writing modules
                  'lem-setup-writing
                  'lem-setup-citation
                  ;; Programming modules
                  'lem-setup-programming
                  'lem-setup-debug
                  'lem-setup-shell))
   (require mod)))

;; MacOS settings - defer load until after init. 
(when sys-mac                                    
  (measure-time                                  
   (run-with-idle-timer 1 nil                    
                        (function require)       
                        'lem-setup-macos nil t)))

;; Fonts
(custom-set-variables
 '(lem-ui-default-font
   '(:font "JetBrains Mono" :weight normal :height 160)))

(custom-set-variables
 '(lem-ui-variable-width-font
   '(:font "DejaVu Sans" :weight normal :height 140)))


;;; Provide
(provide 'config)
;;; config.el ends here
