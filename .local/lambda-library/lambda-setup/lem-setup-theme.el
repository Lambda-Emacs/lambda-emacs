;;; lem-setup-theme.el --- summary -*- lexical-binding: t -*-

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

;; Theme settings & functions

;;; Code:

;;;; No-confirm themes
(setq custom-safe-themes t)

;;;; Custom Theme Folder
(defvar lem-custom-themes-dir (concat lem-user-dir "custom-themes/"))
(mkdir lem-custom-themes-dir t)
(setq-default custom-theme-directory lem-custom-themes-dir)

;; find all themes recursively in custom-theme-folder
(let ((basedir custom-theme-directory))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
        (add-to-list 'custom-theme-load-path (concat basedir f)))))

;;;; What Face?
;; https://stackoverflow.com/a/66287459/6277148
(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;;; Bespoke Theme
(use-package bespoke-themes
  ;; :straight (:type git :host github :repo "mclear-tools/bespoke-themes")
  :straight nil
  :load-path "~/.emacs.d/.local/lambda-library/lambda-user/custom-themes/bespoke-themes"
  :config
  ;; Set use of italics
  (setq bespoke-set-italic-comments t
        bespoke-set-italic-keywords t)
  ;; Set variable pitch
  (setq bespoke-set-variable-pitch t))

;;;; Disable All Custom Themes
(defun lem/disable-all-themes ()
  "disable all active themes."
  (interactive)
  (progn
    (dolist (i custom-enabled-themes)
      (disable-theme i))
    ;; disable window-divider mode
    (window-divider-mode -1)
    ;; revert to mode line
    (setq-default header-line-format nil)
    (setq-default mode-line-format
                  '((:eval
                     (list
                      ;; evil-mode-line-tag
                      "| "
                      "%b "
                      "%m "
                      (cond ((and buffer-file-name (buffer-modified-p))
                             (propertize "(**)" 'face `(:foreground "#f08290")))
                            (buffer-read-only "(RO)" ))
                      " %l:%c %0"
                      " "
                      ))))
    (force-mode-line-update)))

;;;; Load Theme Wrapper
(defun lem/load-theme ()
  (interactive)
  (progn
    (lem/disable-all-themes)
    (call-interactively 'load-theme)))

;;;; Toggle Menubar
;; toggle menubar to light or dark
(defun lem/osx-toggle-menubar-theme ()
  "toggle menubar to dark or light using shell command"
  (interactive)
  (shell-command "dark-mode"))
(defun lem/osx-menubar-theme-light ()
  "turn dark mode off"
  (interactive)
  (shell-command "dark-mode off"))
(defun lem/osx-menubar-theme-dark ()
  "turn dark mode on"
  (interactive)
  (shell-command "dark-mode on"))

;;;; Theme & menubar toggle
;; (setq active-theme 'light-theme)
(defun toggle-dark-light-theme ()
  "Coordinate setting of theme with os theme and toggle"
  (interactive)
  (if (eq active-theme 'light-theme)
      (progn (lem/osx-menubar-theme-dark)
             (setq active-theme 'dark-theme))
    (progn (lem/osx-menubar-theme-light)
           (setq active-theme 'light-theme))))

;;;; After Load Theme Hook
(defvar lem-after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'lem-after-load-theme-hook))

;;;; Reload Active Theme
(defun lem/bespoke-reload-theme ()
  "reload current bespoke theme"
  (interactive)
  (progn
    (bespoke--disable-all-themes)
    (load-theme 'bespoke t)))


(provide 'lem-setup-theme)
;;; lem-setup-theme.el ends here
