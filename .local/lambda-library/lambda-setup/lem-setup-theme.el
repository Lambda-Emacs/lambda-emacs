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

;; Theme settings & functions 𝛌-Emacs comes with its own set of themes --
;; 𝛌-Themes. The user can of course configure things to use whatever theme they
;; like. The Modus themes are especially good and included with Emacs 28+.

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

;;;; Toggle Menubar
;; Toggle MacOS menubar to light or dark
;; Requires dark-mode
;; https://github.com/sindresorhus/dark-mode
(defun lem-osx-toggle-menubar-theme ()
  "toggle menubar to dark or light using shell command"
  (interactive)
  (shell-command "dark-mode"))
(defun lem-osx-menubar-theme-light ()
  "turn dark mode off"
  (interactive)
  (shell-command "dark-mode off"))
(defun lem-osx-menubar-theme-dark ()
  "turn dark mode on"
  (interactive)
  (shell-command "dark-mode on"))

;;;; Theme & menubar toggle
;; (setq active-theme 'light-theme)
(defun toggle-dark-light-theme ()
  "Coordinate setting of theme with os theme and toggle"
  (interactive)
  (cond  ((not (executable-find "dark-mode"))
          (ding)
          (message "Please install dark-mode for CLI. See https://github.com/sindresorhus/dark-mode."))
         ((eq active-theme 'light-theme)
          (lem-osx-menubar-theme-dark)
          (setq active-theme 'dark-theme))
         (t
          (lem-osx-menubar-theme-light)
          (setq active-theme 'light-theme))))

;;;;; After Load Theme Hook
;; Not all themes provide an after-load hook, so create a hook you can run after
;; loading any theme.
(defvar lem-after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'lem-after-load-theme-hook))

;;;; Lambda Themes
;; Set a default theme. Lambda-themes comes with four variants:
;; light, light-faded, dark, and dark-faded.
;; See https://github.com/Lambda-Emacs/lambda-themes
(use-package lambda-themes
  :straight nil
  :load-path "~/.emacs.d/.local/lambda-library/lambda-user/custom-themes/lambda-themes"
  :custom
  ;; Custom settings. To turn any of these off just set to `nil'.
  (lambda-themes-set-variable-pitch t)
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t))

;;;;; System Appearance Hook
;; See https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
;; NOTE: This function works only for MacOS.
(defun lem--system-apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (progn
              (load-theme 'lambda-light)
              (setq active-theme 'light-theme)))
    ('dark (progn
             (load-theme 'lambda-dark)
             (setq active-theme 'dark-theme)))))

;; It seems like ns-system-appearance can only have one function hooked to it,
;; so remove the custom early init hook (that we added to stop the frame
;; from flashing) after startup and add regular system-apply-theme hook.
(when sys-mac
  (progn
    (remove-hook 'ns-system-appearance-change-functions #'lem--apply-default-background)
    (add-hook 'ns-system-appearance-change-functions #'lem--system-apply-theme)))

;;;;; Load Theme
(cond ((eq active-theme 'light-theme)
       (load-theme 'lambda-light t))
      ((eq active-theme 'dark-theme)
       (load-theme 'lambda-dark t))
      (t
       (load-theme 'lambda-light t)))

;; kind-icon (see lem-setup-completion) needs to have its cache flushed after
;; theme change
(with-eval-after-load 'kind-icon
  (add-hook 'lambda-themes-after-load-theme-hook #'kind-icon-reset-cache))

;;; Provide
(provide 'lem-setup-theme)
;;; lem-setup-theme.el ends here
