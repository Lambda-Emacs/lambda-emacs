;;; lem-setup-navigation.el --- summary -*- lexical-binding: t -*-

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

;; Setup code for "navigating" Emacs, in the sense of outlines, goto functions,
;; saving place, recent files, etc.

;;; Code:

;;;; Imenu list outline
;; Make a useful outline buffer
;; Binding set to toggle "o"
(use-package imenu-list
  :commands (imenu-list-smart-toggle imenu-list-minor-mode)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-position 'left))

;;;; Save place
;; Everyone remember where we parked.
(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  (setq save-place-file (concat lem-cache-dir "saved-places"))
  (setq save-place-forget-unreadable-files nil))

;;;; Go To Change
;; Easily go to last change
(use-package goto-last-change
  :straight (:type git :host github :repo "camdez/goto-last-change.el")
  :bind (("C-\"" . #'goto-last-change)))

;;;; Recent files
;; List recent files
(use-package recentf
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-save-file (concat lem-cache-dir "recentf"))
  ;; remove agenda files from list.
  (setq recentf-exclude '(org-agenda-files
                          "bookmarks"
                          "elpa"
                          ".cache"
                          ".cask"
                          "cache"
                          "recentf"
                          "url"
                          recentf-max-saved-items 500
                          recentf-max-menu-items 10)))

;;;; Goto Address
;; This package allows you to click or hit a key sequence while on a
;; URL or e-mail address, and either load the URL into a browser of
;; your choice using the browse-url package, or if it's an e-mail
;; address, to send an e-mail to that address.
(use-package goto-addr
  :hook ((compilation-mode . goto-address-mode)
         (prog-mode . goto-address-prog-mode)
         (eshell-mode . goto-address-mode)
         (text-mode . goto-address-mode)
         (shell-mode . goto-address-mode))
  :bind (:map goto-address-highlight-keymap
         ("<RET>"  . goto-address-at-point)
         ("M-<RET>" . newline))
  :commands (goto-address-prog-mode
             goto-address-mode))

;;;; Goto Functions
(defun lem-goto-private ()
  (interactive)
  (find-file (concat lem-user-dir "private.el")))
(defun lem-goto-journal ()
  (interactive)
  (find-file (concat org-directory "journal.org")))
(defun lem-goto-early-init.el ()
  "Open early-init.el file"
  (interactive)
  (find-file "~/.emacs.d/early-init.el"))
(defun lem-goto-init.el ()
  "Open init.el file"
  (interactive)
  (find-file user-init-file))
(defun lem-goto-custom.el ()
  "Open custom.el file"
  (interactive)
  (find-file custom-file))
(defun lem-goto-config ()
  "Open user config file"
  (interactive)
  (find-file lem-config-file))
(defun lem-load-config ()
  "Load config "
  (interactive)
  (load-file user-init-file)
  (load-file lem-config-file))
(defun lem-goto-emacs-dir ()
  "Open 𝛌-Emacs directory"
  (interactive)
  (find-file user-emacs-directory))
(defun lem-goto-org-files ()
  "Open directory with org files"
  (interactive)
  (find-file org-directory))
(defun lem-goto-projects ()
  "Open projects dir"
  (interactive)
  (find-file lem-project-dir))
(defun lem-goto-elisp-library ()
  "Open user elisp library."
  (interactive)
  (find-file lem-user-elisp-dir))

;;;; Jump in Buffer
(defun lem-jump-in-buffer ()
  "Jump between headlines in buffer using consult"
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (call-interactively 'consult-org-heading))
   (t
    (call-interactively 'consult-outline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Provide
(provide 'lem-setup-navigation)

;;; lem-setup-navigation.el ends here
