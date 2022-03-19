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

;; All code related to navigating emacs goes here.

;;; Code:


;;;; Imenu list outline
(use-package imenu-list
  :commands (imenu-list-smart-toggle imenu-list-minor-mode)
  :config
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t
        imenu-list-position 'left))

;;;; Save place
(use-package saveplace
  :hook (after-init . save-place-mode)
  :config
  (setq save-place-file (concat lem-cache-dir "saved-places"))
  (setq save-place-forget-unreadable-files nil))

;;;; Go To Change
(use-package goto-last-change
  :straight (:type git :host github :repo "camdez/goto-last-change.el")
  :bind (("C-\"" . goto-last-change)))

;;;; Hydra
(use-package hydra :defer 1)

;;;; Recent files
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
                          recentf-max-saved-items 300
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

;;;; Goto Files
(defun lem/goto-private ()
  (interactive)
  (find-file (concat lem-elisp-dir "private.el")))
(defun lem/goto-journal ()
  (interactive)
  (find-file (concat org-directory "journal.org")))
(defun goto-early-init.el ()
  "Open early-init.el file"
  (interactive)
  (find-file "~/.emacs.d/early-init.el"))
(defun goto-init.el ()
  "Open init.el file"
  (interactive)
  (find-file user-init-file))
(defun goto-custom.el ()
  "Open custom.el file"
  (interactive)
  (find-file custom-file))
(defun goto-config.org ()
  "Open config.org file"
  (interactive)
  (find-file "~/.emacs.d/config.org"))
(defun load-config ()
  "Load config "
  (interactive)
  ;; (lem/tangle-emacs-config)
  (load-file user-init-file))
(defun goto-dotfiles.org ()
  "Open dotfiles.org file"
  (interactive)
  (find-file "~/dotfiles/dotfiles.org"))
(defun goto-emacs-dir ()
  "Open dotfiles.org file"
  (interactive)
  (find-file user-emacs-directory))
(defun goto-org-files ()
  "Open directory with org files"
  (interactive)
  (find-file org-directory))
(defun goto-pandoc-config ()
  "open pandoc metadata file"
  (interactive)
  (find-file "~/.pandoc/metadata.yml"))

;;;; Jump in Buffer
(defun lem/jump-in-buffer ()
  (interactive)
  (cond
   ((eq major-mode 'org-mode)
    (call-interactively 'consult-org-heading))
   (t
    (call-interactively 'consult-outline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'lem-setup-navigation)
;;; lem-setup-navigation.el ends here
