;;; setup-macos.el --- summary -*- lexical-binding: t -*-

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

;; Settings for use with macos

;;; Code:

;;;; Clipboad

;; Integrate with MacOS clipboard
(setq select-enable-clipboard t)

;; Saving whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
(setq save-interprogram-paste-before-kill t)

;; Fix for non-ascii characters
;; see https://gist.github.com/the-kenny/267162#gistcomment-2883522
(setenv "LANG" "en_US.UTF-8")

;;;; General Settings

(when sys-mac
  ;; make fonts look better with anti-aliasing
  (setq mac-allow-anti-aliasing t)

;;;; Delete to Trash
  (measure-time
   (message "*Loading delete to trash settings...*")
   ;; delete files by moving them to the trash
   ;; See https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
   (setq delete-by-moving-to-trash t)
   (setq trash-directory "~/.Trash")  ;; fallback for `move-file-to-trash'
   (defun system-move-file-to-trash (path)
     "Moves file at PATH to the macOS Trash according to `move-file-to-trash' convention.

Relies on the command-line utility 'trash' to be installed.
Get it from:  <http://hasseg.org/trash/>"
     (shell-command (concat "trash -vF \"" path "\""
                            "| sed -e 's/^/Trashed: /'")
                    nil ;; Name of output buffer
                    "*Trash Error Buffer*")))

;;;; Frames & Fullscreen

  ;; (Do not) make new frames when opening a new file with Emacs unless on scratch buffer
  (setq ns-pop-up-frames nil)

  ;; Fullscreen (disable for non-space full screen)
  ;; Using fullscreen with "notched" M1 macs requires non-native.
  (setq ns-use-native-fullscreen nil)

;;;; Keybindings

  ;; Set modifier keys
  (setq mac-option-modifier 'meta) ;; Bind meta to ALT
  (setq mac-command-modifier 'super) ;; Bind apple/command to super if you want
  (setq mac-function-modifier 'hyper) ;; Bind function key to hyper if you want
  (setq mac-right-option-modifier 'none) ;; unbind right key for accented input

  ;; Make forward delete work
  (global-set-key (kbd "<H-backspace>") 'delete-forward-char)

  ;; Keybindings
  (global-set-key (kbd "s-q") 'lem-delete-frame-or-quit)
  (global-set-key (kbd "H-q") 'lem-kill-all-emacsen)
  (global-set-key (kbd "s-v") 'yank)
  (global-set-key (kbd "s-c") 'meow-clipboard-save)
  (global-set-key (kbd "s-a") 'mark-whole-buffer)
  (global-set-key (kbd "s-x") 'kill-region)
  (global-set-key (kbd "s-w") 'delete-window)
  (global-set-key (kbd "s-W") 'delete-frame)
  (global-set-key (kbd "s-n") 'eyebrowse-create-window-config)
  (global-set-key (kbd "s-N") 'make-frame)
  (global-set-key (kbd "s-z") 'undo)
  (global-set-key (kbd "s-Z") 'undo-redo)
  (global-set-key (kbd "s-s")
                  (lambda ()
                    (interactive)
                    (call-interactively (key-binding "\C-x\C-s"))))
  ;; (global-set-key (kbd "s-Z") 'undo-tree-redo)
  (global-set-key (kbd "C-s-f") 'ns-toggle-fullscreen)
  ;; Emacs sometimes registers C-s-f as this weird keycode
  (global-set-key (kbd "<C-s-268632070>") 'ns-toggle-fullscreen))

;;;; Reveal in Finder
(use-package reveal-in-osx-finder
  :defer 2)

;;;; Get mac links from safari
(use-package grab-mac-link
  :defer 1)

(with-eval-after-load 'org-mac-link
  (defun org-mac-message-open (message-id)
    "Visit the message with MESSAGE-ID.
This will use the command `open' with the message URL."
    (start-process (concat "open message:" message-id) nil
                   "open" (concat "message://" (substring message-id 2) ""))))

;;;; Homebrew
(use-package homebrew
  :straight (homebrew :host github :repo "jdormit/homebrew.el")
  :commands
  (homebrew-install homebrew-upgrade homebrew-update homebrew-edit homebrew-info homebrew-package-info))

;;;; Security Keychain
;; Seehttps://www.reddit.com/r/emacs/comments/ew75ib/emacs_mu4e_and_mbsync_setup_for_fastmail_on_macos/fg23tcj?utm_source=share&utm_medium=web2x&context=3
(eval-after-load 'auth-source
  '(when (member window-system '(mac ns))
     (add-to-list 'auth-sources 'macos-keychain-internet)
     (add-to-list 'auth-sources 'macos-keychain-generic)))

;;;; OSX Lib
;; useful library of functions
;; https://melpa.org/#/osx-lib
(use-package osx-lib
  :straight (:type git :host github :repo "raghavgautam/osx-lib")
  :defer t)


;;; Provide
(provide 'lem-setup-macos)
;;; setup-macos.el ends here
