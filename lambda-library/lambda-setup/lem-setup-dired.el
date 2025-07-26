;;; lem-setup-dired.el --- summary -*- lexical-binding: t -*-

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
;; Dired is the perfect filesystem navigator. See
;; https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer for discussion of
;; how to avoid creating lots of Dired buffers.

;;; Code:


;;;; Dired Settings
(use-package dired
  :ensure nil
  :commands (dired dired-jump dired-jump-other-window)
  :bind (:map dired-mode-map
         ("l" . dired-find-alternate-file)
         ("h" . lem-dired-updirectory))
  ;; "q" #'quit-window)
  :config
  ;; Function to move up a directory like in ranger
  (defun lem-dired-updirectory ()
    (interactive)
    (find-alternate-file ".."))

  ;; Configure directory listing program and switches
  ;; Use absolute path to gls since it might not be in exec-path during startup
  (let ((gls-path "/opt/homebrew/bin/gls"))
    (if (file-executable-p gls-path)
        (progn
          (message "Setting up gls for dired using absolute path...")
          (setq insert-directory-program gls-path)
          (setq dired-use-ls-dired t)
          (setq ls-lisp-use-insert-directory-program t)
          (setq dired-listing-switches "-laFh1v --group-directories-first")
          (message "Dired configured: insert-directory-program=%s, dired-listing-switches=%s" 
                   insert-directory-program dired-listing-switches))
      (message "gls not found at %s, using fallback dired settings" gls-path)
      (setq dired-use-ls-dired nil)
      (setq ls-lisp-use-insert-directory-program nil)))

  ;; Enhanced sorting functions when gls is available
  (when (file-executable-p "/opt/homebrew/bin/gls")
    (defun dired-sort-by-extension-and-name ()
      "Sort by extension, then name (groups file types together)."
      (interactive)
      (dired-sort-other "-laFh1vX --group-directories-first"))

    (defun dired-sort-by-time ()
      "Sort by modification time within extension groups."
      (interactive)
      (dired-sort-other "-laFh1vtX --group-directories-first"))

    (defun dired-sort-by-name-only ()
      "Sort alphabetically only (directories first)."
      (interactive)
      (dired-sort-other "-laFh1v --group-directories-first"))

    (defun dired-sort-by-size ()
      "Sort by file size within extension groups."
      (interactive)
      (dired-sort-other "-laFh1vSX --group-directories-first"))

    ;; Keybindings for sorting functions
    (define-key dired-mode-map (kbd "s e") 'dired-sort-by-extension-and-name)
    (define-key dired-mode-map (kbd "s t") 'dired-sort-by-time)
    (define-key dired-mode-map (kbd "s n") 'dired-sort-by-name-only)
    (define-key dired-mode-map (kbd "s z") 'dired-sort-by-size))
  ;; Like with ls, append "@" to file names if they're symlinks
  (setq dired-ls-F-marks-symlinks t)
  ;; don't ask about killing buffer visiting file
  (setq dired-clean-confirm-killing-deleted-buffers nil)
  ;; always delete and copy recursively
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq dired-deletion-confirmer 'y-or-n-p)
  (setq dired-dwim-target t)
  ;; allow editing file permissions
  (setq wdired-allow-to-change-permissions t)
  ;; open PDF files in external viewer
  (setq dired-guess-shell-alist-user '(("\.pdf$" . default)))
  ;; Allow dired, gnus, & mu4e to work together
  (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
  
)

;;;; Narrow Dired to Match Filter
(use-package dired-narrow
  :bind* (:map dired-mode-map
          ("/" . dired-narrow)))


;;;; Dired Colors
(use-package diredfl
  :hook (dired-mode . diredfl-global-mode))

;;;; Peep Dired
;; This package is no longer maintained
;; Perhaps switch to https://protesilaos.com/emacs/dired-preview
(use-package peep-dired
  :after dired
  :commands (peep-dired)
  :bind* (:map dired-mode-map
          ("P" . peep-dired)
          :map peep-dired-mode-map
          ("j"    . peep-dired-next-file)
          ("k"    . peep-dired-prev-file)
          ("RET"  . lem-peep-dired-open)
          ("TAB"  . lem-other-window))
  :config
  ;; helper function for opening files in full window
  (defun lem-peep-dired-open ()
    "open files from peep-dired & clean-up"
    (interactive)
    (peep-dired-kill-buffers-without-window)
    (dired-find-file)
    (delete-other-windows))
  (setq peep-dired-ignored-extensions '("mkv" "iso" "mp4" "pdf" "gif"))
  (setq peep-dired-cleanup-eagerly nil)
  (setq peep-dired-enable-on-directories t)
  (setq peep-dired-cleanup-on-disable t))

;;;; Dired Ranger
;; https://github.com/Fuco1/dired-hacks#dired-ranger
;; Very helpful way of copying/moving files
;; Note that to move first you need to copy the file and then go to the target directory and move
(use-package dired-ranger
  :after dired
  :bind (:map dired-mode-map
         ("s-c"  . dired-ranger-copy)
         ("s-m"  . dired-ranger-move)
         ("s-v"  . dired-ranger-paste)))

;;;; Cycle Dired Buffer
;;https://www.reddit.com/r/emacs/comments/qnthhw/comment/hjiv2uc/?utm_source=share&utm_medium=web2x&context=3
;; Allow for cycling from bottom to top of dired buffer and vice versa
(add-hook 'dired-mode-hook
          (defun lem-dired-wrap ()
            "Cycle from bottom to top of buffer"
            (make-local-variable 'post-command-hook)
            (add-hook 'post-command-hook
                      (defun lem-dired-wrap-1 ()
                        ""
                        (if (= 1 (save-excursion
                                   (forward-line)))
                            (goto-line 3))
                        (if (= -1 (save-excursion
                                    (forward-line -1)))
                            (goto-line (count-lines
                                        (point-min)
                                        (point-max))))))))

(provide 'lem-setup-dired)
;;; lem-setup-dired.el ends here
