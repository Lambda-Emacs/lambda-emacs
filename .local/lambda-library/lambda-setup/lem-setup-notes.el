;;; lem-setup-notes.el --- setup for notes -*- lexical-binding: t -*-

;; Author: Colin McLear
;; Maintainer: Colin McLear

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

;; Configuration for basic note-taking with denote and consult-notes

;;; Code:

;;;; Notebook Setup

(defcustom lem-notes-dir nil
  "Set a directory path for notes."
  :group 'lambda-emacs
  :type 'string)

;;;; Denote
;; Simple and deliberate note-taking with Denote
;; https://github.com/protesilaos/denote
;; See also https://systemcrafters.net/live-streams/july-15-2022/

(use-package denote
  :straight (:type git :host github :repo "protesilaos/denote")
  :custom
  (denote-file-type nil) ;; use org
  (denote-allow-multi-word-keywords nil) ;; single word keywords only
  ;; Better backlink display
  (denote-link-backlinks-display-buffer-action
   (quote ((display-buffer-reuse-window
            display-buffer-in-side-window)
           (inhibit-same-window . t)
           (side . bottom)
           (slot . 99)
           (window-height . 10)))))

;; Org-capture note creation with Denote
(with-eval-after-load 'org-capture
  (require 'denote-org-capture)
  (add-to-list 'org-capture-templates
               '("n" "New note (with Denote)" plain
                 (file denote-last-path)
                 #'denote-org-capture
                 :no-save t
                 :immediate-finish nil
                 :kill-buffer t
                 :jump-to-captured t)))

;; Example note creation function
(defun lem-denote-workbook-create-entry ()
  "Create an entry tagged 'workbook' with the date as its title."
  (interactive)
  (let ((folder
         (concat lem-notes-dir "workbook")))
    (denote
     (format-time-string "%Y %A %e %B")   ;; title format like Tuesday 14 June 2022
     '("workbook") ;; keyword
     nil ;; filetype
     (cond ((not (file-exists-p folder)) ;; make dir if doesn't exist
            (make-directory folder)
            folder)
           (t
            folder))))
  ;; insert headline
  (insert "* ")
  ;; insert time as part of headline)
  (lem-insert-time))

;;;; Consult Notes

(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes))

;;; Provide Setup-Notes
(provide 'lem-setup-notes)
;;; lem-setup-notes.el ends here
