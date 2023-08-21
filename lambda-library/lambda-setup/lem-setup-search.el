;;; lem-setup-search.el --- summary -*- lexical-binding: t -*-

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

;; Settings for effective search of buffers, files, and directories.

;;; Code:

;;; Search

;;;; Deadgrep
;; Deadgrep uses ripgrep for extremely fast text searches and provides a
;; separate buffer for results.
(use-package deadgrep
  :bind (:map lem+search-keys
         ("g" . deadgrep)))

;;;; Ripgrep
;; Ripgrep is a replacement for both grep like (search one file) and ag like
;; (search many files) tools. It's fast and versatile and written in Rust.
(use-package rg :commands rg)

;;;; Xref
;; Built-in library for cross-referencing
(use-package xref
  :ensure nil
  :defer 1)

;;;; Search and Replace
(use-package visual-regexp
  :commands (vr/query-replace)
  :config
  (use-package visual-regexp-steroids
    :commands (vr/select-query-replace)))


;;; Provide
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'lem-setup-search)
;;; lem-setup-search.el ends here
