;;; lem-setup-org-base.el --- Org-mode settings -*- lexical-binding: t -*-

;; Author: Colin McLear
;; Maintainer: Colin McLear
;; Version: 0.1
;; Package-Requires: (dependencies)
;; Homepage: https://github.com/Lambda-Emacs/lambda-emacs
;; Keywords: org-mode


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

;; Basic setup for org-mode in 𝛌-Emacs

;;; Code:

;;;; Git Org
;; Use Org from source rather than built in
(use-package org
  :straight t ;; let straight handle this
  :commands (org-mode)
  :mode (("\\.org$" . org-mode))
  :bind
  (:map global-map
   ("C-c a" . org-agenda))
  (:map org-mode-map
   ("M-k" . org-metaup)
   ("M-j" . org-metadown)
   ("M-l" . org-metaright)
   ("M-h" . org-metaleft)
   ("M-J" . org-shiftdown)
   ("M-K" . org-shiftup)
   ("M-L" . org-shiftright)
   ("M-H" . org-shiftleft)
   ;; easily emphasize text
   ("s-b" . (lambda () (interactive) (org-emphasize-dwim ?*)))
   ("s-i" . (lambda () (interactive) (org-emphasize-dwim ?/)))
   ("s-M-`" . org-emphasize-with-verbatim-dwim)
   ("s-M-~" . org-emphasize-with-code-dwim)
   ;; better pasting behavior in org-mode
   ("s-v" . org-yank))
  (:map org-agenda-mode-map
   ("j" . org-agenda-next-item)
   ("k" . org-agenda-previous-item))

  :init
  ;; Org-Emphasis-Regex settings. Set regex boundaries for emphasis.
  ;; Load this before org-mode is loaded.
  ;; See https://emacs.stackexchange.com/q/54673/11934
  ;; https://emacs.stackexchange.com/q/54632/11934

  (setq org-emphasis-regexp-components
        '("-—[:space:]('\"{["
          "\] - [:space:].,:!?;'\")}\\["
          "[:space:]"
          "."
          1))

  :custom
  ;; Aesthetics & UI
  (org-adapt-indentation 'headline-data) ;; adapt indentation only for data lines
  (org-catch-invisible-edits 'smart) ;; prevent editing invisible area
  (org-cycle-separator-lines 0) ;; no empty lines in collapsed view
  (org-ellipsis "…") ;; nicer elipses "↷" "↴" "▼"
  (org-fontify-quote-and-verse-blocks t) ;; make quotes stand out
  (org-hide-emphasis-markers t)  ;; hide emph markers
  (org-hide-leading-stars t)  ;; hide leading stars
  (org-image-actual-width  500) ;; show all images at 500px using imagemagik
  (org-insert-heading-respect-content t) ;; insert new headings after subtree
  (org-list-allow-alphabetical t) ;; allow alphabetical list
  (org-pretty-entities t) ;; make latex look good, etc.
  (org-pretty-entities-include-sub-superscripts t) ;; prettify sub/superscripts
  (org-read-date-prefer-future 'time) ;; Incomplete dates refer to future dates & times
  (org-startup-folded t) ;; start org in outline
  (org-startup-indented nil) ;; don't start in indent mode
  (org-auto-align-tags nil) ;; don't auto-align tags
  (org-tags-column 0) ;; place tags directly next to headline text

  ;; Footnotes
  (org-footnote-section nil) ;; place footnotes locally
  (org-footnote-auto-adjust t) ;; renumber footnotes

  ;; Insertion/Yanking
  (org-M-RET-may-split-line '((default . t)))  ;; don't split line when creating a new headline, list item, or table field
  (org-yank-adjusted-subtrees t)  ;; adjust subtrees to depth when yanked
  (org-yank-folded-subtrees t) ;; fold subtrees on yank

  ;; Lists
  ;; Demote sequence for list bullets
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+")))
  (org-list-indent-offset 1) ;; increase sub-item indentation

  ;; Logging
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-state-notes-insert-after-drawers nil)
  (org-log-redeadline nil) ;; don't log the time a task was rescheduled/redeadlined.
  (org-log-reschedule nil)

  ;; Movement
  (org-return-follows-link t) ;; make RET follow links
  (org-special-ctrl-a/e t)  ;; better movement in headers

  ;; Searching
  (org-imenu-depth 8) ;; scan to depth 8 w/imenu
  (imenu-auto-rescan t) ;; make sure imenu refreshes

  ;; Source block settings
  (org-src-fontify-natively t) ;; use lang-specific fontification
  (org-src-window-setup 'other-window) ;; edit source in other window
  (org-src-tab-acts-natively t) ;; use lang bindings
  (org-confirm-babel-evaluate t) ;; confirm evaluation

  ;; TODOS
  (org-use-fast-todo-selection 'expert) ;; don't use popup window for todos

  :config
  ;; Setup further org config
  (require 'lem-setup-org-settings)
  (require 'lem-setup-org-extensions))

;;; Provide Org-Setup
  (provide 'lem-setup-org-base)
;;; lem-setup-org-base.el ends here
