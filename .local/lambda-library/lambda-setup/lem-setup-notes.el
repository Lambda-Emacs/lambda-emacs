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

;; Configuration for note-taking with consult-notes and org-roam.

;;; Code:


;;; Notebook Setup

(defun lem-notebook ()
  (interactive)
  (let ((vertico-buffer-display-action
         '(display-buffer-reuse-window)))
    (consult-notes)))

;;; Remember Notes
;; Built in simple notes.
(use-package remember
  :straight (:type built-in)
  :commands (remember remember-notes)
  :config
  (setq remember-data-dir (concat lem-cache-dir "remember")
        remember-data-file (concat lem-cache-dir "remember/notes"))
  (unless (file-directory-p remember-data-dir)
    (make-directory remember-data-dir t)))

;;; Search Notes
;;;; Zettelkasten Search

(defvar lem-zettelkasten nil "Variable for zettelkasten search.")
(defun lem-zettelkasten-search ()
  "Search in Zettelkasten with consult-ripgrep."
  (interactive)
  (consult-ripgrep lem-zettelkasten))

;;;; Consult Notes
;; Easily search and select notes from multiple sources using consult. Notes can
;; be in principle added from any source that can be integrated with consult,
;; including org-roam.

(use-package consult-notes
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-all
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  ;; Set org-roam integration
  (consult-notes-org-roam-mode))

;;; Org Roam (Wiki & Notes)
;; Good notes package but a lot is still in flux
;; see https://org-roam.readthedocs.io/en/latest/

;;;; Org Roam
(use-package org-roam
  :straight (:host github :repo "org-roam/org-roam")
  ;; other bindings are under lem+notes-keys in keybindings.el
  :bind (:map org-mode-map
         ("C-M-i" . completion-at-point))
  :commands (lem-find-note-relation
             org-roam-node-find
             org-roam-node-insert
             org-roam-capture
             org-roam-buffer-toggle)
  :custom
  ;; Configure dirs
  (org-roam-db-location (concat org-roam-directory "org-roam.db"))
  (org-roam-completion-everywhere t)
  :init
  ;; No warnings
  (setq org-roam-v2-ack t)
  :config/el-patch
  ;; make sure slugs use hyphens not underscores
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char)
                                    (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s)
                                         (string-glyph-compose
                                          (apply #'string (seq-remove #'nonspacing-mark-p
                                                                      (string-glyph-decompose s)))))
                 (cl-replace (title pair)
                             (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(("[^[:alnum:][:digit:]]" . "-") ;; convert anything not alphanumeric
                        ("--*" . "-")                   ;; remove sequential underscores
                        ("^-"  . "")                     ;; remove starting underscore
                        ("-$"  . "")))                   ;; remove ending underscore
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))
  :config
  (org-roam-db-autosync-mode 1)

  ;; Org Roam Templating
  ;; see https://org-roam.readthedocs.io/en/latest/templating/
  (setq org-roam-capture-templates
        `(("z" "Zettel" plain "%?"
           :target (file+head "%<%Y-%m%d-%H%M>-${slug}.org"
                              ,(concat (concat "#+SETUPFILE:" hugo-notebook-setup-file) "\n#+HUGO_SECTION: zettel\n#+HUGO_SLUG: ${slug}\n#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"))
           :unnarrowed t)
          ("l" "Lecture" plain "%?"
           :target (file+head "lectures/%<%Y-%m%d-%H%M>-${slug}.org"
                              ,(concat (concat "#+SETUPFILE:" hugo-notebook-setup-file) "\n#+HUGO_SECTION: lectures\n#+HUGO_SLUG: ${slug}\n#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"))
           :unnarrowed t)
          ("p" "private" plain "%?"
           :target (file+head "private-${slug}.org"
                              "#+TITLE: ${title}\n#+DATE: %<%Y-%m%d-%H%M>"
                              :unnarrowed t))
          ("r" "reference note" plain "%?"
           :target (file+head "ref-notes/${citekey}.org"
                              ,(concat (concat "#+SETUPFILE:" hugo-notebook-setup-file) "\n#+TITLE: ${author-or-editor-abbrev} ${year}: ${title}\n#+hugo_section: reading-notes\n\n- tags :: \n- bookends link :: bookends://sonnysoftware.com/${beref}\n- pdf :: [[${file}][pdf link]]\n\n(lem-bibtex \"${citekey}\")"))
           :unnarrowed t))))

;;;; Org Roam UI (Server/Web App)
;; Visualize note connections
(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :commands (org-roam-ui-mode))

;;; Provide Setup-Notes
(provide 'lem-setup-notes)
;;; lem-setup-notes.el ends here
