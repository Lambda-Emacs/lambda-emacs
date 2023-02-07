;;; lem-setup-citation.el --- summary -*- lexical-binding: t -*-

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

;; Packages for academic citation. For discussion of what these various parts
;; mean see https://kristofferbalintona.me/posts/202206141852/ and
;; https://blog.tecosaur.com/tmio/2021-07-31-citations.html

;;; Code:

;;;; Citation Variables
(defcustom lem-bibliography nil "User bibliography for citation."
  :group 'lambda-emacs
  :tag "Lambda-Emacs User Bibliography")
(defcustom lem-bib-notes nil "User citation notes directory."
  :group 'lambda-emacs
  :tag "Lambda-Emacs Citation Notes Directory")
(defcustom lem-citar-note nil "Template for use with citar notes."
  :group 'lambda-emacs
  :tag "Lambda-Emacs Citar Notes Template")

;;;; Org-Cite
;; Eventually this should be a full replacement for org-ref
(use-package oc
  :ensure nil
  ;; :straight nil
  :after org
  :config
  (setq org-cite-global-bibliography `(,lem-bibliography))
  (setq org-cite-export-processors
        '((beamer csl)
          (latex csl)
          (t csl))))

;; Use csl
(use-package oc-csl
  ;; :straight nil
  :ensure nil
  :after oc
  :init
  ;; make sure to download csl
  ;; https://citationstyles.org
  ;; https://github.com/citation-style-language
  ;; repos for styles & locales
  (setq org-cite-csl-styles-dir "~/.local/share/csl/styles")
  (setq org-cite-csl-locales-dir "~/.local/share/csl/locales"))

;;;; Citeproc
(use-package citeproc
  ;; :straight (:host github :repo "andras-simonyi/citeproc-el")
  :after (oc oc-csl))

;;;; Citar
(use-package citar
  ;; :straight (:host github :repo "emacs-citar/citar")
  :commands (citar-open-beref
             citar-open-notes
             citar-insert-citation)
  :bind (:map citar-map
         ("b" .  #'citar-open-beref))
  :custom
  ;; Use with org citation
  (org-cite-global-bibliography `(,lem-bibliography))
  (citar-bibliography `(,lem-bibliography))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :config
  ;; use embark with at-point
  (setq citar-at-point-function 'embark-act)
  (setq citar-default-action 'citar-open-beref)
  ;; add beref entry for bookends
  (setq citar-additional-fields '("doi" "url" "beref"))
  (setq citar-templates
        `((main . " ${=key= id:15} ${title:48}")
          (suffix . "${author editor:30}  ${=type=:12}  ${=beref=:12} ${tags keywords:*}")
          (preview . "${author editor} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
          (note . ,lem-citar-note)))
  (when (display-graphic-p)
    (setq citar-symbols
          `((file ,(all-the-icons-octicon "file-pdf"      :face 'error) . " ")
            (note ,(all-the-icons-octicon "file-text"     :face 'warning) . " ")
            (link ,(all-the-icons-octicon "link-external" :face 'org-link) . " "))))
  ;; edit notes
  (setq citar-notes-paths `(,lem-bib-notes)))

(provide 'lem-setup-citation)
;;; lem-setup-citation.el ends here
