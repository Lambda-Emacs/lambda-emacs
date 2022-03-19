;; Useful functions specifically for teaching or teaching-related work
;; Setup files for defined custom classes are in .local

;;; Org Latex PDF Process

;;; Org Latex Classes
(setq org-latex-classes '(("beamer" "\\documentclass[presentation]{beamer}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ("article" "\\documentclass[11pt]{article}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                          ("report" "\\documentclass[11pt]{report}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                          ("book" "\\documentclass[11pt]{book}"
                           ("\\part{%s}" . "\\part*{%s}")
                           ("\\chapter{%s}" . "\\chapter*{%s}")
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

                          ;; notes
                          ("org-notes" "\\documentclass[12pt]{article}
                           [NO-DEFAULT-PACKAGES]
                           [EXTRA]
                           \\input{/Users/roambot/.emacs.d/.local/custom-org-latex-classes/notes-setup-file.tex}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

                          ;; beamer handout
                          ("beamer-handout" "\\documentclass[12pt]{article}
                           [NO-DEFAULT-PACKAGES]
                           [EXTRA]
                           \\input{/Users/roambot/.emacs.d/.local/custom-org-latex-classes/handout-setup-file.tex}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                           ("\\paragraph{%s}" . "\\paragraph*{%s}")
                           ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))

                          ;; beamer presentation
                          ("beamer-presentation" "\\documentclass[presentation]{beamer}
                           [NO-DEFAULT-PACKAGES]
                           [PACKAGES]
                           \\usepackage{pgfpages}
                           [EXTRA]
                           \\setbeameroption{show notes on second screen=right}
                           \\setbeamertemplate{note page}{\\pagecolor{yellow!5}\\insertnote}
                           \\input{/Users/roambot/.emacs.d/.local/custom-org-latex-classes/unl-beamer-preamble.tex}"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))

                          ;; beamer slides only
                          ("beamer-slides-no-notes" "\\documentclass[handout]{beamer}
                           [NO-DEFAULT-PACKAGES]
                           [EXTRA]
                           \\setbeameroption{hidenotes}
                           \\input{/Users/roambot/.emacs.d/.local/custom-org-latex-classes/unl-beamer-preamble.tex}
                           [PACKAGES]"
                           ("\\section{%s}" . "\\section*{%s}")
                           ("\\subsection{%s}" . "\\subsection*{%s}")
                           ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;;; Slide Filter Notes
;; Allow reveal.js notes to work in beamer
;; Filter out notes in specified format backends
;; See
;; https://joonro.github.io/Org-Coursepack/Lectures/04%20Creating%20Content%20for%20Slides%20and%20Handouts.html#speaker-notes
(defun string/starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (and (string-match (rx-to-string `(: bos ,prefix) t) string) t))

(defun my/process-NOTES-blocks (text backend info)
  "Filter NOTES special blocks in export."
  (cond
   ;; Allow export of notes in latex
   ;; ((eq backend 'latex)
   ;;  (if (string/starts-with text "\\begin{NOTES}") ""))
   ((eq backend 'rst)
    (if (string/starts-with text ".. NOTES::") ""))
   ((eq backend 'html)
    (if (string/starts-with text "<div class=\"NOTES\">") ""))
   ((eq backend 'beamer)
    (let ((text (replace-regexp-in-string "\\\\begin{NOTES}" "\\\\note{" text)))
      (replace-regexp-in-string "\\\\end{NOTES}" "}" text)))
   ))

(eval-after-load 'ox '(add-to-list
                       'org-export-filter-special-block-functions
                       'my/process-NOTES-blocks))

;;; Slides
;; I got the tag based selective export idea from J Kitchin
;; https://kitchingroup.cheme.cmu.edu/blog/2013/12/08/Selectively-exporting-headlines-in-org-mode/

;; FIXME: Right now aync export doesn't work with let...

;;;; Org export to slides w/notes

(defun cpm/org-export-beamer-presentation ()
  "Export subtree to beamer PDF"
  (interactive)
  (let ((org-export-exclude-tags '("handout")))
    (save-excursion
      (goto-char (point-min))
      (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-presentation")))))

;; (org-open-file (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-presentation")))))


(defun cpm/org-export--file-beamer-presentation ()
  "Export file to beamer pdf"
  (interactive)
  (let ((org-export-exclude-tags '("noexport" "handout")))
    (save-excursion
      (goto-char (point-min))
      (org-beamer-export-to-pdf nil nil nil nil '(:latex-class "beamer-presentation")))))

;;;; Org export to slides w/o notes
(defun cpm/org-export-beamer-no-notes ()
  "Export org subtree slide content to useful custom style handout (PDF) form"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (org-open-file (org-beamer-export-to-pdf nil t nil nil '(:latex-class "beamer-slides-no-notes")))))

(defun cpm/org-export--file-beamer-no-notes ()
  "Export org file slide content to useful custom style handout (PDF) form"
  (interactive)
  (let ((org-export-exclude-tags '("slides")))
    (save-excursion
      (goto-char (point-min))
      (org-beamer-export-to-pdf t nil nil nil '(:latex-class "beamer-slides-no-notes")))))


;;; Handouts

(defun cpm/org-export-beamer-handout ()
  "Export subtree content to PDF handout. Handout uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b")
        (org-latex-compiler "xelatex")
        (org-export-exclude-tags '("slides")))
    (org-narrow-to-subtree)
    (save-excursion
      (goto-char (point-min))
      (org-latex-export-to-pdf nil t nil nil '(:latex-class "beamer-handout")))
    (widen)))

(defun cpm/org-export--file-beamer-handout ()
  "Export file content to PDF handout. Handout uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b")
        (org-latex-compiler "xelatex")
        (org-export-exclude-tags '("slides" "noexport")))
    (org-latex-export-to-pdf nil nil nil nil '(:latex-class "beamer-handout"))))

;;; Notes

;;;; Org to PDF Notes
(defun cpm/org-export-pdf-notes ()
  "Export subtree of notes to PDF file. Note uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b")
        (org-latex-compiler "xelatex")
        (org-export-exclude-tags '("noexport")))
    (org-narrow-to-subtree)
    (save-excursion
      (goto-char (point-min))
      (org-latex-export-to-pdf nil t nil nil '(:latex-class "org-notes")))
    (widen)))

(defun cpm/org-export--file-pdf-notes ()
  "Export file notes to PDF file. Note uses a distinctive quote style."
  (interactive)
  (let ((org-latex-default-quote-environment "quote-b")
        (org-latex-compiler "xelatex")
        (org-export-exclude-tags '("noexport")))
    (save-excursion
      (goto-char (point-min))
      (org-latex-export-to-pdf nil nil nil nil '(:latex-class "org-notes")))))

(defun cpm/cleanup-pdf-notes()
  "Move notes to static directory & cleanup other files"
  (interactive)
  (shell-command-to-string "
rm -f *.tex \
mv -f *.pdf static/materials/handouts/"))

;;; Org Tree Slides
(use-package org-tree-slide
  :straight t
  :bind (:map org-tree-slide-mode-map
         ("C-j" . org-tree-slide-move-next-tree)
         ("C-k" . org-tree-slide-move-previous-tree)
         ("C-s C-c" . org-tree-slide-content))
  :config
  (setq org-tree-slide-activate-message "Presentation mode ON")
  (setq org-tree-slide-deactivate-message "Presentation mode OFF")
  (setq org-tree-slide-breadcrumbs "    >    ")
  (setq org-tree-slide-content-margin-top 4)
  (setq org-tree-slide-skip-outline-level 4)
  (org-tree-slide-narrowing-control-profile)
  (setq org-tree-slide-skip-done nil)
  (setq org-tree-slide-modeline-display nil))

;;; Provide Teaching
(provide 'lem-setup-teaching)
