;;; lem-setup-writing.el --- summary -*- lexical-binding: t -*-

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

;; Writing & related packages

;;; Code:
;;;; Spelling
(use-package ispell
  :ensure nil
  :commands (ispell-word ispell-region ispell-buffer)
  :config
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    ;; Please note ispell-extra-args contains ACTUAL parameters passed to aspell
    (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))))

(use-package flyspell
  :ensure nil
  :config
  (setq flyspell-abbrev-p t
        flyspell-use-global-abbrev-table-p t
        flyspell-issue-message-flag nil
        flyspell-issue-welcome-flag nil)
  :hook ((markdown-mode . flyspell-mode)
         (org-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode)))

;; completion of spellings
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
         ("C-;" . flyspell-correct-previous)
         ("C-:" . flyspell-correct-at-point))
  :custom
  (flyspell-correct-interface #'flyspell-correct-completing-read))

(with-eval-after-load 'hydra
  ;; keybinding is SPC-b S
  (defhydra hydra-spelling (:color blue)
    "
  ^
  ^Spelling^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^───────
  _q_ quit            _<_ previous        _c_ correction
  ^^                  _>_ next            _d_ dictionary
  ^^                  _f_ check           _m_ mode
  ^^                  ^^                  ^^
  "
    ("q" nil)
    ("<" flyspell-correct-previous :color pink)
    (">" flyspell-correct-next :color pink)
    ("c" ispell)
    ("d" ispell-change-dictionary)
    ("f" flyspell-buffer :color pink)
    ("m" flyspell-mode)))

;; Completion of misspelled words in buffer
(use-package consult-flyspell
  ;; :straight (consult-flyspell :type git :host gitlab
  ;;                             :repo "OlMon/consult-flyspell"
  ;;                             :branch "master")
  :after flyspell
  :config
  (setq consult-flyspell-set-point-after-word t
        consult-flyspell-always-check-buffer nil
        ;; Apply flyspell-correct-at-point directly after selecting candidate
        ;; and jump back to consult-flyspell.
        consult-flyspell-select-function
        (lambda () (flyspell-correct-at-point) (consult-flyspell))))



;;;;; Spelling Goto Next Error
(defun lem-flyspell-ispell-goto-next-error ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

;;;; Abbrev
(use-package abbrev
  :ensure nil
  ;; :straight (:type built-in)
  :defer 2
  :config
  ;; (add-hook 'text-mode-hook #'abbrev-mode)
  (setq abbrev-file-name (concat lem-var-dir "abbrev/.abbrev_defs")
        save-abbrevs 'nil)
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file)))

;;;; Capitalization
;; From https://karthinks.com/software/batteries-included-with-emacs/
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;;;; Markdown
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'"       . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :bind (:map markdown-mode-map
         ("s-*" . markdown-insert-list-item)
         ("s-b" . markdown-insert-bold)
         ("s-i" . markdown-insert-italic))
  :config
  (setq markdown-enable-math nil
        markdown-enable-wiki-links t
        markdown-nested-imenu-heading-index t
        markdown-open-command "~/bin/mark.sh"
        markdown-footnote-location 'immediately
        markdown-unordered-list-item-prefix "-   "
        markdown-header-scaling t
        markdown-use-pandoc-style-yaml-metadata t)
  (setq markdown-live-preview-window-function 'lem--markdown-live-preview-window-xwidget)

  (defun lem--markdown-live-preview-window-xwidget (file)
    "Preview file with xwidget browser"
    (xwidget-webkit-browse-url (concat "file://" file))
    (let ((buf (xwidget-buffer (xwidget-webkit-current-session))))
      (when (buffer-live-p buf)
        (and (eq buf (current-buffer)) (quit-window))
        (pop-to-buffer buf))))

  (defun lem--markdown-settings ()
    "settings for markdown mode"
    (progn
      (turn-on-flyspell)
      (auto-fill-mode)
      (hl-todo-mode)))

  ;; markdown hooks
  (add-hook 'markdown-mode-hook 'lem--markdown-settings)

  ;; for use with meow point movement
  (modify-syntax-entry ?@ "_" markdown-mode-syntax-table))

;; macro: delete backslashes in paragraph to cleanup markdown conversion
(fset 'lem-md-delete-backslash
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("\361\361f\\x" 0 "%d")) arg)))


;;;; Markdown TOC
(use-package markdown-toc
  :after markdown
  :hook (markdown-mode . markdown-toc))

;;;; Pandoc
(use-package pandoc-mode
  :commands (lem-pandoc-convert-to-pdf run-pandoc pandoc-convert-to-pdf)
  :config
  (setq pandoc-use-async t)
  ;; stop pandoc from just hanging forever and not completing conversion
  ;; see https://github.com/joostkremers/pandoc-mode/issues/44
  (setq pandoc-process-connection-type nil)
  (progn
    (defun run-pandoc ()
      "Start pandoc for the buffer and open the menu"
      (interactive)
      (pandoc-mode)
      (pandoc-main-hydra/body))
    (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

    (defun lem-pandoc-convert-to-pdf ()
      (interactive)
      (cond
       ((eq major-mode 'org-mode)
        (call-interactively 'org-pandoc-export-to-latex-pdf-and-open))
       (t
        (call-interactively 'pandoc-convert-to-pdf) (lem-pandoc-pdf-open))))

    (defun lem-pandoc-pdf-open ()
      "Open created PDF file"
      (interactive)
      (find-file-other-window (concat (file-name-sans-extension buffer-file-name) ".pdf"))))
  :init
  (progn
    (setq pandoc-data-dir (concat lem-etc-dir "pandoc-mode/"))
    ;; help pandoc find xelatex
    (setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin"))))

;;;; Writeroom
(use-package writeroom-mode
  :commands (writeroom-mode)
  :config
  (setq writeroom-fullscreen-effect 'maximized)
  (setq writeroom-width 95)
  (setq writeroom-mode-line t)
  (setq writeroom-bottom-divider-width 0))

;;;; Lorem Ipsum
(use-package lorem-ipsum
  :commands (Lorem-ipsum-insert-sentences Lorem-ipsum-insert-list Lorem-ipsum-insert-paragraphs)
  :config
  (lorem-ipsum-use-default-bindings))

;;;; Palimpsest (make archive)
(use-package palimpsest
  :diminish palimpsest-mode
  :hook ((markdown-mode org-mode) . palimpsest-mode)
  :custom
  (palimpsest-send-bottom "C-c C-z")
  (palimpsest-send-top "C-c C-a")
  :config
  (setq palimpsest-trash-file-suffix ".archive"))

;;;; Latex Packages
;; Basic settings
(use-package auctex
  :mode (("\\.tex\\'" . latex-mode)
         ("\\.latex\\'" . latex-mode))
  :commands (latex-mode LaTeX-mode plain-tex-mode)
  :init
  (progn
    (add-hook 'LaTeX-mode-hook #'LaTeX-preview-setup)
    (add-hook 'LaTeX-mode-hook #'flyspell-mode)
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (setq-default TeX-engine 'xetex)
    (setq TeX-auto-save nil
          TeX-parse-self nil
          TeX-save-query nil
          TeX-PDF-mode t)
    (setq-default TeX-master nil)))

(use-package preview
  :ensure nil
  :after auctex
  :commands LaTeX-preview-setup
  :init
  (progn
    (setq-default preview-scale 1.4
                  preview-scale-function '(lambda () (* (/ 10.0 (preview-document-pt)) preview-scale)))))

(use-package reftex
  :commands turn-on-reftex
  :init
  (setq reftex-plug-into-AUCTeX t))

(use-package bibtex
  :defer t
  :mode ("\\.bib" . bibtex-mode)
  :init
  (progn
    (setq bibtex-align-at-equal-sign t)
    (add-hook 'bibtex-mode-hook (lambda () (set-fill-column 120)))))


;; Auto-fill for LaTeX
(defun lem-latex-auto-fill ()
  "Turn on auto-fill for LaTeX mode."
  (turn-on-auto-fill)
  (set-fill-column 80)
  (setq default-justification 'left))
(add-hook 'LaTeX-mode-hook #'lem-latex-auto-fill)

;; Compilation command
(add-hook 'LaTeX-mode-hook (lambda () (setq compile-command "latexmk -pdflatex=xelatex -f -pdf %f")))

;; Prevent ispell from verifying some LaTeX commands
;; http://stat.genopole.cnrs.fr/dw/~jchiquet/fr/latex/emacslatex
(defvar lem-ispell-tex-skip-alists
  '("cite" "nocite"
    "includegraphics"
    "author" "affil"
    "ref" "eqref" "pageref"
    "label"))
(setq ispell-tex-skip-alists
      (list
       (append (car ispell-tex-skip-alists)
               (mapcar #'(lambda (cmd) (list (concat "\\\\" cmd) 'ispell-tex-arg-end)) lem-ispell-tex-skip-alists))
       (cadr ispell-tex-skip-alists)))

;; Indentation with align-current in LaTeX environments
(defvar lem-LaTeX-align-environments '("tabular" "tabular*"))
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (require 'align)
            (setq LaTeX-indent-environment-list
                  ;; For each item in the list...
                  (mapcar (lambda (item)
                            ;; The car is an environment
                            (let ((env (car item)))
                              ;; If this environment is in our list...
                              (if (member env lem-LaTeX-align-environments)
                                  ;; ...then replace this item with a correct one
                                  (list env 'align-current)
                                ;; else leave it alone
                                item)))
                          LaTeX-indent-environment-list))))

;; Use dvipdfmx to convert DVI files to PDF in AUCTeX
(eval-after-load 'tex
  '(add-to-list 'TeX-command-list
                '("DVI to PDF" "dvipdfmx %d" TeX-run-command t t) t))

;; SyncTeX (http://www.emacswiki.org/emacs/AUCTeX#toc19)
(defun synctex/un-urlify (fname-or-url)
  "A trivial function that replaces a prefix of file:/// with just /."
  (if (string= (substring fname-or-url 0 8) "file:///")
      (substring fname-or-url 7)
    fname-or-url))

;;;; Dictionary
(use-package define-word
  :commands (define-word define-word-at-point))

(use-package osx-dictionary
  ;; :straight (:type git :host github :repo "xuchunyang/osx-dictionary.el")
  :commands (osx-dictionary-search-word-at-point osx-dictionary-search-input))

;;;; Narrow/Widen
(defun lem-narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
  Dwim means: region, org-src-block, org-subtree, markdown
  subtree, or defun, whichever applies first. Narrowing to
  org-src-block actually calls `org-edit-src-code'.

  With prefix P, don't widen, just narrow even if buffer
  is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'markdown-mode)
         (markdown-narrow-to-subtree))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

;;bind this in the narrow keymap
(bind-key* "C-x n n" #'lem-narrow-or-widen-dwim narrow-map)

;;; Provide
(provide 'lem-setup-writing)

;;; lem-setup-writing.el ends here
