;;; lem-setup-programming.el --- Programming settings -*- lexical-binding: t -*-

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

;; Settings for better programming/coding. This includes delimiters, languages,
;; indentation, linting, documentation, and compilation.

;;; Code:

;;;; Show Pretty Symbols
(use-package prog-mode
  :straight (:type built-in)
  :defer t
  :custom
  ;; Show markup at point
  (prettify-symbols-unprettify-at-point t)
  :config
  ;; Pretty symbols
  (global-prettify-symbols-mode +1))

;;;; Delimiters & Identifiers
;;;;; Visualization of Delimiters (Rainbow Delimiters)
;; https://github.com/Fanael/rainbow-delimiters Useful package that will highlight
;; delimiters such as parentheses, brackets or braces according to their depth. Each
;; successive level is highlighted in a different color. This makes it easy to spot
;; matching delimiters, orient yourself in the code, and tell which statements are at
;; a given depth.
(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; https://github.com/Fanael/rainbow-identifiers Rainbow identifiers mode is an Emacs
;; minor mode providing highlighting of identifiers based on their names. Each
;; identifier gets a color based on a hash of its name.
(use-package rainbow-identifiers
  :commands rainbow-identifiers-mode)

;;;;; Surround & Change Delimiters

(use-package embrace
  :straight (:type git :host github :repo "cute-jumper/embrace.el")
  :bind (("C-M-s-#" . embrace-commander))
  :config
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (defun embrace-markdown-mode-hook ()
    (dolist (lst '((?* "*" . "*")
                   (?\ "\\" . "\\")
                   (?$ "$" . "$")
                   (?/ "/" . "/")))
      (embrace-add-pair (car lst) (cadr lst) (cddr lst))))
  (add-hook 'markdown-mode-hook 'embrace-markdown-mode-hook))

;;;;; Edit & Traverse Delimiters

(use-package puni
  :straight (:type git :host github :repo "AmaiKinono/puni")
  :hook ((prog-mode tex-mode org-mode markdown-mode
                    eval-expression-minibuffer-setup) . puni-mode))


;;;; Multiple Cursors
(use-package iedit
  :straight (:type git :host github :repo "victorhge/iedit")
  :bind (:map lem+search-keys
         ("c" . iedit-mode)))

;;;; Languages
;;;;; Applescript
(use-package applescript-mode
  :mode (("\\.scpt\\'" . applescript-mode)
         ("\\.applescript\\'"       . applescript-mode))
  :commands (applescript-mode))

;;;;; Elisp
;;;;;; Lisp Packages
(use-package lisp-mode
  :straight (:type built-in)
  :commands lisp-mode
  :straight nil)

(use-package emacs-lisp-mode
  :straight (:type built-in)
  :mode (("\\.el$" . emacs-lisp-mode))
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package eldoc
  :straight (:type built-in)
  :commands eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
  :diminish eldoc-mode
  :config
  ;; Show ElDoc messages in the echo area immediately, instead of after 1/2 a second.
  (setq eldoc-idle-delay 0))

;; better jump to definition
(use-package elisp-def
  :commands (elisp-def elisp-def-mode)
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-def-mode)))

;; Elisp hook
(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq show-trailing-whitespace t)
                                  (setq show-paren-context-when-offscreen t)
                                  (prettify-symbols-mode)
                                  (eldoc-mode)
                                  (yas-minor-mode)
                                  ;; (company-mode)
                                  (rainbow-delimiters-mode)))


;;;;;; Lisp Functions
;; idea from http://www.reddit.com/r/emacs/comments/312ge1/i_created_this_function_because_i_was_tired_of/
(defun lem-eval-current-form ()
  "Looks for the current def* or set* command then evaluates, unlike `eval-defun', does not go to topmost function"
  (interactive)
  (save-excursion
    (search-backward-regexp "(def\\|(set")
    (forward-list)
    (call-interactively 'eval-last-sexp)))

(defun lem-nav-find-elisp-thing-at-point-other-window ()
  "Find thing under point and go to it another window."
  (interactive)
  (let ((symb (variable-at-point)))
    (if (and symb
             (not (equal symb 0))
             (not (fboundp symb)))
        (find-variable-other-window symb)
      (find-function-at-point))))

;;;;;; Fix Parentheses

(defun lem-fix-lonely-parens ()
  "Move all closing parenthesis at start of indentation to previous line."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\s-*)" nil t)
      (delete-indentation))))

;;;;;; Elisp indentation
;; Fix the indentation of keyword lists in Emacs Lisp. See [1] and [2].
;;
;; Before:
;;  (:foo bar
;;        :baz quux)
;;
;; After:
;;  (:foo bar
;;   :bar quux)
;;
;; [1]: https://github.com/Fuco1/.emacs.d/blob/af82072196564fa57726bdbabf97f1d35c43b7f7/site-lisp/redef.el#L12-L94
;; [2]: http://emacs.stackexchange.com/q/10230/12534

(with-eval-after-load 'el-patch
  (el-patch-defun lisp-indent-function (indent-point state)
    "This function is the normal value of the variable `lisp-indent-function'.
The function `calculate-lisp-indent' calls this to determine
if the arguments of a Lisp function call should be indented specially.
INDENT-POINT is the position at which the line being indented begins.
Point is located at the point to indent under (for default indentation);
STATE is the `parse-partial-sexp' state for that position.
If the current line is in a call to a Lisp function that has a non-nil
property `lisp-indent-function' (or the deprecated `lisp-indent-hook'),
it specifies how to indent.  The property value can be:
* `defun', meaning indent `defun'-style
  (this is also the case if there is no property and the function
  has a name that begins with \"def\", and three or more arguments);
* an integer N, meaning indent the first N arguments specially
  (like ordinary function arguments), and then indent any further
  arguments like a body;
* a function to call that returns the indentation (or nil).
  `lisp-indent-function' calls this function with the same two arguments
  that it itself received.
This function returns either the indentation to use, or nil if the
Lisp function does not specify a special indentation."
    (el-patch-let (($cond (and (elt state 2)
                               (el-patch-wrap 1 1
                                 (or (not (looking-at "\\sw\\|\\s_"))
                                     (looking-at ":")))))
                   ($then (progn
                            (if (not (> (save-excursion (forward-line 1) (point))
                                        calculate-lisp-indent-last-sexp))
                                (progn (goto-char calculate-lisp-indent-last-sexp)
                                       (beginning-of-line)
                                       (parse-partial-sexp (point)
                                                           calculate-lisp-indent-last-sexp 0 t)))
                            ;; Indent under the list or under the first sexp on the same
                            ;; line as calculate-lisp-indent-last-sexp.  Note that first
                            ;; thing on that line has to be complete sexp since we are
                            ;; inside the innermost containing sexp.
                            (backward-prefix-chars)
                            (current-column)))
                   ($else (let ((function (buffer-substring (point)
                                                            (progn (forward-sexp 1) (point))))
                                method)
                            (setq method (or (function-get (intern-soft function)
                                                           'lisp-indent-function)
                                             (get (intern-soft function) 'lisp-indent-hook)))
                            (cond ((or (eq method 'defun)
                                       (and (null method)
                                            (> (length function) 3)
                                            (string-match "\\`def" function)))
                                   (lisp-indent-defform state indent-point))
                                  ((integerp method)
                                   (lisp-indent-specform method state
                                                         indent-point normal-indent))
                                  (method
                                   (funcall method indent-point state))))))
      (let ((normal-indent (current-column))
            (el-patch-add
              (orig-point (point))))
        (goto-char (1+ (elt state 1)))
        (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
        (el-patch-swap
          (if $cond
              ;; car of form doesn't seem to be a symbol
              $then
            $else)
          (cond
           ;; car of form doesn't seem to be a symbol, or is a keyword
           ($cond $then)
           ((and (save-excursion
                   (goto-char indent-point)
                   (skip-syntax-forward " ")
                   (not (looking-at ":")))
                 (save-excursion
                   (goto-char orig-point)
                   (looking-at ":")))
            (save-excursion
              (goto-char (+ 2 (elt state 1)))
              (current-column)))
           (t $else)))))))

;;;;; Haskell
(use-package haskell-mode
  :commands haskell-mode)

;;;;; HTML
(use-package web-mode
  :commands (web-mode)
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-enable-auto-pairing t
        web-mode-enable-auto-expanding t
        web-mode-enable-css-colorization t
        web-mode-enable-auto-closing t
        web-mode-enable-auto-quoting t))

;;;;; Lua
(use-package lua-mode
  :commands lua-mode
  :init
  (dolist (pattern '("\\.lua\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'lua-mode))))

;;;;; PHP
(use-package php-mode
  :commands php-mode
  :init
  (dolist (pattern '("\\.php\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'php-mode))))

;;;;; Shell Scripts
(use-package sh-script
  :commands sh-script-mode
  :init
  (progn
    ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
    (dolist (pattern '("\\.zsh\\'"
                       "zlogin\\'"
                       "zlogout\\'"
                       "zpreztorc\\'"
                       "zprofile\\'"
                       "zshenv\\'"
                       "zshrc\\'"))
      (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))))

(defun spacemacs//setup-shell ()
  (when (and buffer-file-name
             (string-match-p "\\.zsh\\'" buffer-file-name))
    (sh-set-shell "zsh")))
(add-hook 'sh-mode-hook 'spacemacs//setup-shell)

;;;;; YAML
(use-package yaml-mode
  :commands yaml-mode
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode))
  :config
  (add-hook 'yaml-mode-hook (lambda () (run-hooks 'prog-mode-hook))))

;;;;; Plist
(use-package plist-mode
  :straight nil
  :load-path "~/bin/lisp-projects/plist-mode"
  :commands (plist-mode))

;;;;; Vim
(use-package vimrc-mode
  :commands vimrc-mode)

;;;; Macrostep
;; https://github.com/joddie/macrostep Interactive macro expander for emacs
(use-package macrostep :commands macrostep-expand)

;;;; Documentation
(use-package tldr
  :commands (tldr tldr-update-docs)
  :init
  (with-eval-after-load 'evil
    (evil-set-initial-state 'tldr-mode 'emacs))
  :config
  (setq tldr-directory-path (expand-file-name "tldr/" lem-etc-dir)))

;;;; Indentation
(use-package aggressive-indent
  :preface
  (defun lem-aggressive-indent-mode-off ()
    (aggressive-indent-mode 0))
  :hook
  ((css-mode . aggressive-indent-mode)
   (emacs-lisp-mode . aggressive-indent-mode)
   (js-mode . aggressive-indent-mode)
   (lisp-mode . aggressive-indent-mode)
   (sgml-mode . aggressive-indent-mode))
  :config
  (setq-default aggressive-indent-comments-too t)
  (add-to-list 'aggressive-indent-protected-commands 'comment-dwim))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq-default highlight-indent-guides-method 'character
                highlight-indent-guides-character ?\│
                ;; default is \x2502 but it is very slow on Mac
                ;; highlight-indent-guides-character ?\xFFE8
                highlight-indent-guides-responsive 'top
                highlight-indent-guides-auto-odd-face-perc 5
                highlight-indent-guides-auto-even-face-perc 5
                highlight-indent-guides-auto-character-face-perc 15
                highlight-indent-guides-auto-enabled t))


;;;; Linting/Error Checking (Flymake)
;; Both Flycheck and Flymake are good linters, but let's stick with the built-in Flymake

(use-package flymake
  :straight (:type built-in)
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-suppress-zero-counters t)
  (flymake-start-on-flymake-mode t)
  (flymake-no-changes-timeout nil)
  (flymake-start-on-save-buffer t)
  (flymake-proc-compilation-prevents-syntax-check t)
  (flymake-wrap-around nil)
  ;; Customize mode-line
  (flymake-mode-line-counter-format '("" flymake-mode-line-error-counter flymake-mode-line-warning-counter flymake-mode-line-note-counter ""))
  (flymake-mode-line-format '(" " flymake-mode-line-exception flymake-mode-line-counters)))

;; Linting for emacs package libraries
(use-package package-lint
  :straight (:type git :host github :repo "purcell/package-lint")
  :commands (package-lint-batch-and-exit
             package-lint-current-buffer
             package-lint-buffer)
  :config
  (add-hook 'emacs-lisp-mode-hook #'package-lint-flymake-setup)
  ;; Avoid`package-not-installable' errors
  ;; See https://github.com/purcell/package-lint/issues/153
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'package-archive-contents)))

;; A collection of flymake backends
(use-package flymake-collection
  :straight t
  :hook (after-init . flymake-collection-hook-setup))

;; Use Consult with Flymake
(use-package consult-flymake
  :disabled
  :straight (:type git :host github :repo "minad/consult-flymake")
  :bind (:map lem+flymake-keys
         ("c" . consult-flymake)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Compiling

;;;;; Multi-Compile
(use-package multi-compile
  :commands (compile multi-compile-run)
  :config
  ;; Use for book compiling
  (defun string/starts-with (string prefix)
    "Return t if STRING starts with prefix."
    (and (stringp string) (string-match (rx-to-string `(: bos ,prefix) t) string)))
  (setq multi-compile-history-file (concat lem-cache-dir "multi-compile.cache"))
  (setq multi-compile-completion-system 'default)
  (setq multi-compile-alist '(
                              (org-mode . (
                                           ("pandoc-docx & Open" . "pandoc -s -C -f org+smart --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx   --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua -o %file-sans.docx %file-name && open %file-sans.docx")
                                           ("pandoc-pdf & Open" . "pandoc -s -C -f org+smart --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                           ("pandoc-beamer-compile-presentation" . "pandoc -C -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && pandoc --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name")
                                           ("pandoc-beamer & Open" . "pandoc -C -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && open %file-sans.pdf")
                                           ("pandoc-beamer-handout & Open" . "pandoc -C --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name && open %file-sans-handout.pdf")
                                           ))

                              ;; commands for pandoc
                              (markdown-mode . (
                                                ("pandoc-normalize" . "pandoc -f markdown -t markdown -s --id-prefix=%file-sans: --atx-headers --columns=85 --wrap=auto --reference-location=block -o %file-name %file-name")
                                                ("pandoc-sep-html & Open" . "pandoc -f markdown -t html4 -s --base-header-level=1 --number-sections --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/sep.html4 --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name && open %file-sans.html")
                                                ("pandoc-pdf & Open" . "pandoc -s -C --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ("pandoc-beamer-compile-presentation" . "pandoc -C -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && pandoc --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name")
                                                ("pandoc-beamer & Open" . "pandoc -C -i --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.beamer --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --metadata-file=/Users/roambot/dotfiles/pandoc/presentation-meta.yml --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -t beamer %file-name -o %file-sans.pdf && open %file-sans.pdf")
                                                ("pandoc-beamer-handout & Open" . "pandoc -C --slide-level=2 --pdf-engine=xelatex --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/pres-handout-meta.yml --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -o %file-sans-handout.pdf %file-name && open %file-sans-handout.pdf")
                                                ("pandoc-handout & Open" . "pandoc -s -C --pdf-engine=xelatex  --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --template=/Users/Roambot/.pandoc/pandoc-templates/tufte.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ("pandoc-docx & Open" . "pandoc -s -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.docx %file-name && open %file-sans.docx")
                                                ("pandoc-html & Open" . "pandoc -C -f markdown -t html5 -s --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/default.html5 --css=/Users/roambot/.pandoc/pandoc.css --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name && open %file-sans.html")
                                                ("pandoc-pdf" . "pandoc -s -C --pdf-engine=xelatex --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/Roambot/.pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name")
                                                ("pandoc-docx" . "pandoc -s -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --reference-doc=/Users/Roambot/.pandoc/custom-reference.docx --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.docx %file-name")
                                                ("pandoc-html" . "pandoc -f markdown -t html5 -s -C --bibliography=/Users/roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --template=/Users/roambot/.pandoc/pandoc-templates/default.html5 --css=/Users/roambot/.pandoc/pandoc.css --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.html %file-name")
                                                ("pandoc-handout" . "pandoc -s --pdf-engine=xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --template=/Users/Roambot/.pandoc/pandoc-templates/tufte.tex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name")
                                                ("test pdf" . "pandoc -s --pdf-engine=xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ("pandoc-letter-pdf & Open" . "pandoc -s --pdf-engine=xelatex -C --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --template=/Users/Roambot/dotfiles/pandoc/pandoc-templates/letter.tex -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ("pandoc-book-chapter-pdf & Open" . "pandoc -s -N --pdf-engine=xelatex --template=/Users/roambot/dotfiles/pandoc/pandoc-templates/default.latex --citeproc --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib --lua-filter=/Users/roambot/dotfiles/pandoc/cutsection.lua --lua-filter=/Users/roambot/dotfiles/pandoc/date.lua --lua-filter=/Users/roambot/dotfiles/pandoc/cuthead.lua --lua-filter=/Users/roambot/dotfiles/pandoc/promote-headers.lua --filter pandoc-latex-color --metadata=reference-section-title:'References' --metadata-file=/Users/roambot/dotfiles/pandoc/metadata.yml -o %file-sans.pdf %file-name && open %file-sans.pdf")
                                                ((string/starts-with buffer-file-name "/Users/roambot/Dropbox/Work/projects/Book-Projects/rationality-book/") . (("compile rationality book" . "cd %make-dir && make -k && open %make-dirbuild/pdf/kant-rationality-book.pdf"))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Compile with Nearest Makefile
;; See https://www.emacswiki.org/emacs/CompileCommand
(defun lem-upward-find-file (filename &optional startdir)
  "Move up directories until we find a certain filename. If we
  manage to find it, return the containing directory. Else if we
  get to the toplevel directory and still can't find it, return
  nil. Start at startdir or . if startdir not given"

  (let ((dirname (expand-file-name
                  (if startdir startdir ".")))
        (found nil) ; found is set as a flag to leave loop if we find it
        (top nil))  ; top is set when we get
                                        ; to / so that we only check it once

                                        ; While we've neither been at the top last time nor have we found
                                        ; the file.
    (while (not (or found top))
                                        ; If we're at / set top flag.
      (if (string= (expand-file-name dirname) "/")
          (setq top t))

                                        ; Check for the file
      (if (file-exists-p (expand-file-name filename dirname))
          (setq found t)
                                        ; If not, move up a directory
        (setq dirname (expand-file-name ".." dirname))))
                                        ; return statement
    (if found dirname nil)))

(defun lem-compile-next-makefile ()
  (interactive)
  (let* ((default-directory (or (lem-upward-find-file "Makefile") "."))
         (compile-command (concat "cd " default-directory " && "
                                  compile-command)))
    (compile compile-command)))



;;; Provide
(provide 'lem-setup-programming)
;;; lem-setup-programming.el ends here
