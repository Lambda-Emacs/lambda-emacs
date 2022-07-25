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

;;;; lsp-mode
(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-keymap-prefix "C-x l")
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (read-process-output-max (* 1024 1024))
  (lsp-keep-workspace-alive nil)
  (lsp-eldoc-hook nil)
  :bind (:map lsp-mode-map ("C-c C-f" . lsp-format-buffer))
  :hook ((java-mode python-mode go-mode rust-mode
          js-mode js2-mode typescript-mode web-mode
          c-mode c++-mode objc-mode haskell-mode) . lsp-deferred)
  :init
    (add-to-list 'exec-path "~/.emacs.d/elixir-ls/release")
  :config
  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t)))

(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background nil))))
  (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
  :bind
  (:map lsp-ui-mode-map
        ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        ("C-c u" . lsp-ui-imenu)
        ("M-i" . lsp-ui-doc-focus-frame))
  (:map lsp-mode-map
        ("M-n" . forward-paragraph)
        ("M-p" . backward-paragraph))
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (when (display-graphic-p)
    (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil))
  ;; `C-g'to close doc
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide))

(use-package dap-mode
  :diminish
  :bind
  (:map dap-mode-map
        (("<f12>" . dap-debug)
         ("<f8>" . dap-continue)
         ("<f9>" . dap-next)
         ("<M-f11>" . dap-step-in)
         ("C-M-<f11>" . dap-step-out)
         ("<f7>" . dap-breakpoint-toggle))))


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

(use-package slime
  :ensure t
  :config
  (load (expand-file-name "~/.roswell/helper.el"))
  ;; $ ros config
  ;; $ ros use sbcl dynamic-space-size=3905
  ;; query with: (/ (- sb-vm:dynamic-space-end sb-vm:dynamic-space-start) (expt 1024 2))

  ;; set memory of sbcl to your machine's RAM size for sbcl and clisp
  ;; (but for others - I didn't used them yet)
  (defun linux-system-ram-size ()
    (string-to-number (shell-command-to-string "free --mega | awk 'FNR == 2 {print $2}'")))
  ;; (linux-system-ram-size)

  (setq inferior-lisp-program (concat "ros -Q dynamic-space-size=" (number-to-string (linux-system-ram-size)) " run"))

  ;; and for fancier look I personally add:
  (setq slime-contribs '(slime-fancy))

  ;; ensure correct indentation e.g. of `loop` form
  (add-to-list 'slime-contribs 'slime-cl-indent)

  ;; don't use tabs
  (setq-default indent-tabs-mode nil))

;; (setq slime-lisp-implementations `(("sbcl" ("ros use sbcl && ros run --" "--dynamic-space-size"
;;                                             ,(number-to-string (linux-system-ram-size))))
;;                                    ("clisp" ("ros use clisp && ros run --" "-m"
;;                                              ,(number-to-string (linux-system-ram-size))
;;                                              "MB"))
;;                                    ("ecl" ("ros use ecl && ros run --"))
;;                                    ("cmucl" ("ros use cmucl && ros run --"))))
;; -LispPac

;; SchemePac
(use-package geiser
  :straight t
  :config
  (setq geiser-active-implementations '(mit guile))
  ;; (setq geiser-chez-binary "chez")
  (setq geiser-mit-binary "/usr/bin/mit-scheme")
  (setq geiser-default-implementation 'mit)
  (add-hook 'scheme-mode-hook 'geiser-mode)
  (add-to-list 'auto-mode-alist
			   '("\\.sls\\'" . scheme-mode)
			   '("\\.sc\\'" . scheme-mode)))

(use-package geiser-mit
  :straight t
  :after geiser)

(defun geiser-save ()
  "Save geiser repl contents to input ring."
  (interactive)
  (geiser-repl--write-input-ring))

;;;; Clojure
(use-package flycheck-clj-kondo
  :ensure t)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))

(use-package cider
  :defer t
  :init
  (progn
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'clojurescript-mode-hook 'cider-mode)
    (add-hook 'clojurec-mode-hook 'cider-mode)
    (add-hook 'cider-repl-mode-hook 'cider-mode))
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-auto-mode nil))

;;;; Erlang
(use-package erlang
  :ensure t
  :defer t
  :if (executable-find "erl")
  :config
  (setq erlang-root-dir (expand-file-name "/usr/lib/erlang"))
  (require 'erlang-start))

;;;; Elixir
(use-package elixir-mode
  :ensure t
  :init
  (add-hook 'elixir-mode-hook
            (lambda ()
              (push '(">=" . ?\u2265) prettify-symbols-alist)
              (push '("<=" . ?\u2264) prettify-symbols-alist)
              (push '("!=" . ?\u2260) prettify-symbols-alist)
              (push '("==" . ?\u2A75) prettify-symbols-alist)
              (push '("=~" . ?\u2245) prettify-symbols-alist)
              (push '("<-" . ?\u2190) prettify-symbols-alist)
              (push '("->" . ?\u2192) prettify-symbols-alist)
              (push '("<-" . ?\u2190) prettify-symbols-alist)
              (push '("|>" . ?\u25B7) prettify-symbols-alist))))

(use-package reformatter
  :ensure t
  :config
                                        ; Adds a reformatter configuration called "+elixir-format"
                                        ; This uses "mix format -"
  (reformatter-define +elixir-format
                      :program "mix"
                      :args '("format" "-"))
                                        ; defines a function that looks for the .formatter.exs file used by mix format
  (defun +set-default-directory-to-mix-project-root (original-fun &rest args)
    (if-let* ((mix-project-root (and buffer-file-name
                                     (locate-dominating-file buffer-file-name
                                                             ".formatter.exs"))))
        (let ((default-directory mix-project-root))
          (apply original-fun args))
      (apply original-fun args)))
                                        ; adds an advice to the generated function +elxir-format-region that sets the proper root dir
                                        ; mix format needs to be run from the root directory otherwise it wont use the formatter configuration
  (advice-add '+elixir-format-region :around #'+set-default-directory-to-mix-project-root)
                                        ; Adds a hook to the major-mode that will add the generated function +elixir-format-on-save-mode
                                        ; So, every time we save an elixir file it will try to find a .formatter.exs and then run mix format from
                                        ; that file's directory
  (add-hook 'elixir-mode-hook #'+elixir-format-on-save-mode))

;;;; Elm
(use-package elm-mode
  :ensure t
  :if (executable-find "elm")
  :bind (:map elm-mode-map
         ("C-c C-d" . elm-oracle-doc-at-point))
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-to-list 'company-backends 'company-elm))

;;;; Go
(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  :bind (:map rust-mode-map ("C-c C-c" . rust-run))
  :config
  (use-package flycheck-rust
    :after flycheck
    :config
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

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

;;;; Ocaml
(use-package caml :defer t
  :config
  ;; tuareg: Major mode for editing OCaml code
  ;; https://github.com/ocaml/tuareg
  (use-package tuareg
    :mode (("\\.ml[ily]?$" . tuareg-mode)
           ("\\.topml$" . tuareg-mode))
    :init
    (add-hook 'tuareg-mode-hook (lambda ()
                                  (abbrev-mode -1)))

    ;; Make OCaml-generated files invisible to filename completion
    (dolist (ext '(".cmo" ".cmx" ".cma" ".cmxa" ".cmi" ".cmxs" ".cmt" ".cmti" ".annot"))
      (add-to-list 'completion-ignored-extensions ext))

    (with-eval-after-load 'smartparens
      ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
      (sp-local-pair 'tuareg-mode "'" nil :actions nil)
      (sp-local-pair 'tuareg-mode "`" nil :actions nil)))

  ;; merlin: Context sensitive completion for OCaml in Vim and Emacs
  ;; https://github.com/ocaml/merlin/tree/beta
  (use-package merlin
    :defer t
    :config
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (setq merlin-completion-with-doc t)

    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)

    (defun my-ocaml-mode-hook()
      (set (make-local-variable 'company-backends)
           '((merlin-company-backend company-files :with company-yasnippet)
             (company-dabbrev-code company-dabbrev))))
    (add-hook 'tuareg-mode-hook #'my-ocaml-mode-hook)
    (add-hook 'tuareg-mode-hook 'company-mode))

  ;; OCaml support for Flycheck using Merlin
  ;; https://github.com/flycheck/flycheck-ocaml
  (use-package flycheck-ocaml
    :config
    (with-eval-after-load 'merlin
      ;; Disable Merlin's own error checking
      (setq merlin-error-after-save nil)

      ;; Enable Flycheck checker
      (flycheck-ocaml-setup))
    (add-hook 'tuareg-mode-hook 'flycheck-mode))

  ;; utop is an improved toplevel for OCaml. It can run in a terminal or in
  ;; Emacs. It supports line editing, history, real-time and context sensitive
  ;; completion, colors, and more.
  ;; https://github.com/diml/utop
  (use-package utop :defer t
    :init
    (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
    (add-hook 'tuareg-mode-hook 'utop-minor-mode)
    :config
    (setq utop-command "opam config exec -- utop -emacs")
    )

  ;; ocp-indent: Indentation tool for OCaml, to be used from editors like Emacs
  ;; and Vim. http://www.typerex.org/ocp-indent.html
  ;; https://github.com/OCamlPro/ocp-indent
  (use-package ocp-indent :defer t
    :init
    (add-hook 'tuareg-mode-hook 'ocp-setup-indent)))

;;;; Python
(use-package elpy
  :init
  (add-to-list 'auto-mode-alist '("\\.py$" . python-mode))
  :bind (:map elpy-mode-map
	     ("<M-left>" . nil)
	     ("<M-right>" . nil)
	     ("<M-S-left>" . elpy-nav-indent-shift-left)
	     ("<M-S-right>" . elpy-nav-indent-shift-right)
	     ("M-." . elpy-goto-definition)
	     ("M-," . pop-tag-mark))
  :config
  (setq elpy-rpc-backend "jedi"))

(use-package pyenv-mode
  :init
  (add-to-list 'exec-path "~/.pyenv/shims")
  (setenv "WORKON_HOME" "~/.pyenv/versions/")
  :config
  (pyenv-mode)
  :bind
  ("C-x p e" . pyenv-activate-current-project))

(defun pyenv-activate-current-project ()
  "Automatically activates pyenv version if .python-version file exists."
  (interactive)
  (let ((python-version-directory (locate-dominating-file (buffer-file-name) ".python-version")))
    (if python-version-directory
        (let* ((pyenv-version-path (f-expand ".python-version" python-version-directory))
               (pyenv-current-version (s-trim (f-read-text pyenv-version-path 'utf-8))))
          (pyenv-mode-set pyenv-current-version)
          (message (concat "Setting virtualenv to " pyenv-current-version))))))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))  ; or lsp-deferred

;;;;; PHP
(use-package php-mode
  :commands php-mode
  :init
  (dolist (pattern '("\\.php\\'"))
    (add-to-list 'auto-mode-alist (cons pattern 'php-mode))))

;;;; R (ess)
(defun japhir/insert-r-pipe ()
  "Insert the pipe operator in R, |>"
  (interactive)
  (just-one-space 1)
  (insert "|>")
  (reindent-then-newline-and-indent))

(use-package ess
  ;; :load-path "/usr/share/emacs/site-lisp/ess"
  :init (require 'ess-site)  ;; seems like this is needed to load the minor modes as well keybindings don't work without it
  :hook (
         ((ess-r-mode inferior-ess-r-mode) . electric-layout-mode)
         ;; (ess-r-post-run . (lambda ()
         ;;    (ess-load-file (make-temp-file nil nil nil
         ;;                                "Sys.setenv(\"DISPLAY\"=\":0.0\")")))
         )
  :commands R
  :bind (:map ess-r-mode-map
         (";" . ess-insert-assign)
         ;; RStudio equivalents
         ("M--" . ess-insert-assign)
         ("C-S-m" . japhir/insert-r-pipe)
         :map inferior-ess-r-mode-map
         (";" . ess-insert-assign)
         ("M--" . ess-insert-assign)
         ("C-S-m" . japhir/insert-r-pipe))
  :config
  (defun my-org-confirm-babel-evaluate (lang body)
    (not (or (string= lang "R")
             (string= lang "python")
             (string= lang "elisp")
             (string= lang "emacs-lisp")
             (string= lang "julia")
             (string= lang "latex"))))
  :custom
  ;; display-buffer-alist
  ;; '(("*R Dired*"
  ;;    (display-buffer-reuse-window display-buffer-in-side-window)
  ;;    (side . right)
  ;;    (slot . -1)
  ;;    (window-width . 0.33))
  ;;   ("*R:"
  ;;    (display-buffer-reuse-window display-buffer-in-side-window)
  ;;    (slot . 2)
  ;;    (window-width . 0.5))
  ;;   ("*Help*"
  ;;    (display-buffer-reuse-window display-buffer-in-side-window)
  ;;    (side . right)
  ;;    (slot . 1)
  ;;    (window-width . 0.33)))
  ;; ess-help-own-frame 'one
  ;; ess-auto-width 'window
  (org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
  (ess-style 'RStudio)
  (ess-use-auto-complete nil)
  (ess-use-company t)
  (ess-indent-with-fancy-comments nil)
  (ess-pdf-viewer-pref 'emacsclient)
  (inferior-R-args "--no-restore-history --no-save")
  (ess-ask-for-ess-directory nil)
  (ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-R-fl-keyword:fun-cals . t)
     (ess-R-fl-keyword:numbers)
     (ess-R-fl-keyword:operators . t)
     (ess-R-fl-keyword:delimiters)
     (ess-R-fl-keyword:=)
     (ess-R-fl-keyword:F&T)))))

(use-package ess-view
  :after ess)

;; Put spaces around operators such as +, -, etc.
(use-package electric-operator
  :hook ((R-mode ess-r-mode inferior-ess-r-mode) . electric-operator-mode)
  :config
  (electric-operator-add-rules-for-mode 'stan-mode
                                        (cons "," ", ")
                                        (cons "~" " ~ "))

  (electric-operator-add-rules-for-mode 'ess-r-mode
                                        (cons ".+" " . + ")
                                        ;; these should never have spacing around them
                                        (cons ":" ":") ;; for ranges, should not add space
                                        (cons "::" "::") ;; to call a function from a package
                                        (cons ":::" ":::") ;; to call an internal function from a package
                                        (cons ":=" " := ") ;; walrus operator
                                        (cons "? " "?")
                                        (cons "){" ") {")
                                        (cons "}else{" "} else {")
                                        (cons "for(" "for (")
                                        (cons "if(" "if (")
                                        (cons "while(" "while (")
                                        (cons "{{" " {{ ") ;; curly-curly tidyverse
                                        (cons "}}" " }} ")
                                        (cons "!!" " !!")
                                        (cons "!!!" " !!!")
                                        (cons "^" "^") ;;
                                        (cons "|>" " |> ") ;; r 4.0 built-in pipe
                                        )
  :custom
  (electric-operator-R-named-argument-style 'spaced))

;; polymode for working with .Rmd files etc.
(use-package polymode :defer t)
(use-package poly-markdown :defer t)

;;;; Ruby
(use-package rbenv
  :hook (after-init . global-rbenv-mode)
  :init (setq rbenv-show-active-ruby-in-modeline nil
              rbenv-executable "rbenv"))

;; Run a Ruby process in a buffer
(use-package inf-ruby
  :hook ((ruby-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter)))

;; Ruby YARD comments
(use-package yard-mode
  :diminish
  :hook (ruby-mode . yard-mode))

;; Ruby refactoring helpers
(use-package ruby-refactor
  :diminish
  :hook (ruby-mode . ruby-refactor-mode-launch))

;; Yet Another RI interface for Emacs
(use-package yari
  :bind (:map ruby-mode-map ([f1] . yari)))

;; RSpec
(use-package rspec-mode
  :diminish
  :commands rspec-install-snippets
  :hook (dired-mode . rspec-dired-mode)
  :config (with-eval-after-load 'yasnippet
            (rspec-install-snippets)))

;; Rails
(use-package projectile-rails
  :diminish
  :hook (projectile-mode . projectile-rails-global-mode))

;;;; Rust
(use-package rust-mode
  :mode "\\.rs\\'"
  :custom
  (rust-format-on-save t)
  :bind (:map rust-mode-map ("C-c C-c" . rust-run))
  :config
  (use-package flycheck-rust
    :after flycheck
    :config
    (with-eval-after-load 'rust-mode
      (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

;;;; Scala
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

;; Enable sbt mode for executing sbt commands
(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false"))
  )

(use-package lsp-metals)

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

;;;; AsciidocPac
(use-package adoc-mode
  :ensure t
  :defer 110
  :config
  (add-to-list
   'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
  )

;;;; ReStructuredText
(require 'rst)
(setq auto-mode-alist
      (append '(("\\.rst$" . rst-mode)) auto-mode-alist))

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
  :custom
  (multi-compile-history-file (concat lem-cache-dir "multi-compile.cache"))
  (multi-compile-completion-system 'default)
  :config
  ;; Use for book compiling
  (defun string/starts-with (string prefix)
    "Return t if STRING starts with prefix."
    (and (stringp string) (string-match (rx-to-string `(: bos ,prefix) t) string))))




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
