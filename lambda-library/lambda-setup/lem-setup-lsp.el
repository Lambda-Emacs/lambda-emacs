;;; lem-setup-lsp.el --- LSP and modern programming configuration -*- lexical-binding: t -*-

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

;; Modern programming setup with LSP (Language Server Protocol) support
;; using built-in Eglot, optimized for Emacs 30 with tree-sitter integration.

;;; Code:

;;;; Eglot - Built-in LSP Client
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook ((;; Existing tree-sitter modes
          c-ts-mode
          c++-ts-mode
          cmake-ts-mode
          csharp-ts-mode
          css-ts-mode
          dockerfile-ts-mode
          go-ts-mode
          html-ts-mode
          java-ts-mode
          js-ts-mode
          json-ts-mode
          python-ts-mode
          ruby-ts-mode
          rust-ts-mode
          toml-ts-mode
          typescript-ts-mode
          yaml-ts-mode
          ;; Traditional modes as fallback
          c-mode
          c++-mode
          python-mode
          ruby-mode
          rust-mode
          js-mode
          typescript-mode) . eglot-ensure)
  :custom
  ;; Shutdown server when last buffer is closed
  (eglot-autoshutdown t)
  ;; Allow edits without confirmation
  (eglot-confirm-server-initiated-edits nil)
  ;; Reduce noise in echo area
  (eglot-events-buffer-size 0)
  ;; Disable progress reporter in minibuffer
  (eglot-report-progress nil)
  :config
  ;; Optimize performance
  (fset #'jsonrpc--log-event #'ignore)
  (setq-default eglot-events-buffer-size 0)
  
  ;; Configure keybindings
  (define-key eglot-mode-map (kbd "C-c l r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c l a") #'eglot-code-actions)
  (define-key eglot-mode-map (kbd "C-c l f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c l F") #'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-c l d") #'eglot-find-declaration)
  (define-key eglot-mode-map (kbd "C-c l i") #'eglot-find-implementation)
  (define-key eglot-mode-map (kbd "C-c l t") #'eglot-find-typeDefinition)
  (define-key eglot-mode-map (kbd "C-c l h") #'eglot-inlay-hints-mode)
  (define-key eglot-mode-map (kbd "C-c l =") #'eglot-reconnect))

;;;; Eldoc Integration
(use-package eldoc
  :ensure nil
  :custom
  ;; Configure eldoc for better LSP integration
  (eldoc-echo-area-use-multiline-p nil)
  (eldoc-echo-area-prefer-doc-buffer t)
  (eldoc-idle-delay 0.1))

;;;; Flymake Configuration for LSP
(use-package flymake
  :ensure nil
  :hook (eglot-managed-mode . flymake-mode)
  :custom
  (flymake-no-changes-timeout 0.5)
  (flymake-start-on-save-buffer t)
  :config
  ;; Show diagnostics at point
  (define-key flymake-mode-map (kbd "C-c ! l") #'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c ! p") #'flymake-show-project-diagnostics)
  (define-key flymake-mode-map (kbd "C-c ! n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "C-c ! p") #'flymake-goto-prev-error))

;;;; Tree-sitter Enhanced Features
(use-package treesit
  :ensure nil
  :custom
  ;; Use tree-sitter for better syntax highlighting
  (treesit-font-lock-level 4)
  :config
  ;; Auto-install grammars
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (lua "https://github.com/MunifTanjim/tree-sitter-lua")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))
  
  ;; Enhanced major mode remapping for Emacs 30
  (setq major-mode-remap-alist
        '((c-mode . c-ts-mode)
          (c++-mode . c++-ts-mode)
          (cmake-mode . cmake-ts-mode)
          (csharp-mode . csharp-ts-mode)
          (css-mode . css-ts-mode)
          (dockerfile-mode . dockerfile-ts-mode)
          (elixir-mode . elixir-ts-mode)
          (go-mode . go-ts-mode)
          (html-mode . html-ts-mode)
          (java-mode . java-ts-mode)
          (js-mode . js-ts-mode)
          (js-json-mode . json-ts-mode)
          (lua-mode . lua-ts-mode)
          (python-mode . python-ts-mode)
          (ruby-mode . ruby-ts-mode)
          (rust-mode . rust-ts-mode)
          (toml-mode . toml-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (yaml-mode . yaml-ts-mode))))

;;;; Completion Integration with LSP
(with-eval-after-load 'corfu
  ;; Enable LSP completion features
  (setq-default corfu-auto t
                corfu-auto-delay 0.1
                corfu-auto-prefix 2))

(with-eval-after-load 'cape
  ;; Add Cape completion functions for LSP
  (defun lem-setup-lsp-capf ()
    "Configure completion-at-point for LSP."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'cape-dabbrev
                       #'cape-file
                       #'cape-keyword))))
  
  (add-hook 'eglot-managed-mode-hook #'lem-setup-lsp-capf))

;;;; Project Integration
(use-package project
  :ensure nil
  :config
  ;; Better project detection for LSP
  (defun lem-project-try-lsp (dir)
    "Find project root by looking for LSP config files."
    (let ((lsp-files '(".lsp" ".ccls" "compile_commands.json" 
                       "compile_flags.txt" ".clangd" "tsconfig.json"
                       "package.json" "Cargo.toml" "go.mod" "pom.xml"
                       "build.gradle" "CMakeLists.txt" "Makefile")))
      (locate-dominating-file dir (lambda (d)
                                   (cl-some (lambda (f) 
                                             (file-exists-p (expand-file-name f d)))
                                           lsp-files)))))
  
  (add-to-list 'project-find-functions #'lem-project-try-lsp))

;;;; Language-specific LSP configurations

;; Python
(with-eval-after-load 'python
  ;; Use tree-sitter mode by default
  (setq python-mode-hook nil)
  (add-hook 'python-ts-mode-hook #'eglot-ensure)
  
  ;; Configure Python LSP servers (in order of preference)
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pylsp" "--"))))

;; JavaScript/TypeScript
(with-eval-after-load 'js
  (add-to-list 'eglot-server-programs
               '((js-mode js-ts-mode tsx-ts-mode typescript-mode typescript-ts-mode)
                 . ("typescript-language-server" "--stdio"))))

;; Rust
(with-eval-after-load 'rust-mode
  (add-to-list 'eglot-server-programs
               '((rust-mode rust-ts-mode) . ("rust-analyzer"))))

;; Go
(with-eval-after-load 'go-mode
  (add-to-list 'eglot-server-programs
               '((go-mode go-ts-mode) . ("gopls"))))

;; C/C++
(add-to-list 'eglot-server-programs
             '((c-mode c-ts-mode c++-mode c++-ts-mode)
               . ("clangd" "--header-insertion=never" "--completion-style=detailed")))

;;;; Debugging Support
(use-package dap-mode
  :disabled t  ;; Enable in personal config if needed
  :commands (dap-debug dap-debug-edit-template)
  :config
  (dap-auto-configure-mode 1))

(provide 'lem-setup-lsp)
;;; lem-setup-lsp.el ends here