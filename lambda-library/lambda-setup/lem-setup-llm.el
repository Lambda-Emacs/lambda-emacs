;;; lem-setup-llm.el --- LLM (Large Language Model) integration -*- lexical-binding: t -*-

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

;; Integration with Large Language Models (LLMs) for coding assistance,
;; documentation, and general AI-powered features. Primary integration
;; with Claude via claudemacs package and agent-shell for LLM interactions.

;;; Code:

;;;; Define Custom Functions First
;; These need to be defined before the transient menus that reference them

;;;###autoload
(defun lem-claudemacs-create-project-context ()
  "Create a project-specific context file for Claude."
  (interactive)
  (let* ((project-root (if (project-current)
                           (project-root (project-current t))
                         default-directory))
         (context-file (expand-file-name "claude-context.md" project-root)))
    (unless (file-exists-p context-file)
      (with-temp-file context-file
        (insert "# Claude Project Context\n\n")
        (insert "## Project Overview\n")
        (insert "<!-- Describe your project here -->\n\n")
        (insert "## Key Technologies\n")
        (insert "<!-- List main technologies, frameworks, and libraries -->\n\n")
        (insert "## Coding Standards\n")
        (insert "<!-- Document your coding standards and conventions -->\n\n")
        (insert "## Important Notes\n")
        (insert "<!-- Any specific instructions for Claude -->\n\n")
        (insert "## Current Task\n")
        (insert "<!-- Describe what you're working on -->\n")))
    (find-file context-file)))

;;;###autoload
(defun lem-claudemacs-fix-error-with-context ()
  "Fix error at point with additional context."
  (interactive)
  (let ((error-context (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (thing-at-point 'line t))))
    (when error-context
      (claudemacs-execute-request
       (format "Fix this error and explain the solution:\n\n%s" error-context)))))

;;;###autoload
(defun lem-claudemacs-explain-code ()
  "Explain the selected code or code at point."
  (interactive)
  (let ((code (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (if code
        (claudemacs-execute-request
         (format "Explain this code in detail:\n\n```%s\n%s\n```"
                 (file-name-extension (buffer-file-name))
                 code))
      (message "No code selected or function at point"))))

;;;###autoload
(defun lem-claudemacs-optimize-code ()
  "Optimize the selected code."
  (interactive)
  (let ((code (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (if code
        (claudemacs-execute-request
         (format "Optimize this code for better performance and readability:\n\n```%s\n%s\n```"
                 (file-name-extension (buffer-file-name))
                 code))
      (message "No code selected or function at point"))))

;;;###autoload
(defun lem-claudemacs-generate-tests ()
  "Generate tests for the selected code."
  (interactive)
  (let ((code (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (if code
        (claudemacs-execute-request
         (format "Generate comprehensive test cases for this code:\n\n```%s\n%s\n```"
                 (file-name-extension (buffer-file-name))
                 code))
      (message "No code selected or function at point"))))

;;;###autoload
(defun lem-claudemacs-add-documentation ()
  "Add documentation to the selected code."
  (interactive)
  (let ((code (if (use-region-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'defun t))))
    (if code
        (claudemacs-execute-request
         (format "Add comprehensive documentation to this code:\n\n```%s\n%s\n```"
                 (file-name-extension (buffer-file-name))
                 code))
      (message "No code selected or function at point"))))

;;;###autoload
(defun lem-claudemacs-programming-setup ()
  "Set up claudemacs keybindings for programming modes."
  (local-set-key (kbd "C-c a e") #'lem-claudemacs-explain-code)
  (local-set-key (kbd "C-c a o") #'lem-claudemacs-optimize-code)
  (local-set-key (kbd "C-c a t") #'lem-claudemacs-generate-tests)
  (local-set-key (kbd "C-c a d") #'lem-claudemacs-add-documentation)
  (local-set-key (kbd "C-c a x") #'lem-claudemacs-fix-error-with-context))



;;;; Claudemacs Integration
(use-package claudemacs
  :vc (:url "https://github.com/cpoile/claudemacs")
  :commands (claudemacs-transient-menu
             claudemacs-switch-to-session
             claudemacs-start-menu
             claudemacs-fix-error-at-point
             claudemacs-execute-request
             claudemacs-add-file-reference
             claudemacs-unstick-terminal)
  :custom
  ;; Configure Claude Code executable path
  (claudemacs-program "claude")
  ;; Control buffer switching behavior
  (claudemacs-switch-to-buffer-on-create nil)
  ;; Prefer project.el over projectile (Lambda Emacs uses project.el)
  (claudemacs-prefer-projectile-root nil)
  :config
  ;; Ensure Claude Code CLI is available
  (unless (executable-find "claude")
    (warn "Claude Code CLI not found. Install from: https://claude.ai/download"))

  ;; Add to programming mode hook
  (add-hook 'prog-mode-hook #'lem-claudemacs-programming-setup)

  ;; Add C-c C-e keybinding to specific mode maps
  (with-eval-after-load 'prog-mode
    (define-key prog-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu))
  (with-eval-after-load 'emacs-lisp-mode
    (define-key emacs-lisp-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu))
  (with-eval-after-load 'text-mode
    (define-key text-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu))
  (with-eval-after-load 'python
    (define-key python-base-mode-map (kbd "C-c C-e") #'claudemacs-transient-menu)))

;;;; Custom Transient Menu for Claudemacs
;; NOTE: The package provides `claudemacs-transient-menu' with full functionality.
;; This custom menu adds Lambda Emacs specific commands.
(with-eval-after-load 'transient
  (transient-define-prefix lem-claudemacs-transient ()
    "Lambda Emacs Claude Code extensions."
    ["Lambda Emacs Claude Extensions"
     ["Analysis"
      ("a" "Explain code" lem-claudemacs-explain-code)
      ("o" "Optimize code" lem-claudemacs-optimize-code)
      ("t" "Generate tests" lem-claudemacs-generate-tests)
      ("d" "Add documentation" lem-claudemacs-add-documentation)]
     ["Context"
      ("E" "Fix error with context" lem-claudemacs-fix-error-with-context)
      ("p" "Create project context" lem-claudemacs-create-project-context)]]))

;;;; Org-mode Integration
(with-eval-after-load 'org
  (defun lem-claudemacs-org-summarize-subtree ()
    "Summarize the current org subtree using Claude."
    (interactive)
    (let* ((content (save-excursion
                      (org-back-to-heading t)
                      (let ((start (point)))
                        (org-end-of-subtree t t)
                        (buffer-substring-no-properties start (point))))))
      (claudemacs-execute-request
       (format "Summarize this org-mode content concisely:\n\n%s" content))))

  (defun lem-claudemacs-org-expand-topic ()
    "Expand on the current heading topic using Claude."
    (interactive)
    (let* ((heading (org-get-heading t t t t)))
      (claudemacs-execute-request
       (format "Write detailed content about: %s\n\nFormat as org-mode with appropriate subheadings." heading))))

  ;; Org-specific keybindings (use C-c C-c prefix to avoid conflicts with claude-code-ide)
  (define-key org-mode-map (kbd "C-c C-c s") #'lem-claudemacs-org-summarize-subtree)
  (define-key org-mode-map (kbd "C-c C-c e") #'lem-claudemacs-org-expand-topic))

;;;; Quick Action Menu
(with-eval-after-load 'transient
  (transient-define-prefix lem-claudemacs-quick-actions ()
    "Quick Claude actions."
    ["Quick Claude Actions"
     ["Common Tasks"
      ("e" "Explain code" lem-claudemacs-explain-code)
      ("f" "Fix error" lem-claudemacs-fix-error-with-context)
      ("o" "Optimize code" lem-claudemacs-optimize-code)
      ("d" "Add documentation" lem-claudemacs-add-documentation)]
     ["Generation"
      ("t" "Generate tests" lem-claudemacs-generate-tests)
      ("x" "Custom request" claudemacs-execute-request)]
     ["Session"
      ("s" "Switch to session" claudemacs-switch-to-session)
      ("S" "Start new session" claudemacs-start-menu)
      ("p" "Project context" lem-claudemacs-create-project-context)]])

  (global-set-key (kbd "C-c q") #'lem-claudemacs-quick-actions))

;;;; Claude Code IDE Integration
;; For project-aware coding with Pro subscription (no API key needed)
(use-package claude-code-ide
  :when (executable-find "claude")
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :commands (claude-code-ide 
             claude-code-ide-resume 
             claude-code-ide-continue
             claude-code-ide-switch-to-buffer
             claude-code-ide-list-sessions
             claude-code-ide-stop)
  :custom
  ;; Set the correct executable name
  (claude-code-ide-program "claude")            ; Use "claude" instead of "claude-code"

  ;; Core settings
  (claude-code-ide-auto-detect-project t)       ; Auto-detect project root
  (claude-code-ide-focus-on-open t)             ; Focus Claude window when opened

  ;; Window configuration
  (claude-code-ide-use-side-window t)           ; Use side window
  (claude-code-ide-window-side 'right)          ; Place on right side
  (claude-code-ide-window-width 80)             ; Window width

  ;; Terminal backend (use eat if available, fallback to vterm)
  (claude-code-ide-terminal-backend
   (if (featurep 'eat) 'eat 'vterm))

  ;; Integration settings
  (claude-code-ide-use-ide-diff t)              ; Use IDE diff viewer
  (claude-code-ide-diagnostics-backend 'auto)   ; Auto-detect diagnostics backend

  :config
  ;; Optional: Add custom system prompt for your workflow
  (setq claude-code-ide-system-prompt
        "You are helping with Emacs Lisp development. Follow elisp conventions and best practices.")

  :bind (("C-c c i" . claude-code-ide)          ; Start IDE session
         ("C-c c s" . claude-code-ide-switch-to-buffer) ; Switch to buffer
         ("C-c c p" . claude-code-ide-list-sessions) ; List/switch sessions
         ("C-c c r" . claude-code-ide-resume)   ; Resume session
         ("C-c c q" . claude-code-ide-stop)))    ; Stop session

;;;; Setup Helper Function
;;;###autoload
(defun lem-claudemacs-setup-check ()
  "Check if claudemacs and related tools are properly set up."
  (interactive)
  (let ((claude-cli (executable-find "claude"))
        (claude-code-cli (executable-find "claude-code"))
        (eat-available (featurep 'eat))
        (agent-shell-available (featurep 'agent-shell)))
    (message "=== LLM Setup Status ===")
    ;; Claude CLI
    (if claude-cli
        (message "✓ Claude CLI found at: %s" claude-cli)
      (message "✗ Claude CLI not found. Install from: https://claude.ai/download"))
    ;; Claude Code CLI
    (if claude-code-cli
        (message "✓ Claude Code CLI found at: %s" claude-code-cli)
      (message "ℹ Claude Code CLI not found. Install: npm install -g @anthropic-ai/claude-code"))
    ;; Eat terminal
    (if eat-available
        (message "✓ eat package is available")
      (message "ℹ eat package recommended for best terminal experience"))
    ;; Agent shell
    (if agent-shell-available
        (message "✓ agent-shell is available")
      (message "ℹ agent-shell not loaded"))
    (message "Setup complete. Use C-c C-e for claudemacs, C-c c i for Claude Code IDE.")))

(provide 'lem-setup-llm)
;;; lem-setup-llm.el ends here
