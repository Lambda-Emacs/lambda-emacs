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
;; with Claude via claudemacs package.

;;; Code:

;;;; Define Custom Functions First
;; These need to be defined before the transient menus that reference them

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

(defun lem-claudemacs-fix-error-with-context ()
  "Fix error at point with additional context."
  (interactive)
  (let ((error-context (if (use-region-p)
                          (buffer-substring-no-properties (region-beginning) (region-end))
                        (thing-at-point 'line t))))
    (when error-context
      (claudemacs-execute-request 
       (format "Fix this error and explain the solution:\n\n%s" error-context)))))

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

(defun lem-claudemacs-programming-setup ()
  "Set up claudemacs keybindings for programming modes."
  (local-set-key (kbd "C-c a e") #'lem-claudemacs-explain-code)
  (local-set-key (kbd "C-c a o") #'lem-claudemacs-optimize-code)
  (local-set-key (kbd "C-c a t") #'lem-claudemacs-generate-tests)
  (local-set-key (kbd "C-c a d") #'lem-claudemacs-add-documentation)
  (local-set-key (kbd "C-c a x") #'lem-claudemacs-fix-error-with-context))

;;;; Claudemacs Integration
(use-package claudemacs
  :vc (:url "https://github.com/cpoile/claudemacs.git")
  :commands (claudemacs-transient-menu
             claudemacs-start-or-switch
             claudemacs-fix-error
             claudemacs-execute-request
             claudemacs-add-file-reference
             claudemacs-unstick-input)
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

;;;; Transient Menu for Claudemacs
(with-eval-after-load 'transient
  (transient-define-prefix claudemacs-transient ()
    "Claudemacs transient menu for Claude Code interactions."
    ["Claude Code"
     ["Session"
      ("s" "Start/Switch session" claudemacs-start-or-switch)
      ("u" "Unstick input" claudemacs-unstick-input)]
     ["Code Operations"
      ("e" "Fix error at point" claudemacs-fix-error)
      ("E" "Fix error with context" lem-claudemacs-fix-error-with-context)
      ("x" "Execute request" claudemacs-execute-request)]
     ["Analysis"
      ("a" "Explain code" lem-claudemacs-explain-code)
      ("o" "Optimize code" lem-claudemacs-optimize-code)
      ("t" "Generate tests" lem-claudemacs-generate-tests)
      ("d" "Add documentation" lem-claudemacs-add-documentation)]
     ["Context"
      ("f" "Add file reference" claudemacs-add-file-reference)
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
  
  ;; Org-specific keybindings
  (define-key org-mode-map (kbd "C-c c s") #'lem-claudemacs-org-summarize-subtree)
  (define-key org-mode-map (kbd "C-c c e") #'lem-claudemacs-org-expand-topic))

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
      ("p" "Project context" lem-claudemacs-create-project-context)]])
  
  (global-set-key (kbd "C-c q") #'lem-claudemacs-quick-actions))

;;;; Setup Helper Function
(defun lem-claudemacs-setup-check ()
  "Check if claudemacs is properly set up."
  (interactive)
  (let ((claude-available (executable-find "claude"))
        (eat-available (featurep 'eat)))
    (if claude-available
        (message "✓ Claude Code CLI found at: %s" claude-available)
      (message "✗ Claude Code CLI not found. Install from: https://claude.ai/download"))
    (if eat-available
        (message "✓ eat package is available")
      (message "ℹ eat package recommended for best terminal experience"))
    (message "Claudemacs setup complete. Use C-c C-e to start.")))

(provide 'lem-setup-llm)
;;; lem-setup-llm.el ends here
