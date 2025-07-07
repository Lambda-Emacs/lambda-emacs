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
;; documentation, and general AI-powered features. Optimized for Claude
;; but supports multiple providers.

;;; Code:

;;;; GPTel - Universal LLM Client
(use-package gptel
  :ensure t
  :commands (gptel gptel-send gptel-menu)
  :bind (("C-c g g" . gptel)
         ("C-c g s" . gptel-send)
         ("C-c g m" . gptel-menu)
         ("C-c g r" . gptel-rewrite)
         ("C-c g a" . gptel-add)
         ("C-c g f" . gptel-fix-code)
         ("C-c g e" . gptel-explain-code)
         ("C-c g d" . gptel-document-code)
         ("C-c g t" . gptel-generate-tests))
  :custom
  ;; Default to Claude 3.5 Sonnet
  (gptel-model "claude-3-5-sonnet-20241022")
  (gptel-default-mode 'org-mode)
  ;; Save conversations
  (gptel-save-state t)
  :config
  ;; Configure Claude (Anthropic)
  (setq gptel-backend (gptel-make-anthropic "Claude"
                        :key (lambda () (getenv "ANTHROPIC_API_KEY"))
                        :stream t))
  
  ;; Alternative backends (uncomment to use)
  ;; OpenAI
  ;; (setq gptel-backend (gptel-make-openai "OpenAI"
  ;;                       :key (lambda () (getenv "OPENAI_API_KEY"))
  ;;                       :stream t))
  
  ;; Local models via Ollama
  ;; (setq gptel-backend (gptel-make-ollama "Ollama"
  ;;                       :host "localhost:11434"
  ;;                       :stream t
  ;;                       :models '("codellama" "mistral" "llama2")))

  ;; Directives for programming assistance
  (setq gptel-directives
        '((default . "You are a helpful AI assistant. Provide clear, accurate, and concise responses.")
          (code . "You are an expert programmer. Provide clean, efficient, and well-documented code following best practices. Explain your reasoning when making design decisions.")
          (emacs . "You are an Emacs expert. Provide Elisp code and configuration examples that follow Emacs conventions and best practices.")
          (explain . "You are a patient teacher. Explain the code or concept clearly, breaking down complex ideas into understandable parts.")
          (fix . "You are a debugging expert. Identify and fix issues in the code, explaining what was wrong and why your fix works.")
          (optimize . "You are a performance expert. Optimize the given code for efficiency while maintaining readability.")
          (document . "You are a technical writer. Add clear, comprehensive documentation including docstrings, comments, and usage examples.")
          (test . "You are a testing expert. Generate comprehensive test cases including edge cases and error conditions.")
          (review . "You are a code reviewer. Provide constructive feedback on code quality, potential bugs, and improvement suggestions.")))

  ;; Helper functions for common tasks
  (defun gptel-fix-code ()
    "Fix issues in selected code."
    (interactive)
    (let ((gptel--system-message (alist-get 'fix gptel-directives)))
      (gptel-send "Fix any issues in this code and explain the changes:")))

  (defun gptel-explain-code ()
    "Explain the selected code."
    (interactive) 
    (let ((gptel--system-message (alist-get 'explain gptel-directives)))
      (gptel-send "Explain this code in detail:")))

  (defun gptel-document-code ()
    "Add documentation to selected code."
    (interactive)
    (let ((gptel--system-message (alist-get 'document gptel-directives)))
      (gptel-send "Add comprehensive documentation to this code:")))

  (defun gptel-generate-tests ()
    "Generate tests for selected code."
    (interactive)
    (let ((gptel--system-message (alist-get 'test gptel-directives)))
      (gptel-send "Generate comprehensive test cases for this code:")))

  (defun gptel-optimize-code ()
    "Optimize selected code."
    (interactive)
    (let ((gptel--system-message (alist-get 'optimize gptel-directives)))
      (gptel-send "Optimize this code for better performance:")))

  (defun gptel-review-code ()
    "Review selected code."
    (interactive)
    (let ((gptel--system-message (alist-get 'review gptel-directives)))
      (gptel-send "Review this code and provide feedback:")))

  ;; Integration with org-mode
  (with-eval-after-load 'org
    ;; Create AI-assisted blocks
    (defun lem-org-ai-block ()
      "Create an org AI conversation block."
      (interactive)
      (insert "#+begin_ai\n")
      (insert "#+model: " (symbol-name gptel-model) "\n")
      (insert "#+temperature: 0.7\n")
      (insert "#+system: " (alist-get 'code gptel-directives) "\n\n")
      (insert "User: \n\n")
      (insert "Assistant: \n\n")
      (insert "#+end_ai\n")
      (forward-line -4)
      (end-of-line))))

;;;; Copilot Alternative - Codeium
(use-package codeium
  :disabled t  ;; Enable in personal config if desired
  ;; :ensure (:host github :repo "Exafunction/codeium.el")
  :init
  ;; Use M-Tab for completion
  (setq codeium-mode-line-enable
        (lambda (api) (not (memq api '(CancelRequest Heartbeat AcceptCompletion)))))
  :config
  (setq use-dialog-box nil)
  (setq codeium-api-enabled
        (lambda (api)
          (memq api '(GetCompletions Heartbeat CancelRequest GetAuthToken RegisterUser auth-redirect AcceptCompletion))))
  (defun my-codeium/document/text ()
    (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (min (+ (point) 1000) (point-max))))
  (defun my-codeium/document/cursor_offset ()
    (codeium-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq codeium/document/text 'my-codeium/document/text)
  (setq codeium/document/cursor_offset 'my-codeium/document/cursor_offset))

;;;; AI-Enhanced Features

;; Smart completion with context
(defun lem-ai-smart-complete ()
  "Use AI to provide context-aware completion."
  (interactive)
  (let* ((context (buffer-substring-no-properties
                  (max (point-min) (- (point) 500))
                  (min (point-max) (+ (point) 200))))
         (before-point (buffer-substring-no-properties
                       (max (point-min) (- (point) 50))
                       (point)))
         (prompt (format "Complete this code based on context:\n\nContext:\n%s\n\nComplete after: %s"
                        context before-point)))
    (gptel-request prompt
                   :callback (lambda (response info)
                              (when response
                                (insert response))))))

;; Generate docstring
(defun lem-ai-generate-docstring ()
  "Generate a docstring for the function at point."
  (interactive)
  (let ((func (thing-at-point 'defun t)))
    (if func
        (save-excursion
          (beginning-of-defun)
          (forward-line 1)
          (let ((prompt (format "Generate a comprehensive docstring for this Emacs Lisp function:\n\n%s" func)))
            (gptel-request prompt
                          :callback (lambda (response info)
                                     (when response
                                       (insert response)
                                       (insert "\n")
                                       (indent-region (point) (progn (forward-line -1) (point))))))))
      (message "No function at point"))))

;; Refactor with AI assistance
(defun lem-ai-refactor-region (start end)
  "Refactor the selected region with AI assistance."
  (interactive "r")
  (let* ((code (buffer-substring-no-properties start end))
         (mode (symbol-name major-mode))
         (prompt (format "Refactor this %s code to be cleaner and more efficient:\n\n%s" mode code)))
    (gptel-request prompt
                   :callback (lambda (response info)
                              (when response
                                (delete-region start end)
                                (goto-char start)
                                (insert response))))))

;; AI-powered error explanation
(defun lem-ai-explain-error ()
  "Explain the error at point using AI."
  (interactive)
  (let ((error-text (or (thing-at-point 'line t)
                       (error "No error at point"))))
    (gptel-request (format "Explain this error and suggest fixes:\n\n%s" error-text)
                   :callback (lambda (response info)
                              (when response
                                (with-output-to-temp-buffer "*AI Error Explanation*"
                                  (princ response)))))))

;;;; Integration with Programming Modes

(defun lem-setup-ai-programming-assistance ()
  "Set up AI assistance for programming modes."
  (local-set-key (kbd "C-c a c") #'lem-ai-smart-complete)
  (local-set-key (kbd "C-c a d") #'lem-ai-generate-docstring)
  (local-set-key (kbd "C-c a r") #'lem-ai-refactor-region)
  (local-set-key (kbd "C-c a e") #'lem-ai-explain-error))

;; Add to programming mode hooks
(add-hook 'prog-mode-hook #'lem-setup-ai-programming-assistance)

;;;; Org-mode AI Integration

(with-eval-after-load 'org
  ;; AI-assisted note generation
  (defun lem-org-ai-summarize-subtree ()
    "Summarize the current org subtree using AI."
    (interactive)
    (let* ((content (org-get-entry))
           (prompt (format "Summarize this org-mode content concisely:\n\n%s" content)))
      (gptel-request prompt
                     :callback (lambda (response info)
                                (when response
                                  (org-end-of-subtree)
                                  (insert "\n** Summary\n")
                                  (insert response)
                                  (insert "\n"))))))
  
  ;; Generate org-mode content
  (defun lem-org-ai-expand-topic ()
    "Expand on the current heading topic using AI."
    (interactive)
    (let* ((heading (org-get-heading t t t t))
           (prompt (format "Write detailed content about: %s" heading)))
      (gptel-request prompt
                     :callback (lambda (response info)
                                (when response
                                  (org-end-of-subtree)
                                  (insert "\n")
                                  (insert response)
                                  (insert "\n")))))))

(provide 'lem-setup-llm)
;;; lem-setup-llm.el ends here