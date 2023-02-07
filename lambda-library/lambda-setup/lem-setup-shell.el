;;; lem-setup-shell.el --- shell setup               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear

;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: tools, lisp, terminals

;; Shell & General terminal settings

;;; Compilation
(use-package compile
  :ensure nil
  :defer 2
  ;; Add recompile to project map
  :bind (:map project-prefix-map
         ("C" . recompile))
  :config
  (setq compilation-always-kill t  ;; kill compilation process before starting another
        compilation-ask-about-save nil ;; save all buffers on `compile'
        compilation-scroll-output 'first-error)
  ;; Automatically truncate compilation buffers so they don't accumulate too
  ;; much data and bog down the rest of Emacs.
  (autoload 'comint-truncate-buffer "comint" nil t)
  (add-hook 'compilation-filter-hook #'comint-truncate-buffer))

;;; Completion Buffer
;; Remove completion buffer when done
(add-hook 'minibuffer-exit-hook
          #'(lambda ()
              (let ((buffer "*Completions*"))
                (and (get-buffer buffer)
                     (kill-buffer buffer)))))

;;; Display Buffers for Shell Processes
;; See https://stackoverflow.com/a/47910509
(defun async-shell-command-no-window (command)
  "Don't pop up buffer for async commands"
  (interactive)
  (let ((display-buffer-alist
         (list
          (cons
           "\\*Async Shell Command\\*.*"
           (cons #'display-buffer-no-window nil)))))
    (async-shell-command command)))

;;; Exec Path
;; Fix path issues when launching from GUI
(use-package exec-path-from-shell
  :custom
  ;; Set this to nil only if you have your startup files set correctly.
  ;; See https://github.com/purcell/exec-path-from-shell#setting-up-your-shell-startup-files-correctly
  (exec-path-from-shell-arguments nil)
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;; Terminal
;;;; Settings
;; Some useful shell settings

;; Don't add newline in long lines
(setq-default term-suppress-hard-newline t)
;; Kill process buffers without query
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
;; Kill term-buffer on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; clickable links & no highlight of line
(defun lem-term-hook ()
  (progn
    (goto-address-mode)
    (hl-line-mode 0)
    (setq comint-buffer-maximum-size most-positive-fixnum)))

(add-hook 'term-mode-hook 'lem-term-hook)
(add-hook 'eshell-mode-hook 'lem-term-hook)

;; paste and navigation
(defun term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

;; Emacs doesn’t handle less well, so use cat instead for the shell pager
(setenv "PAGER" "cat")

;;;;; EAT (Emulate a terminal)
(use-package eat
  :config
  (setq eat-kill-buffer-on-exit t
        eat-enable-yank-to-terminal t
        eat-enable-directory-tracking t
        eat-enable-shell-command-history t
        eat-enable-shell-prompt-annotation t
        eat-term-scrollback-size nil)
  ;; For `eat-eshell-mode' -- integration with eshell.
  (add-hook 'eshell-load-hook #'eat-eshell-mode))

;;; Tramp
;; An easy way to ssh
(use-package tramp
  :ensure nil
  :defer 1
  :config
  (setq tramp-persistency-file-name (concat lem-cache-dir "tramp")
        ;; the most reliable tramp setup I have found (used at work every day...)
        tramp-default-method "ssh"
        tramp-copy-size-limit nil
        tramp-use-ssh-controlmaster-options nil))

;; I recommend the following ~/.ssh/config settings be used with the tramp settings in this cfg:
;; Host *
;; ForwardAgent yes
;; AddKeysToAgent yes
;; ControlMaster auto
;; ControlPath ~/.ssh/master-%r@%h:%p
;; ControlPersist yes
;; ServerAliveInterval 10
;; ServerAliveCountMax 10

;;; Provide Shell
(provide 'lem-setup-shell)
