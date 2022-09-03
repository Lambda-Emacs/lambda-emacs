;;; lem-setup-shell.el --- shell setup               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear

;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: tools, lisp, terminals

;; Shell & General terminal settings

;;; Compilation
(use-package compile
  :straight (:type built-in)
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

;;;; Display Buffers for Shell Processes
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

;;; Terminal
;;;; Settings
;; Sane settings for ansi-term
;;  Other useful shell settings
;; don't add newline in long lines
(setq-default term-suppress-hard-newline t)
;; kill process buffers without query
(setq kill-buffer-query-functions (delq 'process-kill-buffer-query-function kill-buffer-query-functions))
;; kill ansi-buffer on exit
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
(add-hook 'vterm-mode-hook 'lem-term-hook)

;; paste and navigation
(defun term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

;; Emacs doesn’t handle less well, so use cat instead for the shell pager
(setenv "PAGER" "cat")

;;;; Vterm
;; Better terminal function---much faster than ansi-term
(use-package vterm
  :commands (vterm vterm-other-window)
  :bind ((:map vterm-mode-map
          ;; fix issue with fzf
          ;; "C-c" #'vterm-send-C-c
          ("C-g" . #'vterm--self-insert)
          ("C-j" . #'vterm-send-down   )
          ("C-k" . #'vterm-send-up     )
          ("C-l" . #'vterm-clear       )
          ("s-v" . #'vterm-yank        )
          ("C-v" . #'vterm-yank        )
          ;; "<C-escape>" #'evil-collection-vterm-toggle-send-escape)
          ("<C-escape>" . #'lem-vterm-escape-toggle)))
  :custom (vterm-install t)
  :config
  (eval-when-compile
    (setq vterm-always-compile-module t))
  (setq vterm-keymap-exceptions nil)
  ;; set colors -- this is best with dark solarized right now
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-max-scrollback 100000)
  (setq ansi-color-names-vector
        ["#002833" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
  (setq vterm-term-environment-variable "xterm-256color"))

;; Escape to vim mode in shell
(defun lem-vterm-escape-toggle ()
  (interactive)
  ;; (evil-collection-vterm-toggle-send-escape)
  (vterm-send-key "<escape>"))

;; directory tracking
(defun vterm--rename-buffer-as-title (title)
  (let ((dir (string-trim-left (concat (nth 1 (split-string title ":")) "/"))))
    (cd-absolute dir)
    (rename-buffer (format "term %s" title) t)))
(add-hook 'vterm-set-title-functions 'vterm--rename-buffer-as-title)

;; vterm frame
(defun vterm-frame ()
  "Open a new terminal frame."
  (interactive)
  (let ((frame (selected-frame)))
    (with-selected-frame frame
      (progn
        (vterm)
        (set-frame-parameter frame 'name "terminal")))))

;;;; Multi-Vterm
(use-package multi-vterm
  :commands (multi-vterm
             multi-vterm-projectile
             multi-vterm-dedicated-toggle))

;;; Virtualenvwrapper
(use-package virtualenvwrapper
  :after (:any eshell sane-term ansi-term)
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  (setq venv-project-home
        (expand-file-name (or (getenv "PROJECT_HOME") "~/Dropbox/Work/projects/")))
  (setq venv-location "~/bin/virtualenvs")
  (add-hook 'venv-postactivate-hook (lambda () (workon-venv)))
  (defun workon-venv ()
    "change directory to project in eshell"
    (eshell/cd (concat venv-project-home venv-current-name))))

;;; Tramp
;; An easy way to ssh
(use-package tramp
  :straight nil
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

(use-package tramp-term :commands tramp-term)

;;; Provide Shell
(provide 'lem-setup-shell)
