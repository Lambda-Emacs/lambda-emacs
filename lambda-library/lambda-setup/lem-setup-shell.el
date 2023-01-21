;;; lem-setup-shell.el --- shell setup               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear

;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: tools, lisp, terminals

;; Shell & General terminal settings

;;; Compilation
(use-package compile
  ;; :straight (:type built-in)
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

;;;; Toggle Vterm
;; I'm less interested in this for vterm than as using the project stuff for my vterm toggle function.
(defun lem-is-vterm-toggled ()
  "Checks if vterm is toggled."
  (let ((vterm-buffer-name nil)
        (result nil))
    (if (project-current)
        (setq vterm-buffer-name (concat "*vterm " (project-root (project-current)) "*"))
      (setq vterm-buffer-name (concat "*vterm " default-directory "*")))
    (dolist (elmnt (window-list) result)
      (if (string= vterm-buffer-name (buffer-name (window-buffer elmnt)))
          (setq result t)))
    result))

(defun lem-toggle-vterm ()
  "Toggle a mini vterm."
  (interactive)
  (let ((vterm-exists nil)
        (vterm-buffer-name nil)
        (vterm-buffer nil)
        (default-directory default-directory))

    (if (project-current)
        (setq vterm-buffer-name (concat "*vterm " (project-root (project-current)) "*")
              default-directory (project-root (project-current)))
      (setq vterm-buffer-name (concat "*vterm " default-directory "*")))

    (dolist (buffer (buffer-list) vterm-exists)
      (if (string= (buffer-name buffer) vterm-buffer-name)
          (progn (setq vterm-exists t)
                 (setq vterm-buffer buffer))))

    (cond ((and (get-buffer-window vterm-buffer-name)
                (derived-mode-p 'vterm-mode))
           (pop-to-buffer vterm-buffer-name)
           (delete-window))
          ((and (get-buffer-window vterm-buffer-name)
                (not (derived-mode-p 'vterm-mode)))
           (pop-to-buffer vterm-buffer-name))
          (t
           (vterm)))))

;;;; Run Command in Vterm
;; See https://www.reddit.com/r/emacs/comments/ft84xy/run_shell_command_in_new_vterm/
;; https://github.com/akermu/emacs-libvterm/pull/145

(defun lem-run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun lem-run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'lem-run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

;;; Virtualenvwrapper
(use-package virtualenvwrapper
  :after (:any vterm ansi-term)
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-vterm) ;; if you want vterm support
  (setq venv-project-home
        (expand-file-name (or (getenv "PROJECT_HOME") "~/Dropbox/Work/projects/")))
  (setq venv-location "~/bin/virtualenvs")
  (add-hook 'venv-postactivate-hook (lambda () (workon-venv)))
  (defun workon-venv ()
    "change directory to project in vterm"
    (vterm/cd (concat venv-project-home venv-current-name))))

;;; Tramp
;; An easy way to ssh
(use-package tramp
  ;; :straight nil
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

(use-package tramp-term :commands tramp-term)

;;; Provide Shell
(provide 'lem-setup-shell)
