;;; lem-setup-eshell.el --- eshell setup             -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear
;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: terminals, lisp, emulations

;;; Commentary
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
;; Eshell is an elisp shell. It has its own configuration parameters, distinct
;; from those of shell or ansi-terminal. It is closer to something like bash or
;; zsh than it is to a terminal emulator like vterm

;;; Eshell Settings
(use-package esh-mode
  :straight (:type built-in)
  :after eshell
  :custom
  (eshell-directory-name (concat lem-etc-dir "eshell/"))
  ;; auto truncate after 20k lines
  (eshell-buffer-maximum-lines 20000)
  (eshell-scroll-to-bottom-on-input 'all)
  (eshell-scroll-to-bottom-on-output 'all))

(use-package em-dirs
  :straight (:type built-in)
  :after eshell
  :custom
  (eshell-list-files-after-cd t)
  (eshell-last-dir-ring-file-name (concat lem-etc-dir "eshell/lastdir"))
  :config
  ;; https://github.com/dakra/dmacs/blob/master/init.org#eshell
  (defun eshell/rcd (&optional directory)
    "Like regular 'cd' but don't jump out of a tramp directory.
When on a remote directory with tramp don't jump 'out' of the server.
So if we're connected with sudo to 'remotehost'
'$ rcd /etc' would go to '/sudo:remotehost:/etc' instead of just
'/etc' on localhost."
    (unless (file-remote-p default-directory)
      (error "not in a remote location"))
    (with-parsed-tramp-file-name default-directory nil
      (eshell/cd
       (tramp-make-tramp-file-name
        method user nil host nil (or directory "") hop)))))

(use-package em-alias
  :straight (:type built-in)
  :after eshell
  :custom
  (eshell-aliases-file (concat lem-etc-dir "eshell/alias")))

(use-package em-cmpl
  :straight (:type built-in)
  :after eshell
  :custom
  (eshell-cmpl-ignore-case t)
  (eshell-cmpl-cycle-completions t))

(use-package em-prompt
  :straight (:type built-in)
  :after eshell
  :custom
  (eshell-highlight-prompt t)
  (eshell-prompt-regexp "^[^λ]+ λ "))

(use-package em-term
  :straight (:type built-in)
  :after eshell
  :custom
  ;; Let buffer linger, but can easily quit since view-mode
  ;; is enabled in term-exec-hook above.
  (eshell-destroy-buffer-when-process-dies nil))

;; FIXME: Need to understand this one better!
;; (use-package em-smart :straight
;;   (:type built-in)
;;   :hook ((eshell-mode . eshell-smart-initialize)))

(use-package em-banner
  :straight (:type built-in)
  :after eshell
  :custom
  (eshell-banner-message ""))

;; Alternative banners
;; eshell-banner-message (message "Emacs initialized in %.2fs \n\n" (float-time (time-subtract (current-time) my-start-time)))

(use-package em-hist
  :straight (:type built-in)
  :after eshell
  :custom
  (eshell-history-file-name (concat lem-etc-dir "eshell/history"))
  (eshell-history-size (* 10 1024))
  (eshell-hist-ignoredups t))

(use-package em-glob
  :straight (:type built-in)
  :after eshell
  :custom
  (eshell-glob-case-insensitive t)
  (eshell-error-if-no-glob t))

(use-package em-term
  :straight (:type built-in)
  :after eshell
  :config
  ;; Visual commands
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
  (add-to-list 'eshell-visual-commands '("ranger" "vi" "screen" "top" "less" "more" "lynx"
                                         "ncftp" "pine" "tin" "trn" "elm" "vim"
                                         "nmtui" "alsamixer" "htop" "el" "elinks"
                                         "tail" "top" "nano" "ssh")))

;;; Eshell Pcomplete

(use-package pcmpl-homebrew
  :after eshell
  :straight t)

(use-package pcmpl-git
  :after eshell
  :straight t)

(use-package pcmpl-args
  :after eshell
  :straight t)

(use-package pcomplete-extension
  :after eshell
  :straight t)

(use-package esh-help
  :after eshell
  :straight t
  :config
  ;; Eldoc support.
  (setup-esh-help-eldoc))

(use-package eshell-up
  :straight t
  :config
  (defalias 'eshell/up #'eshell-up))

(defun lem-setup-eshell ()
  (interactive)
  ;; Use imenu to jump prompts
  ;; https://xenodium.com/imenu-on-emacs-eshell/
  (setq-local imenu-generic-expression
              '(("Prompt" " λ \\(.*\\)" 1)))
  ;; turn off semantic-mode in eshell buffers
  (semantic-mode -1)
  ;; turn off hl-line-mode
  (hl-line-mode -1))

(add-hook 'eshell-mode-hook #'lem-setup-eshell)

;;; Eshell Prompt
;; See http://www.modernemacs.com/post/custom-eshell/
;; https://github.com/zwild/eshell-prompt-extras
;; https://www.bytedude.com/custom-eshell-prompts/
;; https://github.com/xenodium/dotsies/tree/main/emacs
;; https://www.emacswiki.org/emacs/EshellPrompt
;; https://github.com/howardabrams/dot-files/blob/master/emacs-eshell.org

;; Prompt char
(defun lem-eshell-config--prompt-char ()
  "Return shell character."
  (format "%s" (if (= (user-uid) 0)
                   "#"
                 "λ")))

;; Implement a "prompt number" section
(defvar eshell-prompt-number 0
  "Set a prompt number for eshell.")
(add-hook 'eshell-exit-hook (lambda () (setq eshell-prompt-number 0)))
(advice-add 'eshell-send-input :before
            (lambda (&rest args) (setq eshell-prompt-number (+ 1 eshell-prompt-number))))

(defun lem-eshell-config--git-prompt (pwd)
  "Returns current git branch as a string, or the empty string if
PWD is not in a git repo (or the git command is not found)."
  (interactive)
  (when (and (not (file-remote-p pwd))
             (eshell-search-path "git")
             (locate-dominating-file pwd ".git"))
    (let* ((git-url (shell-command-to-string "git config --get remote.origin.url"))
           (git-repo (file-name-base (s-trim git-url)))
           (git-output (shell-command-to-string (concat "git rev-parse --abbrev-ref HEAD")))
           (git-branch (s-trim git-output))
           (git-icon  "\xe0a0")
           (git-icon2 (propertize "\xf020" 'face `(:family "octicons")))
           (git-sep (propertize "" 'face 'lambda-meek)))
      (concat (propertize " (" 'face 'lambda-mild)
              (propertize git-repo 'face `(:inherit lambda-meek :weight light))
              git-sep
              (propertize git-branch 'face `(:inherit lambda-meek :weight light))
              (propertize ") " 'face 'lambda-mild)))))

(defun lem--pwd-replace-home (pwd)
  "Replace home in PWD with tilde (~) character."
  (interactive)
  (let* ((home (expand-file-name (getenv "HOME")))
         (home-len (length home)))
    (if (and
         (>= (length pwd) home-len)
         (equal home (substring pwd 0 home-len)))
        (concat "~" (substring pwd home-len))
      pwd)))

(defun lem--pwd-shorten-dirs (pwd)
  "Shorten all directory names in PWD except the last two."
  (let ((p-lst (split-string pwd "/")))
    (if (> (length p-lst) 2)
        (concat
         (mapconcat (lambda (elm) (if (zerop (length elm)) ""
                               (substring elm 0 1)))
                    (butlast p-lst 2)
                    "/")
         "/"
         (mapconcat (lambda (elm) elm)
                    (last p-lst 2)
                    "/"))
      pwd)))  ;; Otherwise, we just return the PWD

(defun lem--split-directory-prompt (directory)
  (if (string-match-p ".*/.*" directory)
      (list (file-name-directory directory) (file-name-base directory))
    (list "" directory)))

(defun lem-eshell-config--prompt-function ()
  "Prettify eshell prompt."
  (let* ((os-char (cond ((string-equal system-type "darwin") "")
                        ((string-equal system-type "gnu/linux") "🐧")
                        ((string-equal system-type "cygwin") "🗔")
                        (t "?")))
         (pwd        (eshell/pwd))
         (directory (lem--split-directory-prompt
                     (lem--pwd-shorten-dirs
                      (lem--pwd-replace-home pwd))))
         (parent (car directory))
         (name   (cadr directory))
         (branch (lem-eshell-config--git-prompt pwd)))

    (concat (propertize "\n╭─ " 'face 'lambda-mild)
            (propertize (format "%s" os-char) 'face 'lambda-meek)
            (propertize " ─ "  'face 'lambda-mild)
            (propertize (format-time-string "%H:%M:%S" (current-time))  'face 'lambda-meek)
            (propertize " ─ "  'face 'lambda-mild) (propertize "\xf07c  "  'face 'lambda-meek)
            (propertize parent 'face 'lambda-meek)
            (propertize name 'face `(:inherit lambda-meek :weight bold))
            "\n"
            (propertize (concat "│" (number-to-string eshell-prompt-number))   'face 'lambda-mild)
            "\n"
            (propertize "╰─>>"  'face 'lambda-mild)
            (if branch
                branch
              " ")
            (propertize (lem-eshell-config--prompt-char) 'face `(:inherit lambda-yellow :weight ultra-bold))
            ;; needed for the input text to not have prompt face
            (propertize " " 'face 'default))))

(setq eshell-prompt-function #'lem-eshell-config--prompt-function)

;;; Clear Eshell
;; Make eshell act like a standard unix terminal.
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
          #'(lambda()
              (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

;;; Open in iTerm
(defun eshell/iterm ()
  "Open the current directory of the eshell buffer in iTerm."
  (interactive)
  (let* ((iterm-app-path "/Applications/iTerm.app")
         (iterm-brew-path "/opt/homebrew-cask/Caskroom/iterm2/2.1.4/iTerm.app")
         (iterm-path (if (file-directory-p iterm-app-path)
                         iterm-app-path
                       iterm-brew-path)))
    (shell-command (concat "open -a " iterm-path " ."))))

;;; Aliases
;; It's nicer to type (range 0 3) in eshell.
(defalias 'eshell/range #'number-sequence)
(defalias 'range #'number-sequence)


;;; Provide Eshell
(provide 'lem-setup-eshell)
