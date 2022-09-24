;;; lem-setup-eshell.el --- eshell setup             -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Colin McLear
;; Author: Colin McLear <mclear@unl.edu>
;; Keywords: terminals, lisp, emulations

;;; Commentary
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
;; Eshell is an elisp shell. It has its own configuration parameters, distinct
;; from those of shell or ansi-terminal. It is closer to something like bash or
;; zsh than it is to a terminal emulator like vterm

;;;; Eshell Settings
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
  (eshell-list-files-after-cd nil)
  (eshell-last-dir-ring-file-name (concat lem-etc-dir "eshell/lastdir")))

(use-package em-ls
  :straight (:type built-in)
  :after eshell
  :custom
  (eshell-ls-use-colorls t)
  (eshell-ls-use-in-dired nil))

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
  (eshell-destroy-buffer-when-process-dies t))

(use-package em-banner
  :straight (:type built-in)
  :after eshell
  :custom
  (eshell-banner-message ""))

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
                                         "nmtui" "alsamixer" "htop" "el" "elinks" "tail" "top" "nano" "ssh")))

;;;; Eshell Pcomplete

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

;; Provide help support -- see also the info function below
(use-package esh-help
  :after eshell
  :straight t
  :config
  ;; Eldoc support.
  (setup-esh-help-eldoc))

;; Dir navigation -- see also dir jumping below
(use-package eshell-up
  :straight t
  :commands (eshell-up)
  :config
  (defalias 'eshell/up #'eshell-up))

;;;; Eshell Prompt
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

;;;; Aliases
;; It's nicer to type (range 0 3) in eshell.
(defalias 'eshell/range #'number-sequence)
(defalias 'range #'number-sequence)

;; Don't auto-write our aliases! Let us manage our own `eshell-aliases-file'
;; or configure `lem-eshell-aliases' via elisp.
(advice-add #'eshell-write-aliases-list :override #'ignore)

(defvar lem-eshell-aliases
  '(;; Git
    ("g" "git --no-pager $*")
    ("gg" "magit-status")
    ("gd" "git diff --color $*")
    ("gl" "magit-log-all")
    ("gsh" "git stash")
    ("gbr" "git branch $*")
    ("gco" "git checkout $*")
    ("gs" "git status")
    ("grb" "git rebase $*")
    ("grh" "git reset --hard")

    ;; Homebrew
    ("bi" "brew info")
    ("bs" "brew search")
    ("bu" "brew update && brew outdated && brew upgrade && brew cleanup && brew doctor")

    ;; Listing
    ("l"  "ls $*")
    ("ls"  "ls -X $*")
    ("la" "ls -laX $*")
    ("ll" "ls -lahsX $*")

    ;; Navigation
    ("bb" "consult-buffer")
    ("bd" "eshell-up $1")
    ("d" "dired $1")
    ("e" "find-file $1")
    ("ec" "find-file $1")
    ("ed" (eshell/cd "~/.emacs.d"))
    ("ff" "find-file $1")
    ("fo" "find-file-other-window $1")
    ("fr" (consult-recent-file))
    ("pp" "project-switch-project")
    ("pk" "eshell-up-peek $1")
    ("up" "eshell-up $1")

    ;; Search
    ("rg" "rg --color=always $*")

    ;; Quitting
    ("ex" "exit")
    ("x" "exit")
    ("q"  "exit")
    ("qr" "restart-emacs")
    ("qq" "save-buffers-kill-emacs")
    ) ; more sensible than default
  "An alist of default eshell aliases, meant to emulate useful shell utilities,
like fasd and bd. Note that you may overwrite these in your
`eshell-aliases-file'. This is here to provide an alternative, elisp-centric way
to define your aliases.
You should use `lem-set-eshell-alias' to change this.")

;; Define a var to backup aliases that may already exist
(defvar lem-eshell--default-aliases nil)

;;;###autodef
(defun lem-set-eshell-alias (&rest aliases)
  "Define aliases for eshell.
ALIASES is a flat list of alias -> command pairs. e.g.
  (lem-set-eshell-alias
    \"hi\"  \"echo hello world\"
    \"bye\" \"echo goodbye world\")"
  (or (cl-evenp (length aliases))
      (signal 'wrong-number-of-arguments (list 'even (length aliases))))
  (with-eval-after-load 'em-alias
    (while aliases
      (let ((alias (pop aliases))
            (command (pop aliases)))
        (if-let* ((oldval (assoc alias lem-eshell-aliases)))
            (setcdr oldval (list command))
          (push (list alias command) lem-eshell-aliases))))
    (when (boundp 'eshell-command-aliases-list)
      (if lem-eshell--default-aliases
          (setq eshell-command-aliases-list
                (append lem-eshell--default-aliases
                        lem-eshell-aliases))
        (setq eshell-command-aliases-list lem-eshell-aliases)))))

(use-package em-alias
  :straight (:type built-in)
  :after eshell
  :custom
  (eshell-aliases-file (concat lem-etc-dir "eshell/alias"))
  :config
  ;; See https://github.com/doomemacs/doomemacs/blob/master/modules/term/eshell/
  (setq lem-eshell--default-aliases eshell-command-aliases-list
        eshell-command-aliases-list
        (append eshell-command-aliases-list
                lem-eshell-aliases)))

;;;; Syntax Highlighting
(use-package eshell-syntax-highlighting
  :after eshell
  :straight t
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode +1))

;;;; Better File Listing (ls)
;; https://github.com/mnewt/dotemacs/blob/master/init.el
;; Make files and dirs clickable as well as prettyfied w/icons and suffixes

(defun eshell-ls-file-at-point ()
  "Get the full path of the Eshell listing at point."
  (get-text-property (point) 'file-name))

(defun eshell-ls-find-file ()
  "Open the Eshell listing at point."
  (interactive)
  (find-file (eshell-ls-file-at-point)))

(defun eshell-ls-delete-file ()
  "Delete the Eshell listing at point."
  (interactive)
  (let ((file (eshell-ls-file-at-point)))
    (when (yes-or-no-p (format "Delete file %s?" file))
      (delete-file file 'trash))))

(defvar eshell-ls-file-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'eshell-ls-find-file)
    (define-key map (kbd "<return>") #'eshell-ls-find-file)
    (define-key map [mouse-1] #'eshell-ls-find-file)
    (define-key map (kbd "D") #'eshell-ls-delete-file)
    map)
  "Keys in effect when point is over a file from `eshell/ls'.")

(defface all-the-icons-eshell-dir-face
  '((((background dark)) :foreground "white")
    (((background light)) :foreground "black"))
  "Face for the directory icon"
  :group 'all-the-icons-faces)

(defcustom all-the-icons-eshell-v-adjust 0.01
  "The default vertical adjustment of the icon in the eshell buffer."
  :group 'all-the-icons
  :type 'number)

(defcustom all-the-icons-eshell-monochrome t
  "Whether to show the icons as the same color as the text on the same line."
  :group 'all-the-icons
  :type 'boolean)

(defun lem-eshell-better-ls (file)
  "Add features to listings in `eshell/ls' output.
The features are:
1. Add decoration like 'ls -F':
 * Mark directories with a `/'
 * Mark executables with a `*'
2. Make each listing into a clickable link to open the
corresponding file or directory.
3. Add icons (requires `all-the-icons`)
This function is meant to be used as advice around
`eshell-ls-annotate', where FILE is the cons describing the file."
  (let* ((name (car file))
         (icon (if (eq (cadr file) t)
                   (all-the-icons-icon-for-dir name)
                 (all-the-icons-icon-for-file name)))
         (suffix
          (cond
           ;; Directory
           ((eq (cadr file) t)
            "/")
           ;; Executable
           ((and (/= (user-uid) 0) ; root can execute anything
                 (eshell-ls-applicable (cdr file) 3 #'file-executable-p (car file)))
            "*"))))
    (cons
     (concat " "
             icon
             " "
             (propertize name
                         'keymap eshell-ls-file-keymap
                         'mouse-face 'highlight
                         'file-name (expand-file-name (substring-no-properties (car file)) default-directory))
             (when (and suffix (not (string-suffix-p suffix name)))
               (propertize suffix 'face 'shadow)))
     (cdr file)
     )))

(advice-add #'eshell-ls-annotate :filter-return #'lem-eshell-better-ls)

;;;; Useful Functions

;;;;; Eshell in Home Directory
(defun lem-eshell-home ()
  "Open eshell in home dir."
  (interactive)
  (let ((default-directory "~/"))
    (require 'eshell)
    (eshell)))

;;;;; Clear Eshell
;; Make eshell act like a standard unix terminal.
(defun eshell-clear-buffer ()
  "Clear terminal"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;;;;; Eshell Info
(defun eshell/info (subject)
  "Read the Info manual on SUBJECT."
  (let ((buf (current-buffer)))
    (Info-directory)
    (let ((node-exists (ignore-errors (Info-menu subject))))
      (if node-exists
	      0
	    ;; We want to switch back to *eshell* if the requested
	    ;; Info manual doesn't exist.
	    (switch-to-buffer buf)
	    (eshell-print (format "There is no Info manual on %s.\n"
			                  subject))
	    1))))

;;;;; Eshell Less
(defun eshell/less (&rest args)
  "Invoke `view-file' on a file. \"less +42 foo\" will go to line 42 in
      the buffer for foo."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
	    (let* ((line (string-to-number (match-string 1 (pop args))))
	           (file (pop args)))
	      (lem-eshell-view-file file)
	      (goto-line line))
      (lem-eshell-view-file (pop args)))))
(defalias 'eshell/more 'eshell/less)

;;;;; Eshell Make Dir/CD

(defun eshell/mkdir-and-cd (dir)
  "Create a directory then cd into it."
  (make-directory dir t)
  (eshell/cd dir))

;;;;; Git Status
;; Used as an eshell/alias, the current directory isn't registered, so we must
;; use a function.
(defun eshell/gst (&rest args)
  (magit-status (pop args) nil)
  (eshell/echo)) ;; The echo command suppresses output

;;;;; Jump Directories (w/Consult & Consult-Dir)
(defun eshell/z (&optional regexp)
  "Navigate to a previously visited directory in eshell, or to
any directory proferred by `consult-dir'."
  (let ((eshell-dirs (delete-dups
                      (mapcar 'abbreviate-file-name
                              (ring-elements eshell-last-dir-ring)))))
    (cond
     ((and (not regexp) (featurep 'consult-dir))
      (let* ((consult-dir--source-eshell `(:name "Eshell"
                                           :narrow ?e
                                           :category file
                                           :face consult-file
                                           :items ,eshell-dirs))
             (consult-dir-sources (cons consult-dir--source-eshell
                                        consult-dir-sources)))
        (eshell/cd (substring-no-properties
                    (consult-dir--pick "Switch directory: ")))))
     (t (eshell/cd (if regexp (eshell-find-previous-directory regexp)
                     (completing-read "cd: " eshell-dirs)))))))


;;;;; Jump to Project Root
(defun eshell/cg () (interactive) (eshell/cd (vc-git-root ".")))

;;;;; Open in iTerm
(defun eshell/iterm ()
  "Open the current directory of the eshell buffer in iTerm."
  (interactive)
  (let* ((iterm-app-path "/Applications/iTerm.app")
         (iterm-brew-path "/opt/homebrew-cask/Caskroom/iterm2/2.1.4/iTerm.app")
         (iterm-path (if (file-directory-p iterm-app-path)
                         iterm-app-path
                       iterm-brew-path)))
    (shell-command (concat "open -a " iterm-path " ."))))

;;;;; Remote Directory Change
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
      method user nil host nil (or directory "") hop))))

;;;;; View Files

(defun lem-eshell-view-file (file)
  "A version of `view-file' which properly respects the eshell prompt."
  (interactive "fView file: ")
  (unless (file-exists-p file) (error "%s does not exist" file))
  (let ((had-a-buf (get-file-buffer file))
	    (buffer (find-file-noselect file)))
    (if (eq (with-current-buffer buffer (get major-mode 'mode-class))
	        'special)
	    (progn
	      (switch-to-buffer buffer)
	      (message "Not using View mode because the major mode is special"))
      (let ((undo-window (list (window-buffer) (window-start)
			                   (+ (window-point)
				                  (length (funcall eshell-prompt-function))))))
	    (switch-to-buffer buffer)
	    (view-mode-enter (cons (selected-window) (cons nil undo-window))
			             'kill-buffer)))))

;;;; Setup Hooks
(defun lem-setup-eshell ()
  (interactive)
  ;; Clear eshell keybind
  (local-set-key (kbd "C-l") 'eshell-clear-buffer)
  ;; Use imenu to jump prompts
  ;; https://xenodium.com/imenu-on-emacs-eshell/
  (setq-local imenu-generic-expression
              '(("Prompt" " λ \\(.*\\)" 1)))
  ;; Turn off semantic-mode in eshell buffers
  (semantic-mode -1)
  ;; Turn off hl-line-mode
  (hl-line-mode -1)
  ;; Remove fringe
  (set-window-fringes nil 0 0)
  (set-window-margins nil 1 nil)
  ;; Scrolling
  (setq hscroll-margin 0)
  ;; Text wrapping
  (visual-line-mode +1)
  (set-display-table-slot standard-display-table 0 ?\ ))
(add-hook 'eshell-mode-hook #'lem-setup-eshell)

;; ls files on cd
(defun lem-eshell-list-files-on-cd ()
  "Use ls to show files w/directories first."
  (eshell/ls "-X"))
(add-hook 'eshell-directory-change-hook #'lem-eshell-list-files-on-cd)

;;; End Shell
;;; Provide Eshell
(provide 'lem-setup-eshell)
