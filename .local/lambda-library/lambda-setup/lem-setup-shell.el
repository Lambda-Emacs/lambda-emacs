;; Shell
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
(setq-default shell-file-name "/opt/homebrew/bin/zsh")
(setq explicit-shell-file-name "/opt/homebrew/bin/zsh")
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
(defun my-term-hook ()
  (goto-address-mode) (hl-line-mode 0) (setq comint-buffer-maximum-size most-positive-fixnum))
(add-hook 'term-mode-hook 'my-term-hook)
(add-hook 'vterm-mode-hook 'my-term-hook)

;; paste and navigation
(defun term-send-tab ()
  "Send tab in term mode."
  (interactive)
  (term-send-raw-string "\t"))

;; Emacs doesn’t handle less well, so use cat instead for the shell pager
(setenv "PAGER" "cat")

;; hack to fix pasting issue, the paste micro-state won't work in term
;; (general-define-key :states '(normal motion) :keymaps 'term-raw-map
;;   "p" 'term-paste
;;   "C-k" 'term-send-up
;;   "C-j" 'term-send-down)

;; (general-define-key :states '(insert) :keymaps 'term-raw-map
;;   "C-c C-d" 'term-send-eof
;;   "C-c C-z" 'term-stop-subjob
;;   "<tab>"   'term-send-tab
;;   "s-v"     'term-paste
;;   "C-k"     'term-send-up
;;   "C-j"     'term-send-down)



;;;; Vterm
;; Better terminal function---way faster than ansi-term
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
  (setq vterm-term-environment-variable "xterm-256color")
  (add-hook 'vterm-mode-hook
            (lambda ()
              (meow-insert-mode))))

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

;;; Eshell
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell Eshell is
;; an elisp shell. It has its own configuration parameters, distinct from those of
;; shell or ansi-terminal.
;;;; Eshell Settings
(use-package eshell
  :commands eshell
  :init
  (setq eshell-directory-name (concat lem-etc-dir "eshell/")
        eshell-history-file-name (concat lem-etc-dir "eshell/history")
        eshell-aliases-file (concat lem-etc-dir "eshell/alias")
        eshell-last-dir-ring-file-name (concat lem-etc-dir "eshell/lastdir")
        eshell-highlight-prompt nil
        eshell-buffer-shorthand t
        eshell-cmpl-ignore-case t
        eshell-cmpl-cycle-completions t
        eshell-destroy-buffer-when-process-dies t
        eshell-history-size 10000
        ;; auto truncate after 20k lines
        eshell-buffer-maximum-lines 20000
        eshell-hist-ignoredups t
        eshell-error-if-no-glob t
        eshell-glob-case-insensitive t
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output 'all
        eshell-list-files-after-cd t
        eshell-banner-message ""
        ;; eshell-banner-message (message "Emacs initialized in %.2fs \n\n" (float-time (time-subtract (current-time) my-start-time)))
        ;; eshell-banner-message "What would you like to do?\n\n"
        )
  ;; Visual commands
  (setq eshell-visual-commands '("ranger" "vi" "screen" "top" "less" "more" "lynx"
                                 "ncftp" "pine" "tin" "trn" "elm" "vim"
                                 "nmtui" "alsamixer" "htop" "el" "elinks"
                                 ))
  (setq eshell-visual-subcommands '(("git" "log" "diff" "show"))))

(defun lem-setup-eshell ()
  (interactive)
  ;; turn off semantic-mode in eshell buffers
  (semantic-mode -1)
  ;; turn off hl-line-mode
  (hl-line-mode -1))

;;;; Eshell Evil Histgory Navigation
;; History browsing. Note keybindings need to be buffer local as per
;; https://github.com/noctuid/general.el/issues/80
(add-hook 'eshell-mode-hook
          (lambda ()
            (bind-keys :map eshell-mode-map
              ("<down>" . eshell-next-input)
              ("<up>"   . eshell-previous-input)
              ("C-k"    . eshell-next-input)
              ("C-j"    . eshell-previous-input))))

;;;; Eshell Prompt
;; A nicer eshell prompt https://gist.github.com/ekaschalk/f0ac91c406ad99e53bb97752683811a5
;; with some useful discussion of how it was put together http://www.modernemacs.com/post/custom-eshell/
;; I've made just a few tiny modifications.

(with-eval-after-load 'eshell
  (require 'dash)
  (require 's)

  (defmacro with-face (STR &rest PROPS)
    "Return STR propertized with PROPS."
    `(propertize ,STR 'face (list ,@PROPS)))

  (defmacro esh-section (NAME ICON FORM &rest PROPS)
    "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
    `(setq ,NAME
           (lambda () (when ,FORM
                   (-> ,ICON
                       (concat esh-section-delim ,FORM)
                       (with-face ,@PROPS))))))

  (defun esh-acc (acc x)
    "Accumulator for evaluating and concatenating esh-sections."
    (--if-let (funcall x)
        (if (s-blank? acc)
            it
          (concat acc esh-sep it))
      acc))

  (defun esh-prompt-func ()
    "Build `eshell-prompt-function'"
    (concat esh-header
            (-reduce-from 'esh-acc "" eshell-funcs)
            "\n"
            eshell-prompt-string))

  (esh-section esh-dir
               "\xf07c"  ;  (faicon folder)
               (abbreviate-file-name (eshell/pwd))
               '(:foreground "#268bd2" :underline t))

  (esh-section esh-git
               "\xe907"  ;  (git icon)
               (with-eval-after-load 'magit
                 (magit-get-current-branch))
               '(:foreground "#b58900"))

  (esh-section esh-python
               "\xe928"  ;  (python icon)
               (with-eval-after-load "virtualenvwrapper"
                 venv-current-name))

  (esh-section esh-clock
               "\xf017"  ;  (clock icon)
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))

  ;; Below I implement a "prompt number" section
  (setq esh-prompt-num 0)
  (add-hook 'eshell-exit-hook (lambda () (setq esh-prompt-num 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq esh-prompt-num (cl-incf esh-prompt-num))))

  (esh-section esh-num
               "\xf0c9"  ;  (list icon)
               (number-to-string esh-prompt-num)
               '(:foreground "brown"))

  ;; Separator between esh-sections
  (setq esh-sep " | ")  ; or "  "

  ;; Separator between an esh-section icon and form
  (setq esh-section-delim " ")

  ;; Eshell prompt header
  (setq esh-header "\n┌─")  ; or "\n "

  ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
  ;; your login, these can be the same.
  (setq eshell-prompt-regexp "^└─>> ") ;; note the '^' to get regex working right
  (setq eshell-prompt-string "└─>> ")

  ;; Choose which eshell-funcs to enable
  (setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func))

;;;; Clear Eshell
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

;;;; Eshell Magit
(defun eshell/magit ()
  "Function to open magit-status for the current directory"
  (interactive)
  (require 'magit)
  (magit-status-setup-buffer default-directory)
  nil)

;;;; Eshell Fringe Status
;; Show last status in fringe
;; https://github.com/ryuslash/eshell-fringe-status
(use-package eshell-fringe-status
  :defer t
  :config
  (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode))

;;;; Eshell Autosuggest
;; Fish-like history autosuggestions in eshell
;; https://github.com/dieggsy/esh-autosuggest/
(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode))

;;;; Open in iTerm
(defun lem-open-dir-in-iterm ()
  "Open the current directory of the buffer in iTerm."
  (interactive)
  (let* ((iterm-app-path "/Applications/iTerm.app")
         (iterm-brew-path "/opt/homebrew-cask/Caskroom/iterm2/2.1.4/iTerm.app")
         (iterm-path (if (file-directory-p iterm-app-path)
                         iterm-app-path
                       iterm-brew-path)))
    (shell-command (concat "open -a " iterm-path " ."))))
;; (global-set-key (kbd "C-x t") 'open-dir-in-iterm)

;;;; End Shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lem-setup-shell)
