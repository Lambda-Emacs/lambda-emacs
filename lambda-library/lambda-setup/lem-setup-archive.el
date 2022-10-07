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
    `(setq ,NAME(lambda () (when ,FORM
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

  (esh-section esh-clock
               "\xf017"  ;  (clock icon)
               (format-time-string "%H:%M" (current-time))
               '(:foreground "forest green"))

  ;; Below I implement a "prompt number" section
  (setq eshell-prompt-number 0)
  (add-hook 'eshell-exit-hook (lambda () (setq eshell-prompt-number 0)))
  (advice-add 'eshell-send-input :before
              (lambda (&rest args) (setq eshell-prompt-number (+ 1 eshell-prompt-number))))

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
  (setq eshell-funcs (list esh-dir))

  ;; Enable the new eshell prompt
  (setq eshell-prompt-function 'esh-prompt-func))

;;;; Eshell Fringe Status
;; Show last status in fringe
;; https://github.com/ryuslash/eshell-fringe-status
(use-package eshell-fringe-status
  :disabled
  :config
  (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode))

;;;; Eshell Autosuggest
;; Fish-like history autosuggestions in eshell
;; https://github.com/dieggsy/esh-autosuggest/
(use-package esh-autosuggest
  :disabled 
  :hook (eshell-mode . esh-autosuggest-mode))

;; (defun lem-eshell-config--git-branch-prompt ()
;;   "Git branch prompt."
;;   (let ((branch (car (cl-loop for match in (split-string (shell-command-to-string "git branch") "\n")
;;                               when (string-match "^\*" match)
;;                               collect match))))
;;     (if (not (eq branch nil))
;;         (concat
;;          (propertize "[" 'face 'lambda-mild)
;;          (propertize "\xe907 " 'face 'lambda-meek)
;;          (propertize (substring branch 2) 'face 'lambda-meek)
;;          (propertize "] " 'face 'lambda-mild))
;;       "")))

(defun old-lem-eshell-config--git-branch-prompt ()
  "Git project & branch prompt using Magit."
  (require 'magit)
  (let ((project (ignore-errors (file-name-nondirectory (directory-file-name (magit-toplevel))))) ;; magit errors when not in a repo, so ignore that
        (branch (magit-get-current-branch)))
    (if (not (eq project nil))
        (concat
         (propertize "[" 'face 'lambda-mild)
         (propertize project 'face '(:inherit lambda-meek :weight light))
         (propertize "" 'face 'lambda-mild)
         (propertize branch 'face '(:inherit lambda-meek :weight light))
         (propertize "] " 'face 'lambda-mild))
      "")))
;;;; Eshell Magit
(defun eshell/magit ()
  "Function to open magit-status for the current directory"
  (interactive)
  (require 'magit)
  (magit-status-setup-buffer default-directory)
  nil)

;;;; Eshell Dired
(defun eshell/do ()
  "Open dired buffer for current directory in other window."
  (interactive)
  (dired-other-window "."))

;;;; Eshell Toggle
(use-package eshell-toggle
  :straight (:type git :host github :repo "4da/eshell-toggle")
  :disabled
  :custom
  (eshell-toggle-size-fraction 3)
  (eshell-toggle-run-command nil)
  (eshell-toggle-window-side 'above)
  (eshell-toggle-use-projectile-root nil)
  :bind
  ("M-\\" . eshell-toggle))


;;;; Colors

;; (use-package xterm-color
;;   :straight t
;;   :defer t)

;; (defun eshell-enable-256-colors ()
;;   (interactive)
;;   (progn
;;     (require 'xterm-color)
;;     (setq xterm-color-preserve-properties t)
;;     (setenv "TERM" "xterm-256color")
;;     (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)

;;     (setq eshell-output-filter-functions
;;           (remove 'eshell-handle-ansi-color eshell-output-filter-functions))))
;;;; Icons
;; Requires `exa` be installed
;; https://github.com/ogham/exa
;; (with-eval-after-load 'eshell
;;   (lem-set-eshell-alias
;;    "ls"  "exa --group-directories-first --icons --color=always           "
;;    "ll"  "exa -a -l --group-directories-first --git --icons --color=always"
;;    "lsa" "exa -a --group-directories-first --icons --color=always        "
;;    "lsd" "exa -D -a -l --git --icons --color=always                      "))
:init
(when (version= emacs-version "29")
  ;; HACK: - see https://github.com/hlissner/doom-emacs/issues/6063
  (defvar read-symbol-positions-list nil)
  :config/el-patch
  (defun helpful--autoloaded-p (sym buf)
    "Return non-nil if function SYM is autoloaded."
    (-when-let (file-name (buffer-file-name buf))
      (setq file-name (s-chop-suffix ".gz" file-name))
      (help-fns--autoloaded-p sym)))

  (defun helpful--skip-advice (docstring)
    "Remove mentions of advice from DOCSTRING."
    (let* ((lines (s-lines docstring))
           (relevant-lines
            (--take-while
             (not (or (s-starts-with-p ":around advice:" it)
                      (s-starts-with-p "This function has :around advice:" it)))
             lines)))
      (s-trim (s-join "\n" relevant-lines))))))
;; NOTE: emacs 29 has a breaking change so using el-patch to keep helpful working
;; see https://github.com/Wilfred/helpful/pull/283
;;;; Tab Bookmark
;; Bookmark window configurations in a tab
;; NOTE: would be good to get this working with tabspaces
(use-package tab-bookmark
  :straight (:type git :host github :repo "minad/tab-bookmark")
  :commands (tab-bookmark tab-bookmark-open)
  :bind (:map project-prefix-map
         ("m" . tab-bookmark)))
