;;; lem-setup-functions.el --- summary -*- lexical-binding: t -*-

;; Author: Colin McLear
;; Maintainer: Colin McLear
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


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

;; Useful functions

;;; Code:

;;;; Config Helper Functions

;; Function to navigate config files
(defun lem/find-files-setup-config-directory ()
  "Find lem-setup files."
  (interactive)
  (let ((default-directory lem-setup-dir))
    (call-interactively 'find-file)))

;; Function to search in config files
(defun lem/search-setup-config-files ()
  "Async fuzzy search with ripgrep for all lem configuration files"
  (interactive)
  (consult-ripgrep lem-setup-dir))

;; Load init file
(defun lem/load-init-file ()
  "Load the base init file."
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

(defun lem/load-config-file ()
  "Load the user config file."
  (interactive)
  (load-file lem-config-file))

;;;; Get string from file
(defun lem/get-string-from-file (filePath)
  "Read a file and return the contents as a string"
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;;;; Archive region to setup-archive
(defun lem/setup-kill-and-archive-region ()
  "Delete & append region to end of setup-archive.el"
  (interactive)
  (append-to-file (region-beginning) (region-end) (concat lem-setup-dir "setup-files-archive/setup-archive.el"))
  (delete-region (region-beginning) (region-end)))

;;;; Insert Comment Seperator
;; ======================================================
;; Insert commented seperator like this line
;; ======================================================
;; https://github.com/kuanyui/writing-utils.el/blob/db29d30e11b6d6d96c0d351b642af97631f3365f/writing-utils.el#L85

(defun lem/insert-commented-separator()
  "Insert a commented separator in your code. Like this in
  ELisp:
  ;; ======================================================
  ;; Title
  ;; ======================================================
  Which makes code easier to read.
  "
  (interactive)
  (let* ((line (make-string 54 (string-to-char "=")))
	     (comment-start (if (member major-mode '(emacs-lisp-mode lisp-mode))
			                ";; " comment-start))
         (seperator (concat comment-start line)))
    (when (> (current-column) 0) (end-of-line) (newline))
    (insert (format "%s\n%s\n%s"
		            seperator comment-start seperator))
    (previous-line)
    ))

;;;; Delete Frame or Quit
(defun lem/delete-frame-or-quit ()
  "Delete the selected frame & kill terminal buffers. If the last frame, kill Emacs."
  (interactive)
  (kill-matching-buffers "*vterm" nil t)
  (when (condition-case nil (delete-frame)
          (error (save-buffers-kill-emacs))))
  (select-frame-set-input-focus (selected-frame)))

;;;; Insert Weather
;; From [[https://www.baty.blog/2019/insert-weather-into-emacs-buffer][Jack Baty]] with some slight modifications for formatting. See also [[https://github.com/chubin/wttr.in][wttr.in]]. 
(defun lem/insert-weather ()
  (interactive)
  (let ((w (shell-command-to-string "curl -s 'wttr.in/?0qT'")))
    (insert (mapconcat (function (lambda (x) (format ": %s" x)))
                       (split-string w "\n")
                       "\n")))
  (newline))

;;;; Built-in Functions
;; These are useful built-in functions, but you have to enable them
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; Not going to use these commands
(put 'ns-print-buffer 'disabled t)
(put 'suspend-frame 'disabled t)

;;;; Call an emacs instance
;; Call an emacs instance for testing

(defun lem/call-emacs ()
  (interactive)
  (start-process "Emacs" nil
                 ;; (executable-find "/usr/local/bin/emacs")))
                 (executable-find "/Applications/Emacs.app/Contents/MacOS/Emacs")))
;; (executable-find "Emacs")))

;;;; Formatted Copy
(defun formatted-copy ()
  "Export region to HTML, and copy it to the clipboard."
  (interactive)
  (save-window-excursion
    (let* ((buf (org-export-to-buffer 'html "*Formatted Copy*" nil nil t t))
           (html (with-current-buffer buf (buffer-string))))
      (with-current-buffer buf
        (shell-command-on-region
         (point-min)
         (point-max)
         "textutil -stdin -format html -convert rtf -stdout | pbcopy"))
      (kill-buffer buf))))

(global-set-key (kbd "H-w") 'formatted-copy)

;;;; CRUX
(use-package crux
  :defer 1
  :bind
  ("C-k"   . crux-smart-kill-line)
  ("C-a"   . crux-move-beginning-of-line)
  (:map lem+buffer-keys
   ("C"    . crux-cleanup-buffer-or-region)))


;;;; Delete Current File
;; from magnars

(defun lem/delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to delete this file? ")
        (delete-file filename t)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;;;; Duplicate file
;; Duplicate a file in dired or deer
(defun lem/duplicate-file ()
  (interactive)
  (dired-do-copy-regexp "\\(.*\\)\\.\\(.*\\)" "\\1 (copy).\\2"))

;;;; Ediff Hydra
;; From the hydra wiki https://github.com/abo-abo/hydra/wiki/Emacs#ediff

(with-eval-after-load 'ediff
  (with-eval-after-load 'hydra
  (defhydra hydra-ediff (:color blue :hint nil)
    "
  ^Buffers           Files           VC                     Ediff regions
  ----------------------------------------------------------------------
  _b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
  _B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
  _c_urrent file
  "
    ("b" ediff-buffers)
    ("B" ediff-buffers3)
    ("=" ediff-files)
    ("f" ediff-files)
    ("F" ediff-files3)
    ("c" ediff-current-file)
    ("r" ediff-revision)
    ("l" ediff-regions-linewise)
    ("w" ediff-regions-wordwise))))
;; esc quits

;;;; Quit All the Things!
;; From a great vim migration guide by Juanjo Álvarez
;; https://juanjoalvarez.net/en/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
;; (original code from davvil) https://github.com/davvil/.emacs.d/blob/master/init.el

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
  In Delete Selection mode, if the mark is active, just deactivate it;
  then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(with-eval-after-load 'evil
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state))


;;;; Fill or Unfill
;; See https://sachachua.com/dotemacs/
;; and https://endlessparentheses.com/fill-and-unfill-paragraphs-with-a-single-key.html
(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

;;FIXME: I can't get this to work properly in org-mode
(defun lem/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'lem/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'lem/fill-paragraph)))

(defun lem/fill-paragraph ()
  "if in an org buffer use org-fill-paragraph; else use fill-paragraph"
  (interactive)
  (if (derived-mode-p 'org-mode)
      (call-interactively #'lem-org-fill-paragraph)
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'lem/fill-paragraph)


;;;; Unfill
;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun lem/unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))
(keymap-global-set "M-Q" #'lem/unfill-paragraph)

;;;; Insert seconds
(defun lem/insert-time-string ()
  "Insert year, day, hour, month, and second as a single string
with no seperation"
  (interactive)
  (insert (format-time-string "%Y%d%H%M%S")))

(defun lem/insert-time-seconds-epoch ()
  "Insert the integer number of seconds since the epoch."
  (interactive)
  (insert (format-time-string "%s")))
;; (global-set-key (kbd "C-c e") 'lem/insert-time-seconds-epoch)

;;;; Jump to sexp

(defun lem/forward-or-backward-sexp (&optional arg)
  "Go to the matching parenthesis character if one is adjacent to point."
  (interactive "^p")
  (cond ((looking-at "\\s(") (forward-sexp arg))
        ((looking-back "\\s)" 1) (backward-sexp arg))
        ;; Now, try to succeed from inside of a bracket
        ((looking-at "\\s)") (forward-char) (backward-sexp arg))
        ((looking-back "\\s(" 1) (backward-char) (forward-sexp arg))))

;;;; Make Parent Directory
;;  Create a directory – or a hierarchy of them – while finding a file in a
;;  nonexistent directory. From mbork
;;  http://mbork.pl/2016-07-25_Making_directories_on_the_fly

(defun make-parent-directory ()
  "Make sure the directory of `buffer-file-name' exists."
  (make-directory (file-name-directory buffer-file-name) t))

(add-hook 'find-file-not-found-functions #'make-parent-directory)

;;;; Move File
(defun lem/move-file ()
  "Write this file to a new location, and delete the old one."
  (interactive)
  (let ((old-location (buffer-file-name)))
    (call-interactively #'write-file)
    (when old-location
      (delete-file old-location))))


;;;; Clipboard Transforms Using Pandoc
(defun lem/org-to-markdown ()
  "convert clipboard contents from org to markdown and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t markdown"))
  (yank))

(defun lem/markdown-to-org ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f markdown -t org"))
  (yank))

(defun lem/tex-to-org ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f latex -t org"))
  (yank))

(defun lem/org-to-tex ()
  "convert clipboard contents from org to tex and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t latex"))
  (yank))

(defun lem/tex-to-markdown ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f latex -t markdown --markdown-headings=atx"))
  (yank))

(defun lem/markdown-to-tex ()
  "convert clipboard contents from markdown to org and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f markdown -t latex"))
  (yank))

(defun lem/cite-to-org ()
  "convert clipboard contents from markdown to org with citations and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc --bibliography=/Users/Roambot/Dropbox/Work/Master.bib -s -t markdown-native_divs-raw_html-citations | pandoc -f markdown -t org"))
  (yank))

(defun lem/cite-to-markdown ()
  "convert clipboard contents to markdown with citations and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc --bibliography=/Users/Roambot/Dropbox/Work/bibfile.bib -s -t markdown-native_divs-raw_html-citations --markdown-headings=atx"))
  (yank))

(defun lem/bibtex-to-yaml-reference ()
  "convert clipboard bibtex contents to yaml and paste"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc-citeproc -y -f bibtex | pbcopy"))
  (yank))

(defun lem/md-to-rtf ()
  "convert md to rtf and send to clipboard"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f markdown -t rtf | pbcopy"))
  (yank))


;; NOTE: piping to pbcopy doesn't seem to work but it is ready to paste as is
(defun lem/org-to-rtf ()
  "convert org to rtf and send to clipboard"
  (interactive)
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t html | /usr/bin/textutil -stdin -stdout -format html -convert rtf -fontsize 14 -font Helvetica | pbcopy")))

(defun lem/org-to-mail-rtf ()
  "copy buffer, convert clipboard contents from org to rtf, and send to mail message"
  (interactive)
  (lem/copy-whole-buffer-to-clipboard)
  ;; (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t rtf"))
  (kill-new (shell-command-to-string "pbpaste | pandoc -f org -t html | /usr/bin/textutil -stdin -stdout -format html -convert rtf -fontsize 14 | pbcopy"))
  (kill-buffer)
  (delete-frame)
  (do-applescript "if application \"Mail\" is running then
  tell application \"Mail\"
  activate
  delay 0.35
  tell application \"System Events\"
  keystroke \"v\" using {command down}
  end tell
  end tell
  end if"))


;;;; Mailmate save mail and kill client
;; Save buffer and exit emacsclient for Mailmate
(defun lem/email-save-and-kill ()
  (interactive)
  (save-buffer)
  (server-edit))

;;;; Smart Yanking
;;Courtesy of Marcin Borkowski http://mbork.pl/2018-07-02_Smart_yanking

(defun has-space-at-boundary-p (string)
  "Check whether STRING has any whitespace on the boundary.
  Return 'left, 'right, 'both or nil."
  (let ((result nil))
    (when (string-match-p "^[[:space:]]+" string)
      (setq result 'left))
    (when (string-match-p "[[:space:]]+$" string)
      (if (eq result 'left)
          (setq result 'both)
        (setq result 'right)))
    result))

(defun is-there-space-around-point-p ()
  "Check whether there is whitespace around point.
  Return 'left, 'right, 'both or nil."
  (let ((result nil))
    (when (< (save-excursion
               (skip-chars-backward "[:space:]"))
             0)
      (setq result 'left))
    (when (> (save-excursion
               (skip-chars-forward "[:space:]"))
             0)
      (if (eq result 'left)
      (setq result 'both)
    (setq result 'right)))
    result))

(defun set-point-before-yanking (string)
  "Put point in the appropriate place before yanking STRING."
  (let ((space-in-yanked-string (has-space-at-boundary-p string))
    (space-at-point (is-there-space-around-point-p)))
    (cond ((and (eq space-in-yanked-string 'left)
        (eq space-at-point 'left))
       (skip-chars-backward "[:space:]"))
      ((and (eq space-in-yanked-string 'right)
        (eq space-at-point 'right))
       (skip-chars-forward "[:space:]")))))

(defun set-point-before-yanking-if-in-text-mode (string)
  "Invoke `set-point-before-yanking' in text modes."
  (when (derived-mode-p 'text-mode)
    (set-point-before-yanking string)))

(advice-add
 'insert-for-yank
 :before
 #'set-point-before-yanking-if-in-text-mode)
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2007-05/msg00975.html


;;;; Transpose hydra
;; From the hydra wiki https://github.com/abo-abo/hydra/wiki/Emacs#transpose

(with-eval-after-load 'hydra
  (bind-key (concat lem-prefix " .")
            (defhydra hydra-transpose (:color red)
              "Transpose"
              ("c" transpose-chars "characters")
              ("w" transpose-words "words")
              ("o" org-transpose-words "Org mode words")
              ("l" transpose-lines "lines")
              ("s" transpose-sentences "sentences")
              ("e" org-transpose-element "Org mode elements")
              ("p" transpose-paragraphs "paragraphs")
              ("t" org-table-transpose-table-at-point "Org mode table")
              ("q" nil "cancel" :color blue))))



;;;; Toggle markup
(defun lem/toggle-display-markup ()
  "Toggle the display of markup in markdown and org modes"
  (interactive)
  (if (eq major-mode 'org-mode)
      (org-toggle-link-display)
    (if markdown-hide-markup
        (markdown-toggle-markup-hiding 0)
      (markdown-toggle-markup-hiding))))



;;;; Wrap in Yaml block
(defun lem/yaml-wrap ()
  "wrap region in --- for a yaml block"
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "---" "\n")
    (goto-char start)
    (insert "---" "\n")))

;;;; Quit Function
(defun doom-quit-p (&optional prompt)
  "Return t if this session should be killed. Prompts the user for
 confirmation."
  (or (yes-or-no-p (format "››› %s" (or prompt "Quit Emacs?")))
      (ignore (message "Aborted"))))
(setq confirm-kill-emacs nil)
(add-hook 'kill-emacs-query-functions #'doom-quit-p)
(defvar +doom-quit-messages
  '(;; from Doom 1
    "Let's beat it -- This is turning into a bloodbath!"
    "I wouldn't leave if I were you. DOS is much worse."
    "Ya know, next time you come in here I'm gonna toast ya."
    "Go ahead and leave. See if I care."
    "Are you sure you want to quit this great editor?"
    ;; Custom
    "Emacs! Emacs!! Emacs!!!"
    "The King is dead, long live the King!"
    "Like you have somewhere better to be..."
    "Don't worry, I won't tell everyone you're a failure"
    "Aus so krummem Holze, als woraus der Mensch gemacht ist, kann nichts ganz Gerades gezimmert werden"
    "Sed omnia praeclara tam difficilia, quam rara sunt"
    "(setq nothing t everything 'permitted)"
    "Emacs will remember that."
    "Emacs, Emacs never changes."
    "Hey! Hey, M-x listen!"
    "Okay, look. We've both said a lot of things you're going to regret..."
    "You are *not* prepared!")
  "A list of quit messages, picked randomly by `+doom-quit'. Taken from
 http://doom.wikia.com/wiki/Quit_messages and elsewhere.")

(defun +doom|quit (&rest _)
  (doom-quit-p
   (format "%s  Quit?"
           (nth (random (length +doom-quit-messages))
                +doom-quit-messages))))

(remove-hook 'kill-emacs-query-functions #'doom-quit-p)
(add-hook 'kill-emacs-query-functions #'+doom|quit)

;;; End Funtions-Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lem-setup-functions)

;;; lem-setup-functions.el ends here
