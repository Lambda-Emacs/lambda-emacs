;;; lem-setup-help.el --- summary -*- lexical-binding: t -*-

;; Author: Colin McLear
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

;; This file provides settings for dialogues, popups, help, and info buffers.

;;; Code:


;;;; Dialogs, Menus, & Popups

;;;;; Dialogs and popups
;; No file dialog
(setq use-file-dialog nil)
;; No dialog box
(setq use-dialog-box nil)
;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)
;; Set popup windows
(setq-default pop-up-windows t)
;; Set popup frames
(setq-default pop-up-frames nil)

;;;;; Hydra Menus
(use-package hydra :defer 1)

;;;;; Transient Menus
(use-package transient
  :defer 1
  :custom
  (transient-levels-file (concat lem-cache-dir "transient/levels.el"))
  (transient-values-file (concat lem-cache-dir "transient/values.el"))
  (transient-history-file (concat lem-cache-dir "transient/history.el"))
  ;; set transient popop to top of window
  (transient-display-buffer-action '(display-buffer-in-side-window
                                     (side . top)
                                     (dedicated . t)
                                     (inhibit-same-window . t)
                                     (window-parameters (no-other-window . t)))))

;;;; Help & Information

;;;;; Menu-bar
;; Use menu bar in GUI on MacOS as its useful for discovery & doesn't take any
;; extra room.
(if (and sys-mac (display-graphic-p))
    (customize-set-variable 'menu-bar-mode t)
  (customize-set-variable 'menu-bar-mode nil))

;;;;; Help Focus
(use-package help
  :straight (:type built-in)
  :custom
  ;; Always focus on help window/buffer
  (help-window-select 't))

;;;;; Help At Point
(use-package help-at-pt
  :straight (:type built-in)
  :custom
  (help-at-pt-timer-delay 0.1)
  (help-at-pt-display-when-idle '(flymake-diagnostic)))

;;;;; Better Help with Helpful
;; Better help info
;; Much better lookup both in details and headings/aesthetics

;; NOTE: emacs 29 has a breaking change so using el-patch to keep helpful working
;; see https://github.com/Wilfred/helpful/pull/283

(use-package helpful
  :defer t
  :bind (("C-h f"   . #'helpful-function)
         ("C-h k"   . #'helpful-key)
         ("C-h o"   . #'helpful-symbol)
         ("C-h v"   . #'helpful-variable)
         ("C-h C-." . #'helpful-at-point)
         ("C-h C-l" . #'find-library))
  :init
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
      (s-trim (s-join "\n" relevant-lines)))))

;; Display file commentary section
(global-set-key (kbd "C-h C-c") 'finder-commentary)

;;;;; Elisp Demos
;; Provide examples of Elisp code
(use-package elisp-demos
  :defer 1
  :config
  ;; inject demos into helpful
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

;;;;; Better Info
;; Better looking info pages
(use-package info-colors
  :straight (:host github :repo "ubolonton/info-colors")
  :hook (Info-selection . info-colors-fontify-node))

;;;;; Help Transient

;; A little more useful for calling help than just C-h (less info density)
;; see https://luca.cambiaghi.me/vanilla-emacs/readme.html#h:14F8ECDE-9E15-46F7-B903-ECE383251C48
(with-eval-after-load 'transient
  (bind-key (concat lem-prefix " h") 'lem-help-transient)
  (transient-define-prefix lem-help-transient ()
    ["Help Commands"
     ["Mode & Bindings"
      ("m" "Mode" describe-mode)
      ("b" "Major Bindings" which-key-show-full-major-mode)
      ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
      ("d" "Descbinds" describe-bindings)
      ]
     ["Describe"
      ("c" "Command" helpful-command)
      ("f" "Function" helpful-callable)
      ("o" "Symbol"  helpful-symbol)
      ("v" "Variable" helpful-variable)
      ("k" "Key" helpful-key)
      ]
     ["Info on"
      ("C-c" "Emacs Command" Info-goto-emacs-command-node)
      ("C-f" "Function" info-lookup-symbol)
      ("C-v" "Variable" info-lookup-symbol)
      ("C-k" "Emacs Key" Info-goto-emacs-key-command-node)
      ]
     ["Goto Source"
      ("L" "Library" find-library)
      ("F" "Function" find-function)
      ("V" "Variable" find-variable)
      ("K" "Key" find-function-on-key)
      ]
     ]
    [
     ["Internals"
      ("e" "Echo Messages" view-echo-area-messages)
      ("l" "Lossage" view-lossage)
      ]
     ["Describe"
      ("s" "Symbol" helpful-symbol)
      ("." "At Point   " helpful-at-point)
      ("C-d" "Face" describe-face)
      ("w" "Where Is" where-is)
      ("=" "Position" what-cursor-position)
      ]
     ["Info Manuals"
      ("C-i" "Info" info)
      ("C-4" "Other Window " info-other-window)
      ("C-e" "Emacs" completing-read-info-emacs-manual)
      ("C-l" "Elisp" completing-read-info-elisp-manual)
      ]
     ["Exit"
      ("q" "Quit" transient-quit-one)
      ("<escape>" "Quit" transient-quit-one)
      ]
     ]
    [
     ["External"
      ("W" "Dictionary" dictionary-lookup-definition)
      ]
     ]
    ))

;;;;; Completing-Read Info
;; Info search commands using completing-read
;; https://github.com/raxod502/selectrum/wiki/Useful-Commands#info
(defvar Info-directory-list)
(defvar Info-additional-directory-list)
(defvar Info-default-directory-list)
(declare-function info-initialize "info")
(declare-function cl-mapcar "cl-lib")

(defvar completing-read-info-history nil
  "Completion history for `completing-read-info' and derived commands.")

(defun completing-read--info-section-candidates (top-node)
  "Return an alist of sections and candidates in the Info buffer TOP-NODE.

Candidates are returned in the order that their links are listed
in the Info buffer, which might be different from how the
sections are actually ordered."
  (let ((sub-topic-format
         ;; Node links look like "* Some Thing:: Description" or
         ;; "* Some Thing: actual link. Description", where descriptions
         ;; are optional and might continue on the next line.
         ;;
         ;; The `info' library states:
         ;; Note that nowadays we expect Info files to be made using makeinfo.
         ;; In particular we make these assumptions:
         ;;  - a menu item MAY contain colons but not colon-space ": "
         ;;  - a menu item ending with ": " (but not ":: ") is an index entry
         ;;  - a node name MAY NOT contain a colon
         ;; This distinction is to support indexing of computer programming
         ;; language terms that may contain ":" but not ": ".
         (rx "* " (group (+? (not ?:))) ":"
             (or ":" (seq " "  (group (+? (not "."))) "."))
             ;; Include the description, if one exists.
             ;; If it doesn't, the line ends immediately.
             (or "\n"
                 (seq (0+ blank)
                      (group (+? anychar))
                      ;; Sometimes a heading follows on the next line,
                      ;; and sometimes there's any empty blank line
                      ;; (such as before a section title).  For now,
                      ;; assume continuation lines use indentation and
                      ;; other lines don't.
                      "\n" (not blank))))))
    (save-match-data
      (save-selected-window
        (with-temp-buffer
          ;; Some nodes created from multiple files, so we need to create a
          ;; buffer to make sure that we see everything.
          (info top-node (current-buffer))
          (goto-char (point-min))
          (let ((candidates-alist))
            (while (re-search-forward sub-topic-format nil t)
              (forward-line 0)         ; Go back to start of line.
              (let* ((node-display-name (match-string 1))
                     (node-actual-name (or (match-string 2) node-display-name)))
                (push (cons (concat node-display-name
                                    (if-let ((node-description (match-string 3)))
                                        (propertize
                                         (thread-last node-description
                                                      (replace-regexp-in-string "\n" "")
                                                      (replace-regexp-in-string " +" " ")
                                                      (concat " - "))
                                         'face 'completions-annotations)))
                            node-actual-name)
                      candidates-alist)))
            (nreverse candidates-alist)))))))

(defun completing-read--info-top-dir-menu-items ()
  (let ((sub-topic-format
         ;; The `info' library states:
         ;; Note that nowadays we expect Info files to be made using makeinfo.
         ;; In particular we make these assumptions:
         ;;  - a menu item MAY contain colons but not colon-space ": "
         ;;  - a menu item ending with ": " (but not ":: ") is an index entry
         ;;  - a node name MAY NOT contain a colon
         ;; This distinction is to support indexing of computer programming
         ;; language terms that may contain ":" but not ": ".
         (rx (seq "* " (group (+? anything))
                  ": "
                  (group "(" (+? anything) ")" (*? (not ".")))
                  "."
                  (zero-or-one (seq (any "\n" " " "\t")
                                    (group (+? anychar))))
                  "\n" (or "\n" "*")))))
    (let ((candidates-alist))
      ;; Go through nodes in Info buffer "(dir)Top".
      (save-match-data
        (save-selected-window
          (with-temp-buffer
            ;; Some nodes created from multiple files, so we need to create a
            ;; buffer to make sure that we see everything.
            (info "(dir)Top" (current-buffer))
            (goto-char (point-min))
            (search-forward "Menu:\n")
            (while (re-search-forward sub-topic-format nil t)
              (forward-line 0)          ; Go back to start of line.
              (let* ((node-display-name (match-string-no-properties 1))
                     (node-actual-name (or (match-string-no-properties 2) node-display-name)))
                (push (cons (concat node-display-name
                                    (if-let ((node-description (match-string-no-properties 3)))
                                        (propertize
                                         (thread-last node-description
                                                      (replace-regexp-in-string "\n" "")
                                                      (replace-regexp-in-string " +" " ")
                                                      (concat " - "))
                                         'face 'completions-annotations)))
                            node-actual-name)
                      candidates-alist))))))
      ;; In case something isn't listed (Emacs might just insert itself?), also
      ;; add in files from the Info directories as nodes themselves.
      (dolist (file (save-match-data
                      (thread-last (append (or Info-directory-list
                                               Info-default-directory-list)
                                           Info-additional-directory-list)
                                   (mapcan (lambda (directory)
                                             (when (file-directory-p directory)
                                               (directory-files directory nil "\\.info" t))))
                                   (mapcar (lambda (file)
                                             (string-match "\\(.+?\\)\\." file)
                                             (match-string 1 file)))
                                   seq-uniq)))
        ;; TODO: Node should actually come from opening the file.
        (let ((node (concat "(" file ")")))
          (unless (rassoc node candidates-alist)
            (push (cons file node) candidates-alist))))
      (nreverse candidates-alist))))

;;;###autoload
(defun completing-read-info (&optional top-node)
  "Use `completing-read' to jump to an Info topic.

Select from the available Info top-level nodes, then one of the sub-nodes.
If TOP-NODE is provided, then just select from its sub-nodes."
  (interactive)
  (unless top-node
    (setq top-node
          (let* ((items (completing-read--info-top-dir-menu-items))
                 (key (completing-read "Info node: "
                                       (lambda (input predicate action)
                                         (if (eq action 'metadata)
                                             `(metadata
                                               ;; (display-sort-function . identity)
                                               (category              . info))
                                           (complete-with-action action
                                                                 items
                                                                 input
                                                                 predicate)))
                                       nil
                                       t)))
            (cdr (assoc key items)))))
  ;; If looking at a base node (e.g., "(emacs)"), then select from list of
  ;; optional sub-nodes.  If looking at a normal node (e.g., "(emacs)Intro"),
  ;; then just go there instead of asking for more sub-nodes.
  (if (string-match-p "(.*?)\\'" top-node)
      (let* ((section-candidates-alist (completing-read--info-section-candidates top-node))
             (section (completing-read "Info section: "
                                       (lambda (input predicate action)
                                         (if (eq action 'metadata)
                                             `(metadata
                                               (display-sort-function . identity)
                                               (category              . info))
                                           (complete-with-action action
                                                                 section-candidates-alist
                                                                 input
                                                                 predicate)))
                                       nil
                                       t nil 'completing-read-info-history)))
        (info (concat
               top-node
               (cdr (assoc section section-candidates-alist)))))
    (info top-node)))

;;;###autoload
(defun completing-read-info-elisp-manual ()
  "Like ‘completing-read-info’, but choose nodes from the Elisp reference manual. "
  (interactive)
  (completing-read-info "(elisp)"))

;;;###autoload
(defun completing-read-info-emacs-manual ()
  "Like ‘completing-read-info’, but directly choose nodes from the Emacs manual."
  (interactive)
  (completing-read-info "(emacs)"))

;;;###autoload
(defun completing-read-info-org-manual ()
  "Like ‘completing-read-info’, but directly choose nodes from the Org manual."
  (interactive)
  (completing-read-info "(org)"))

;; Bind keys for completing-read-info
(bind-key "C-h i" #'completing-read-info)


;;; Provide
(provide 'lem-setup-help)
;;; lem-setup-help.el ends here
