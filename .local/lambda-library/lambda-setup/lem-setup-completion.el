;;; lem-setup-completion.el --- summary -*- lexical-binding: t -*-

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

;; All packages related to core narrowing and completion functions

;;; Code:

;;;; Narrowing Completion

;;;;; Vertico
;; Enable vertico for vertical completion
;; This and selectrum are great packages, but vertico is preferable if I can get feature parity with what I was using in selectrum
(use-package vertico
  :straight (:host github :repo "minad/vertico"
             :includes (vertico-repeat vertico-directory vertico-buffer)
             :files (:defaults "extensions/vertico-directory.el" "extensions/vertico-buffer.el" "extensions/vertico-repeat.el"))
  :bind (:map vertico-map
         ("<escape>" . #'minibuffer-keyboard-quit)
         ("C-n"      . #'vertico-next-group      )
         ("C-p"      . #'vertico-previous-group  )
         ("C-j"      . #'vertico-next            )
         ("C-k"      . #'vertico-previous        )
         ("M-RET"    . #'vertico-exit))
  :hook (after-init . vertico-mode)
  :config
  ;; Cycle through candidates
  (setq vertico-cycle t)

  ;; Don't resize buffer
  (setq vertico-resize nil)

  ;; try the `completion-category-sort-function' first
  (advice-add #'vertico--sort-function :before-until #'completion-category-sort-function)

  (defun completion-category-sort-function ()
    (alist-get (vertico--metadata-get 'category)
               completion-category-sort-function-overrides))

  (defvar completion-category-sort-function-overrides
    '((file . directories-before-files))
    "Completion category-specific sorting function overrides.")

  (defun directories-before-files (files)
    ;; Still sort by history position, length and alphabetically
    (setq files (vertico-sort-history-length-alpha files))
    ;; But then move directories first
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files))))

;;;;; Vertico Packages
;; Use vertico in buffer
(use-package vertico-buffer
  :after vertico
  :config/el-patch
  ;; Use el-patch
  ;; Set no headerline in vertico-buffer
  (defun vertico-buffer--setup ()
    "Setup minibuffer overlay, which pushes the minibuffer content down."
    (add-hook 'pre-redisplay-functions 'vertico-buffer--redisplay nil 'local)
    (let ((temp (generate-new-buffer "*vertico*")))
      (setq vertico-buffer--window (display-buffer temp vertico-buffer-display-action))
      (set-window-buffer vertico-buffer--window (current-buffer))
      (kill-buffer temp))
    (let ((sym (make-symbol "vertico-buffer--destroy"))
          (depth (recursion-depth))
          (now (window-parameter vertico-buffer--window 'no-other-window))
          (ndow (window-parameter vertico-buffer--window 'no-delete-other-windows)))
      (fset sym (lambda ()
                  (when (= depth (recursion-depth))
                    (with-selected-window (active-minibuffer-window)
                      (when (window-live-p vertico-buffer--window)
                        (set-window-parameter vertico-buffer--window 'no-other-window now)
                        (set-window-parameter vertico-buffer--window 'no-delete-other-windows ndow))
                      (when vertico-buffer-hide-prompt
                        (set-window-vscroll nil 0))
                      (remove-hook 'minibuffer-exit-hook sym)))))
      ;; NOTE: We cannot use a buffer-local minibuffer-exit-hook here.
      ;; The hook will not be called when abnormally exiting the minibuffer
      ;; from another buffer via `keyboard-escape-quit'.
      ;; (add-hook 'minibuffer-exit-hook sym)
      (set-window-parameter vertico-buffer--window 'no-other-window t)
      (set-window-parameter vertico-buffer--window 'no-delete-other-windows t)
      (overlay-put vertico--candidates-ov 'window vertico-buffer--window)
      (when (and vertico-buffer-hide-prompt vertico--count-ov)
        (overlay-put vertico--count-ov 'window vertico-buffer--window))
      (setq-local show-trailing-whitespace nil
                  truncate-lines t
                  header-line-format nil
                  mode-line-format
                  (list (format " %s "
                                (propertize
                                 (format (if (< depth 2) "*%s*" "*%s [%s]*")
                                         (replace-regexp-in-string
                                          ":? *\\'" ""
                                          (minibuffer-prompt))
                                         depth)
                                 'face 'mode-line-buffer-id))
                        '(:eval (vertico--format-count)))
                  cursor-in-non-selected-windows 'box
                  vertico-count (- (/ (window-pixel-height vertico-buffer--window)
                                      (default-line-height)) 2))))
  :config
  (setq vertico-buffer-display-action
        '(display-buffer-in-side-window
          (window-height . 13)
          (side . top)))
  (vertico-buffer-mode 1))


;; Vertico repeat last command
(use-package vertico-repeat
  :hook (minibuffer-setup . vertico-repeat-save)
  :commands (vertico-repeat))

;; Configure directory extension
(use-package vertico-directory
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; A few more useful configurations...
(use-package emacs
  :straight (:type built-in)
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Grow and shrink minibuffer
  (setq resize-mini-windows t)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setf enable-recursive-minibuffers t))

;; Persist history over Emacs restarts with savehist mode. Vertico sorts by history position.
;; See setup-settings.el

;;;;; Ordering
;; Setup for vertico
;; Use the `orderless' completion style.
;; Enable `partial-completion' for files to allow path expansion.
;; You may prefer to use `initials' instead of `partial-completion'.
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;;;;; Embark
;; Actions on narrowed candidates
(use-package embark
  :straight (embark :type git :host github :repo "oantolin/embark")
  :commands (embark-act embark-keymap-help)
  :custom
  ;; Use which-key
  ;; Don't display extra embark buffer
  (embark-indicators '(embark-which-key-indicator
                       embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  ;; Use keymap -- completing-read on C-h
  (embark-prompter 'embark-keymap-prompter)
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-completion-map
         ("C-;"   . embark-act-noexit)
         ("C-S-o" . embark-act)
         ("C-J"   . embark-switch-to-live-occur)
         ("M-q"   . embark-occur-toggle-view)
         :map completion-list-mode-map
         (";" . embark-act)
         :map embark-file-map
         ("x" . consult-file-externally)
         ;; When using the Embark package, you can bind `marginalia-cycle' as an Embark action
         :map embark-general-map
         ("A"  . marginalia-cycle))
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  ;; Useful Functions
  (define-key embark-file-map (kbd "D") 'lem/dired-here)
  (defun lem/dired-here (file)
    "Open dired in this directory"
    (dired (file-name-directory file)))

  (define-key embark-file-map (kbd "g") 'lem/consult-rg-here)
  (defun lem/consult-rg-here (file)
    "consult-ripgrep in this directory."
    (let ((default-directory (file-name-directory file)))
      (consult-ripgrep)))

  ;; Which-key integration
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . embark-consult-preview-minor-mode))

;;;;; Marginalia
;; Enable richer annotations using the Marginalia package
;; Info about candidates pulled from metadata
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
  :custom-face
  (marginalia-documentation ((t (:inherit bespoke-faded))))
  :bind (:map minibuffer-local-map
         ("C-M-a" . marginalia-cycle))
  :init
  (marginalia-mode)
  :config
  (setq marginalia-align 'center))

;;;;; Consult
;; Example configuration for Consult
;; Useful functions; a drop-in replacement for ivy/swiper

(use-package consult
  :straight (consult :type git :host github :repo "minad/consult" :includes consult-org)
  :commands (consult-line
             consult-line-multi
             consult-buffer
             consult-project-buffer
             consult-find
             consult-apropos
             consult-yank-pop
             consult-goto-line
             consult-outline
             consult-org-agenda
             consult-org-heading
             consult-flymake)
  :bind (:map project-prefix-map
         ("b" . consult-project-buffer)
         ("m" .  consult-bookmark))

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :custom-face
  (consult-file ((t (:inherit bespoke-popout))))
  (consult-line-number ((t (:inherit bespoke-faded))))

  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)

  :config
  ;; configure a function which returns the project root directory.
  ;; project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

  ;; Preview is manual and immediate
  ;; https://github.com/minad/consult#live-previews
  (setq consult-preview-key (kbd "C-f"))

  ;; search settings
  (setq consult-grep-args
        "grep --null --line-buffered --color=never --ignore-case\
   --exclude-dir=.git --line-number -I -R -S .")

  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --line-number .")

  ;; Make consult locate work with macos spotlight
  (setq consult-locate-args "mdfind -name")

  (setq consult-async-min-input 0))

;; Use consult-completing-read for enhanced interface.
(advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

;;;;; Consult Search At Point
;; Search at point with consult
(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;;;; Consult Dir
;; Consult-dir allows you to easily select and switch between “active” directories.
(use-package consult-dir
  :straight (:host github :repo "karthink/consult-dir")
  :commands (consult-dir
             consult-dir-jump-file)
  :bind (:map vertico-map
         ("C-d" . consult-dir)))

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

;;;; In-Buffer Completion
;;;;; Corfu
(use-package corfu
  :straight (:type git :host github :repo "minad/corfu")
  :defer 1
  :bind
  (:map corfu-map
   ("C-j"      . corfu-next)
   ("C-k"      . corfu-previous)
   ("M-d"      . corfu-show-documentation)
   ("C-g"      . corfu-quit)
   ("M-l"      . corfu-show-location)
   ("<escape>" . corfu-quit)
   ("<return>" . corfu-insert)
   ;; Or use TAB
   ("TAB" . corfu-next)
   ([tab] . corfu-next)
   ("S-TAB" . corfu-previous)
   ([backtab] . corfu-previous))
  :custom
  ;; auto-complete
  (corfu-auto t)

  (corfu-min-width 90)
  (corfu-max-width corfu-min-width)     ; Always have the same width
  (corfu-count 10)
  (corfu-scroll-margin 4)
  (corfu-cycle t)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)

  (corfu-echo-documentation nil)        ; Use corfu-doc
  (corfu-separator  ?_)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary t)

  (corfu-preview-current nil)       ; Preview current candidate?
  (corfu-preselect-first t)           ; Preselect first candidate?
  :config
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-auto t) ; Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  (corfu-global-mode 1))

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;;;;; Kind Icon (For Corfu)
(use-package kind-icon
  :straight (:type git :host github :repo "jdtsmith/kind-icon")
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)

  ;; NOTE kind-icon' depends `svg-lib' which creates a cache directory that
  ;; defaults to the `user-emacs-directory'. Here, I change that directory to
  ;; the cache location.
  (svg-lib-icons-dir (concat lem-cache-dir  "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; Add hook to reset cache so the icon colors match my theme
  ;; NOTE 2022-02-05: This is a hook which resets the cache whenever I switch
  ;; the theme using my custom defined command for switching themes. If I don't
  ;; do this, then the backgound color will remain the same, meaning it will not
  ;; match the background color corresponding to the current theme. Important
  ;; since I have a light theme and dark theme I switch between. This has no
  ;; function unless you use something similar
  (add-hook 'after-load-theme-hook #'kind-icon-reset-cache))

;;;;; Corfu Doc
(use-package corfu-doc
  :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :after corfu
  ;; :hook   (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
         ;; This is a manual toggle for the documentation window.
         ([remap corfu-show-documentation] . corfu-doc-toggle) ; Remap the default doc command
         ;; Scroll in the documentation window
         ("M-p" . corfu-doc-scroll-up)
         ("M-n" . corfu-doc-scroll-down))
  :custom
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20)

  ;; NOTE 2022-02-05: I've also set this in the `corfu' use-package to be
  ;; extra-safe that this is set when corfu-doc is loaded. I do not want
  ;; documentation shown in both the echo area and in the `corfu-doc' popup.
  (corfu-echo-documentation nil)
  :config
  ;; NOTE 2022-02-05: This is optional. Enabling the mode means that every corfu
  ;; popup will have corfu-doc already enabled. This isn't desirable for me
  ;; since (i) most of the time I do not need to see the documentation and (ii)
  ;; when scrolling through many candidates, corfu-doc makes the corfu popup
  ;; considerably laggy when there are many candidates. Instead, I rely on
  ;; manual toggling via `corfu-doc-toggle'.
  )

;;;;;; Corfu Extensions (Cape)
;; Add extensions
(use-package cape
  :straight (:type git :host github :repo "minad/cape")
  :after corfu
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-symbol)
         ("C-c p a" . cape-abbrev)
         ("C-c p i" . cape-ispell)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  (add-to-list 'completion-at-point-functions #'cape-abbrev))


;;;;; Company Org Block
;; Org block completion
;; https://github.com/xenodium/company-org-block
(use-package company-org-block
  :straight (:host github :repo "xenodium/company-org-block")
  :after org
  :bind (:map org-mode-map
         ("C-'" . org-block-capf))
  :custom
  (company-org-block-edit-style 'auto) ;; 'auto, 'prompt, or 'inline
  :config
  (require 'org-element)
  (defalias 'org-block-capf (cape-interactive-capf (cape-company-to-capf 'company-org-block))))

;;;;; Yasnippet
(use-package yasnippet
  :straight (:type git :host github :repo "joaotavora/yasnippet")
  :defer 1
  :bind (:map yas-minor-mode-map
         ("C-'" . yas-expand))
  :config
  ;; NOTE: need to specify dirs; does not look in non-snippet subdirs
  (setq yas-snippet-dirs '("~/.emacs.d/.local/all-snippets/lem-snippets/"
                           "~/.emacs.d/.local/all-snippets/yasnippet-snippets/"))
  (setq yas--loaddir yas-snippet-dirs)
  (setq yas-installed-snippets-dir yas-snippet-dirs)
  (setq yas--default-user-snippets-dir yas-snippet-dirs)
  ;; see https://emacs.stackexchange.com/a/30150/11934
  (defun lem/yas-org-mode-hook ()
    (setq-local yas-buffer-local-condition
                '(not (org-in-src-block-p t))))
  (add-hook 'org-mode-hook #'lem/yas-org-mode-hook)
  ;; Adding yasnippet support to company
  (with-eval-after-load 'company-mode
    (add-to-list 'company-backends '(company-yasnippet)))
  (yas-global-mode 1))

;; ;; the official snippet collection https://github.com/AndreaCrotti/yasnippet-snippets
;; (use-package yasnippet-snippets
;;   :straight (:type git :host github :repo "AndreaCrotti/yasnippet-snippets")
;;   :after (yasnippet)
;;   :config
;;   (setq yasnippet-snippets-dir (concat lem-local-dir "all-snippets/yasnippet-snippets")))


;;;; Completion Icons
(use-package all-the-icons-completion
  :straight (:host github :repo "iyefrat/all-the-icons-completion")
  :if (display-graphic-p)
  :hook (after-init . all-the-icons-completion-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'lem-setup-completion)
;;; lem-setup-completion.el ends here
