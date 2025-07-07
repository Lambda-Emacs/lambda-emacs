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
  :bind (:map vertico-map
         ("<escape>" . #'minibuffer-keyboard-quit)
         ("M-RET"    . #'vertico-exit))
  :hook (emacs-startup . vertico-mode)
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
  :ensure nil
  :after vertico
  :custom
  (vertico-buffer-hide-prompt t)
  :config/el-patch
  ;; Use el-patch
  ;; Set no headerline in vertico-buffer
  (cl-defmethod vertico--setup :after (&context (vertico-buffer-mode (eql t)))
    (add-hook 'pre-redisplay-functions 'vertico-buffer--redisplay nil 'local)
    (let* ((action vertico-buffer-display-action) tmp win old-buf
           (_ (unwind-protect
                  (progn
                    (with-current-buffer (setq tmp (generate-new-buffer "*vertico-buffer*"))
                      ;; Set a fake major mode such that `display-buffer-reuse-mode-window'
                      ;; does not take over!
                      (setq major-mode 'vertico-buffer-mode))
                    ;; Temporarily select the original window such
                    ;; that `display-buffer-same-window' works.
                    (setq old-buf (mapcar (lambda (win) (cons win (window-buffer win))) (window-list))
                          win (with-minibuffer-selected-window (display-buffer tmp action))
                          old-buf (alist-get win old-buf))
                    (set-window-buffer win (current-buffer)))
                (kill-buffer tmp)))
           (sym (make-symbol "vertico-buffer--destroy"))
           (depth (recursion-depth))
           (now (window-parameter win 'no-other-window))
           (ndow (window-parameter win 'no-delete-other-windows)))
      (fset sym (lambda ()
                  (when (= depth (recursion-depth))
                    (with-selected-window (active-minibuffer-window)
                      (if (not (and (window-live-p win) (buffer-live-p old-buf)))
                          (delete-window win)
                        (set-window-parameter win 'no-other-window now)
                        (set-window-parameter win 'no-delete-other-windows ndow)
                        (set-window-buffer win old-buf))
                      (when vertico-buffer-hide-prompt
                        (set-window-vscroll nil 0))
                      (remove-hook 'minibuffer-exit-hook sym)))))
      ;; NOTE: We cannot use a buffer-local minibuffer-exit-hook here.
      ;; The hook will not be called when abnormally exiting the minibuffer
      ;; from another buffer via `keyboard-escape-quit'.
      (add-hook 'minibuffer-exit-hook sym)
      (set-window-parameter win 'no-other-window t)
      (set-window-parameter win 'no-delete-other-windows t)
      (overlay-put vertico--candidates-ov 'window win)
      (when (and vertico-buffer-hide-prompt vertico--count-ov)
        (overlay-put vertico--count-ov 'window win))
      (setq-local show-trailing-whitespace nil
                  truncate-lines t
                  face-remapping-alist
                  (copy-tree `((mode-line-inactive mode-line)
                               ,@face-remapping-alist))
                  header-line-format nil
                  mode-line-format
                  (list (format " %s "
                                (propertize
                                 (format (if (< depth 2) "*%s*" "*%s [%s]*")
                                         (replace-regexp-in-string
                                          ":? *\\'" ""
                                          (minibuffer-prompt))
                                         depth)
                                 'face 'mode-line-buffer-id)))
                  cursor-in-non-selected-windows 'box
                  vertico-count (- (/ (window-pixel-height win)
                                      (default-line-height)) 1))))
  :config
  ;; put minibuffer at top -- this is the more natural place to be looking!
  (setq vertico-buffer-display-action
        '(display-buffer-in-side-window
          (window-height . 12)
          (side . top)))
  (vertico-buffer-mode 1))

;; Vertico repeat last command
(use-package vertico-repeat
  :ensure nil
  :hook (minibuffer-setup . vertico-repeat-save)
  :commands (vertico-repeat-last))

;; Configure directory extension
(use-package vertico-directory
  :ensure nil
  :after vertico
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

;; A few more useful configurations...
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
(setf enable-recursive-minibuffers t)

;; Persist history over Emacs restarts with savehist mode. Vertico sorts by history position.
;; Savehist is set up in lem-setup-settings.el

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
         ("C->" . embark-act-all)
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
  (define-key embark-file-map (kbd "D") 'lem-dired-here)
  (defun lem-dired-here (file)
    "Open dired in this directory"
    (dired (file-name-directory file)))

  (define-key embark-file-map (kbd "g") 'lem-consult-rg-here)
  (defun lem-consult-rg-here (file)
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
          ;; Support both external and built-in which-key
          (if (fboundp 'which-key--hide-popup-ignore-command)
              (which-key--hide-popup-ignore-command)
            (when (fboundp 'which-key-hide-popup)
              (which-key-hide-popup)))
        ;; Support both external and built-in which-key
        (if (fboundp 'which-key--show-keymap)
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
                         (not (string-suffix-p "-argument" (cdr binding)))))
          (when (fboundp 'which-key-show-keymap)
            (which-key-show-keymap
             (if (eq (plist-get (car targets) :type) 'embark-become)
                 "Become"
               (format "Act on %s '%s'%s"
                       (plist-get (car targets) :type)
                       (embark--truncate-target (plist-get (car targets) :target))
                       (if (cdr targets) "…" "")))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    ;; Support both external and built-in which-key
    (cond ((fboundp 'which-key--hide-popup-ignore-command)
           (which-key--hide-popup-ignore-command))
          ((fboundp 'which-key-hide-popup)
           (which-key-hide-popup)))
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;; Marginalia
;; Enable richer annotations using the Marginalia package
;; Info about candidates pulled from metadata
(use-package marginalia
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
  :init
  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)
  :config
  ;; Previewing
  ;; https://github.com/minad/consult#live-previews
  ;; This setting previews on any key but with a very short delay (.2 sec)
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file consult-theme
   :preview-key '(:debounce 0.2 any))

  ;; Configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; search settings
  (setq consult-ripgrep-args "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")

  ;; Make consult locate work with macos spotlight
  (setq consult-locate-args "mdfind -name")

  (setq consult-async-min-input 2)

  ;; Consult info functions
  (defun consult-info-emacs ()
    "Search through Emacs info pages."
    (interactive)
    (consult-info "emacs" "efaq" "elisp" "cl" "compat"))

  (defun consult-info-org ()
    "Search through the Org info page."
    (interactive)
    (consult-info "org"))

  (defun consult-info-completion ()
    "Search through completion info pages."
    (interactive)
    (consult-info "vertico" "consult" "marginalia" "orderless" "embark"
                  "corfu" "cape" "tempel"))
  (bind-key "C-h i" #'consult-info))

;;;;; Consult Search At Point
;; Search at point with consult
(defun consult-line-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

;;;;; Consult Dir
;; Consult-dir allows you to easily select and switch between “active” directories.
(use-package consult-dir
  :commands (consult-dir
             consult-dir-jump-file)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;;; In-Buffer Completion
;;;;; Corfu
(use-package corfu
  :hook
  (window-setup . global-corfu-mode)
  :bind
  (:map corfu-map
   ("C-j"      . corfu-next)
   ("C-k"      . corfu-previous)
   ("C-g"      . corfu-quit)
   ("M-l"      . corfu-show-location)
   ("M-SPC" . corfu-insert-separator)
   ("<escape>" . corfu-quit)
   ("<return>" . corfu-insert)
   ("TAB" . corfu-insert)
   ([tab] . corfu-insert))
  :custom
  ;; auto-complete
  (corfu-auto t)
  (corfu-min-width 25)
  (corfu-max-width 100)
  (corfu-count 10)
  (corfu-scroll-margin 5)
  (corfu-cycle t)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)
  (corfu-separator ?\s) ;; Use space as separator
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary 'separator)
  (corfu-preview-current t)  ;; Preview current candidate?
  (corfu-preselect-first t)    ;; Preselect first candidate?
  (corfu-history-mode 1) ;; Use history for completion
  (corfu-popupinfo-delay 1) ;; delay for info popup
  :config
  ;; Enable Corfu completion for commands like M-: (eval-expression) or M-!
  ;; (shell-command)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  (add-hook 'eshell-mode-hook (lambda () (setq-local corfu-quit-no-match t
                                                corfu-quit-at-boundary t
                                                corfu-auto nil)))
  ;; Avoid press RET twice in shell
  ;; https://github.com/minad/corfu#completing-in-the-eshell-or-shell
  (defun corfu-send-shell (&rest _)
    "Send completion candidate when inside comint/eshell."
    (cond
     ((and (derived-mode-p 'eshell-mode) (fboundp 'eshell-send-input))
      (eshell-send-input))
     ((and (derived-mode-p 'comint-mode)  (fboundp 'comint-send-input))
      (comint-send-input))))

  (advice-add #'corfu-insert :after #'corfu-send-shell)

  ;; Completion in eshell
  (add-hook 'eshell-mode-hook
            (lambda ()
              (setq-local corfu-auto nil)
              (corfu-mode)))

  ;; Display popup info
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1))


;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;;;;;; Corfu Extensions (Cape)
;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ;; ("C-c p s" . cape-symbol)
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
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  :config
  ;; Sanitize the `pcomplete-completions-at-point' Capf.
  ;; The Capf has undesired side effects on Emacs 28 and earlier.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify))

;;;;; Kind Icon (For Corfu)
(use-package kind-icon
  :defer 1
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)
  ;; NOTE kind-icon' depends on `svg-lib' which creates a cache directory that
  ;; defaults to the `user-emacs-directory'. Here, I change that directory to
  ;; the cache location.
  (svg-lib-icons-dir (concat lem-cache-dir  "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'

  ;; NOTE 2022-02-05: Add hook to reset cache so the icon colors match the theme
  ;; If this isn't done, then the backgound color will remain the same, meaning
  ;; it will not match the background color corresponding to the current theme.
  ;; This hook is already set in the `lem-setup-themes.el' file, but you could
  ;; set it here if you prefer:
  ;; e.g. (add-hook 'after-load-theme-hook #'kind-icon-reset-cache)
  ;; kind-icon needs to have its cache flushed after theme change
  (add-hook 'lambda-themes-after-load-theme-hook #'kind-icon-reset-cache))

;;;;; Yasnippet
(defcustom lem-all-snippets-dir (concat lem-etc-dir "all-snippets/") "DIR for all snippet files."
  :group 'lambda-emacs)

(use-package yasnippet
  :defer 1
  :bind (:map yas-minor-mode-map
         ("C-'" . yas-expand))
  :preface
  (mkdir (concat lem-all-snippets-dir "lem-snippets/") t)
  (mkdir (concat lem-all-snippets-dir "yasnippet-snippets/") t)
  :custom
  (yas-snippet-dirs `(;; custom snippets
                      ,(concat lem-all-snippets-dir "lem-snippets/")
                      ;; yas snippets
                      ,(concat lem-all-snippets-dir "yasnippet-snippets/")))
  ;; NOTE: need to specify dirs; does not look in non-snippet subdirs
  (yas--loaddir yas-snippet-dirs)
  (yas-installed-snippets-dir yas-snippet-dirs)
  (yas--default-user-snippets-dir yas-snippet-dirs)
  :config
  ;; see https://emacs.stackexchange.com/a/30150/11934
  (defun lem-yas-org-mode-hook ()
    (setq-local yas-buffer-local-condition
                '(not (org-in-src-block-p t))))
  (add-hook 'org-mode-hook #'lem-yas-org-mode-hook)
  ;; suppress warnings when expanding
  (with-eval-after-load 'warnings
    (push '(yasnippet backquote-change) warning-suppress-types))
  (yas-global-mode 1))

;; the official snippet collection https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :after (yasnippet)
  :custom
  (yasnippet-snippets-dir (concat lem-all-snippets-dir "yasnippet-snippets")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lem-setup-completion)
;;; lem-setup-completion.el ends here
