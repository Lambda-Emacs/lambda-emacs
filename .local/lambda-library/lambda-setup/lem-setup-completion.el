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
  :custom
  (vertico-buffer-hide-prompt t)
  :config/el-patch
  ;; Use el-patch
  ;; Set no headerline in vertico-buffer
  (defun vertico-buffer--setup ()
    "Setup buffer display."
    (add-hook 'pre-redisplay-functions 'vertico-buffer--redisplay nil 'local)
    (let* ((action vertico-buffer-display-action) tmp win
           (_ (unwind-protect
                  (progn
                    (setq tmp (generate-new-buffer "*vertico*")
                          ;; Temporarily select the original window such
                          ;; that `display-buffer-same-window' works.
                          win (with-minibuffer-selected-window (display-buffer tmp action)))
                    (set-window-buffer win (current-buffer)))
                (kill-buffer tmp)))
           (sym (make-symbol "vertico-buffer--destroy"))
           (depth (recursion-depth))
           (now (window-parameter win 'no-other-window))
           (ndow (window-parameter win 'no-delete-other-windows)))
      (fset sym (lambda ()
                  (when (= depth (recursion-depth))
                    (with-selected-window (active-minibuffer-window)
                      (when (window-live-p win)
                        (set-window-parameter win 'no-other-window now)
                        (set-window-parameter win 'no-delete-other-windows ndow))
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
                  mode-line-format nil
                  cursor-in-non-selected-windows 'box
                  vertico-count (- (/ (window-pixel-height win)
                                      (default-line-height)) 2))))
  :config
  ;; put minibuffer at top -- this is the more natural place to be looking!
  (setq vertico-buffer-display-action
        '(display-buffer-in-side-window
          (window-height . 13)
          (side . top)))
  (vertico-buffer-mode 1))

;; Vertico repeat last command
(use-package vertico-repeat
  :hook (minibuffer-setup . vertico-repeat-save)
  :commands (vertico-repeat-last))

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
(measure-time
 (message "*Loading further completion settings...*")
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
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;; Marginalia
;; Enable richer annotations using the Marginalia package
;; Info about candidates pulled from metadata
(use-package marginalia
  :straight (marginalia :type git :host github :repo "minad/marginalia")
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
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Replace `multi-occur' with `consult-multi-occur', which is a drop-in replacement.
  (fset 'multi-occur #'consult-multi-occur)
  :config
  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any))

  ;; Preview is manual and immediate
  ;; https://github.com/minad/consult#live-previews
  (setq consult-preview-key (kbd "C-f"))

  ;; search settings
  (setq consult-grep-args
        "grep --null --line-buffered --color=never --ignore-case\
   --exclude-dir=.git --line-number -I -R -S .")

  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
  --smart-case --no-heading --line-number --hidden --glob=!.git/ -L .")

  ;; Make consult locate work with macos spotlight
  (setq consult-locate-args "mdfind -name")

  (setq consult-async-min-input 2))

;; Use consult-completing-read for enhanced interface.
;; (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

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
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

;;;; In-Buffer Completion
;;;;; Corfu
(use-package corfu
  :straight (:type git :host github :repo "minad/corfu")
  :hook
  (after-init . global-corfu-mode)
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
  (corfu-max-width 90)
  (corfu-count 10)
  (corfu-scroll-margin 4)
  (corfu-cycle t)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3)
  (corfu-echo-documentation nil) ;; use corfu doc
  (corfu-separator  ?_)
  (corfu-quit-no-match 'separator)
  (corfu-quit-at-boundary t)
  (corfu-preview-current nil)       ; Preview current candidate?
  (corfu-preselect-first t)           ; Preselect first candidate?
  :config
  ;; Enable Corfu completion for commands like M-: (eval-expression) or M-!
  ;; (shell-command)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer))

;; Use dabbrev with Corfu!
(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand)))

;;;;; Corfu Doc
(use-package corfu-doc
  :straight (corfu-doc :type git :host github :repo "galeo/corfu-doc")
  :hook   (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
         ("C-d" . corfu-doc-toggle)
         ;;        ;; This is a manual toggle for the documentation window.
         ;;        ([remap corfu-show-documentation] . corfu-doc-toggle) ; Remap the default doc command
         ;; Scroll in the documentation window
         ("M-k" . corfu-doc-scroll-up)
         ("M-j" . corfu-doc-scroll-down))
  :custom
  (corfu-doc-max-width 70)
  (corfu-doc-max-height 20))

;;;;;; Corfu Extensions (Cape)
;; Add extensions
(use-package cape
  :straight (:type git :host github :repo "minad/cape")
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
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-tex)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-symbol)
  (add-to-list 'completion-at-point-functions #'cape-ispell)
  ;; (add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-line)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  )


;;;;; Kind Icon (For Corfu)
(use-package kind-icon
  :straight (:type git :host github :repo "jdtsmith/kind-icon")
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
  (add-hook 'lambda-themes-after-load-theme-hook #'kind-icon-reset-cache)
  )

;;;;; Yasnippet
(defcustom lem-all-snippets-dir (concat lem-etc-dir "all-snippets/") "DIR for all snippet files."
  :group 'lambda-emacs)

(use-package yasnippet
  :straight (:type git :host github :repo "joaotavora/yasnippet")
  :defer 1
  :bind (:map yas-minor-mode-map
         ("C-'" . yas-expand))
  :config
  ;; NOTE: need to specify dirs; does not look in non-snippet subdirs
  (setq yas-snippet-dirs `(,(concat lem-all-snippets-dir "lem-snippets/") ; custom snippets
                           ,(concat lem-all-snippets-dir "yasnippet-snippets/") ; yas snippets
                           ))
  (setq yas--loaddir yas-snippet-dirs)
  (setq yas-installed-snippets-dir yas-snippet-dirs)
  (setq yas--default-user-snippets-dir yas-snippet-dirs)
  ;; see https://emacs.stackexchange.com/a/30150/11934
  (defun lem-yas-org-mode-hook ()
    (setq-local yas-buffer-local-condition
                '(not (org-in-src-block-p t))))
  (add-hook 'org-mode-hook #'lem-yas-org-mode-hook)
  ;; suppress warnings when expanding
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-global-mode 1))

;; the official snippet collection https://github.com/AndreaCrotti/yasnippet-snippets
(use-package yasnippet-snippets
  :straight (:type git :host github :repo "AndreaCrotti/yasnippet-snippets")
  :after (yasnippet)
  :config
  (setq yasnippet-snippets-dir (concat lem-all-snippets-dir "yasnippet-snippets")))


;;;; Completion Icons
(use-package all-the-icons-completion
  :straight (:host github :repo "iyefrat/all-the-icons-completion")
  :if (display-graphic-p)
  :hook (after-init . all-the-icons-completion-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lem-setup-completion)
;;; lem-setup-completion.el ends here
