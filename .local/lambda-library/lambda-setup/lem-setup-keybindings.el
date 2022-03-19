;;; lem-setup-keybindings.el --- summary -*- lexical-binding: t -*-

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

;; Keybindings for config. This has its disadvantages (e.g. separating functions
;; or packages from keybindings) but it also makes it the place to go to deal
;; with all keybindings.
;;; Code:


;;;; Bind Key
;; Note that bind-key comes with use-package
(use-package bind-key
  :straight nil
  :config
  (setq bind-key-describe-special-forms t))

;;;; Personal Keybindings Prefix
(defvar lem-prefix "C-c C-SPC"
  "Prefix for all personal keybinds.")

;;;; Personal Leader Key

(defvar lem+leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> key, for use with modal keybindings.")

;; Use lem-prefix as leader namespace
(bind-keys :prefix-map lem+leader-map
           :prefix lem-prefix)

;;;; Meow Setup
(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)

  (meow-motion-overwrite-define-key
   '("s-[" . lem/previous-user-buffer)
   '("s-]" . lem/next-user-buffer)
   '("s-{" . tab-bar-switch-to-prev-tab)
   '("s-}" . tab-bar-switch-to-next-tab)
   '("j" . meow-next)
   '("k" . meow-prev))

  ;; Set leader This isn't the sanctioned way to do this, but it seems to be the
  ;; only way to get `leader' to properly display keys from
  ;; `meow-leader-define-key' and my personal keymap in `lem+leader-map' I think
  ;; the preferred way is via (setq meow-keypad-leader-dispatch "...") but
  ;; that doesn't work as i want it to
  (add-to-list 'meow-keymap-alist (cons 'leader lem+leader-map))
  ;; Keypad prefixes hijack personal keybinds so disable them
  ;; See https://github.com/meow-edit/meow/issues/206
  (setq meow-keypad-meta-prefix nil
        meow-keypad-ctrl-meta-prefix nil
        meow-keypad-literal-prefix nil
        meow-keypad-start-keys nil)

  (meow-leader-define-key

   ;;  ;; here we create bindings for necessary, high frequency commands
   '("?" . consult-apropos)
   ;; high frequency keybindings
   '(")" . "C-)")
   '("}" . "C-}")
   '("." . "M-.")
   '("[" . lem/previous-user-buffer)
   '("]" . lem/next-user-buffer)
   '("{" . tab-bar-switch-to-prev-tab)
   '("}" . tab-bar-switch-to-next-tab)
   '("TAB" . lem/tab-bar-select-tab-dwim)
   '("SPC" . execute-extended-command)
   ;; high frequency commands
   '(";" . comment-line)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet)
   '("=" . hl-line-mode)
   '("a" . consult-org-agenda)
   '("b" . lem+buffer-keys)
   '("c" . lem+comment-wrap-keys)
   '("C" . lem+config-keys)
   '("d" . dired-jump)
   '("D" . dired-jump-other-window)
   '("e" . lem+eval-keys)
   '("E" . restart-emacs-start-new-emacs)
   '("f" . lem+file-keys)
   '("F" . lem+flycheck-keys)
   '("i" . lem/find-files-setup-config-directory)
   '("I" . lem/search-setup-config-files)
   '("j" . avy-goto-char-timer)
   '("J" . crux-top-join-line)
   '("k" . consult-yank-from-kill-ring)
   '("l" . vertico-repeat)
   '("L" . consult-locate)
   '("M" . lem+compile-keys)
   '("n" . lem+notes-keys)
   '("N" . consult-notes-search-all)
   `("p" . ,project-prefix-map)
   '("q" . lem+quit-keys)
   '("r" . consult-register)
   '("R" . consult-recent-file)
   '("s" . lem+search-keys)
   '("S" . lem/search-in-input-dir)
   '("t" . lem+toggle-keys)
   '("u" . lem+user-keys)
   '("v" . lem+vc-keys)
   '("V" . multi-vterm-dedicated-toggle)
   '("w" . lem+window-keys)
   '("W" . lem+workspace-keys)
   '("y" . yas-minor-mode-map)
   )


  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '(":" . meow-goto-line)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . beginning-of-buffer)
   '("G" . end-of-buffer)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . overwrite-mode)
   '("s" . meow-kill)
   '("S" . embrace-commander)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-swap-grab)
   '("y" . meow-clipboard-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("&" . meow-query-replace-regexp)
   '("%" . meow-query-replace)
   '("=" . meow-grab)
   '("s-[" . lem/previous-user-buffer)
   '("s-]" . lem/next-user-buffer)
   '("s-{" . tab-bar-switch-to-prev-tab)
   '("s-}" . tab-bar-switch-to-next-tab)
   '("<escape>" . meow-cancel-selection)))

;;;; Meow
;; Note load Meow before loading personal keybindings, otherwise some might get clobbered
(use-package meow
  :straight (:type git :host github :repo "meow-edit/meow")
  :config
  ;; set colors in bespoke theme
  (setq meow-use-dynamic-face-color nil)
  (setq meow-use-cursor-position-hack t)
  ;; Make sure delete char means delete char
  ;; see https://github.com/meow-edit/meow/issues/112
  (setq meow--kbd-delete-char "<deletechar>")
  (setq meow-use-clipboard t)
  (setq meow-goto-line-function 'consult-goto-line)
  (meow-thing-register 'angle '(regexp "<" ">") '(regexp "<" ">"))
  (add-to-list 'meow-char-thing-table '(?a . angle))

  (meow-setup)
  (meow-global-mode 1))

;;;; Personal Keybindings by Group
;;;;; Buffer Keys
(bind-keys :prefix-map lem+buffer-keys
           :prefix (concat lem-prefix " b")
           ("a" . ibuffer)
           ("b" . consult-buffer)
           ("c" . lem/copy-whole-buffer-to-clipboard )
           ("d" . kill-buffer-and-window             )
           ("e" . lem/email-save-and-kill            )
           ("E" . erase-buffer                       )
           ("f" . reveal-in-osx-finder               )
           ("i" . ibuffer-jump                       )
           ("j" . lem/jump-in-buffer                 )
           ("J" . consult-imenu                      )
           ("k" . lem/kill-this-buffer               )
           ("K" . crux-kill-other-buffers            )
           ("m" . consult-global-mark                )
           ("n" . lem/create-new-buffer              )
           ("N" . lem/new-buffer-new-frame           )
           ("p" . consult-project-buffer             )
           ("r" . revert-buffer                      )
           ("R" . crux-rename-buffer-and-file        )
           ("s" . consult-buffer-other-window        )
           ("S" . hydra-spelling/body                )
           ("t" . lem/open-dir-in-iterm              )
           ("[" . lem/previous-user-buffer           )
           ("]" . lem/next-user-buffer               )
           ("{" . tab-bar-switch-to-prev-tab         )
           ("}" . tab-bar-switch-to-next-tab         )
           ("<backtab>" . crux-switch-to-previous-buffer)
           ("TAB" . lem/tab-bar-select-tab-dwim      )
           ("C-M-t" . tab-bar-new-tab                ))

;;;;; Comment Keybindings
(bind-keys :prefix-map lem+comment-wrap-keys
           :prefix (concat lem-prefix " c")
           ("c" . comment-dwim)
           ("d" . crux-duplicate-and-comment-current-line-or-region)
           ("l" . comment-line)
           ("o" . org-block-wrap)
           ("y" . lem/yaml-wrap))

;;;;; Config Keybindings
(bind-keys :prefix-map lem+config-keys
           :prefix (concat lem-prefix " C")
           ("a" . lem/setup-kill-and-archive-region     )
           ("c" . goto-custom.el                        )
           ("d" . goto-dotfiles.org                     )
           ("D" . goto-emacs-dir                        )
           ("e" . goto-early-init.el                    )
           ("f" . lem/find-files-setup-config-directory )
           ("k" . lem/byte-compile-dotemacs             )
           ("K" . lem/delete-byte-compiled-files        )
           ("l" . load-config                           )
           ("i" . goto-init.el                          )
           ("I" . lem/load-init-file                    )
           ("o" . goto-org-files                        )
           ("p" . goto-pandoc-config                    )
           ("s" . lem/search-setup-config-files         ))

;;;;; Compile Keybindings
(bind-keys :prefix-map lem+compile-keys
           :prefix (concat lem-prefix " M")
           ("m"  . compile                  )
           ("M"  . multi-compile-run        )
           ("e"  . compile-goto-error       )
           ("k"  . lem/compile-next-makefile)
           ("K"  . kill-compilation         )
           ("r"  . recompile                )
           ("v"  . lem/make-move            ))

;;;;; Eval Keybindings
(bind-keys :prefix-map lem+eval-keys
           :prefix (concat lem-prefix " e")
           ("b"  . eval-buffer )
           ("c"  . lem/eval-current-form)
           ("e"  . eval-last-sexp)
           ("f"  . eval-defun)
           ("r"  . eval-last-region))

;;;;; File Keybindings
(bind-keys :prefix-map lem+file-keys
           :prefix (concat lem-prefix " f")
           ("b" . consult-bookmark                 )
           ("f" . find-file                        )
           ("l" . consult-locate                   )
           ("o" . crux-open-with                   )
           ("s" . save-buffer                      )
           ("r" . consult-recent-file              )
           ("y" . lem/show-and-copy-buffer-filename))

;;;;; Linting (Flycheck)
(bind-keys :prefix-map lem+flycheck-keys
           :prefix (concat lem-prefix " F"))

;;;;; Mail Keybindings
(bind-keys :prefix-map lem+mail-keys
           :prefix (concat lem-prefix " m")
           ("m" .  lem/open-email-in-workspace)
           ("c" .  mu4e-compose-new           )
           ("i" .  lem/go-to-mail-inbox       )
           ("k" .  lem/mu-kill-server         )
           ("s" .  mu4e-update-mail-and-index )
           ("S" .  lem/swiftbar-email-update  )
           ("u" .  lem/go-to-mail-unread      ))

;;;;; Quit Keybindings
(bind-keys :prefix-map lem+quit-keys
           :prefix (concat lem-prefix " q")
           ("d" . lem/kill-emacs-capture-daemon)
           ("q" . save-buffers-kill-emacs      )
           ("Q" . lem/kill-all-emacsen         )
           ("r" . restart-emacs                ))

;;;;; Search Keybindings
(bind-keys :prefix-map lem+search-keys
           :prefix (concat lem-prefix " s")
           ("a" . consult-org-agenda           )
           ;; search current buffer's directory
           ("d" . consult-ripgrep              )
           ;; search with directory input
           ("D" . lem/search-in-input-dir      )
           ("b" . consult-multi-occur          )
           ("f" . consult-line                 )
           ("h" . consult-org-heading          )
           ("j" . lem/forward-or-backward-sexp )
           ("k" . consult-yank-pop             )
           ("l" . selectrum-repeat             )
           ("n" . consult-notes-search-all     )
           ("p" . consult-line-symbol-at-point )
           ("r" . vr/query-replace             )
           ("s" . consult-line                 )
           ;; search for next spelling error
           ("S" . lem/flyspell-ispell-goto-next-error)
           ("t" . lem/hydra-todo/body))

;;;;; Toggle Keybindings
(bind-keys :prefix-map lem+toggle-keys
           :prefix (concat lem-prefix " t")
           ("b" . buffer-line-mode            )
           ("g" . git-gutter-mode             )
           ("h" . hl-line-mode                )
           ("H" . hidden-mode-line-mode       )
           ("e" . toggle-indicate-empty-lines )
           ("E" . eldoc-mode                  )
           ("F" . flycheck-mode               )
           ("m" . lem/toggle-display-markup   )
           ("n" . display-line-numbers-mode   )
           ("N" . org-numbers-overlay-mode    )
           ("o" . imenu-list-smart-toggle     )
           ("p" . puni-global-mode            )
           ("P" . show-paren-mode             )
           ("r" . rainbow-identifiers-mode    )
           ("s" . flyspell-mode               )
           ("S" . ispell-buffer               )
           ("t" . toggle-dark-light-theme     )
           ("T" . lem/load-theme              )
           ("w" . writeroom-mode              )
           ("z" . zone                        ))

;;;;; User Keybindings
(bind-keys :prefix-map lem+user-keys
           :prefix (concat lem-prefix " u")
           ("a" .  lem/jump-to-org-super-agenda                 )
           ("c" . lem/find-files-setup-config-directory         )
           ("C" . lem/search-setup-config-files                 )
           ("d" .  osx-dictionary-search-input                  )
           ("m" .  lem/org-to-markdown                          )
           ("g" .  org-mac-grab-link                            )
           ("h" .  lem/org-export-to-buffer-html-as-body        )
           ("i" .  lem/org-goto-inbox                           )
           ("k" .  kill-compilation                             )
           ("l" .  desktop-read                                 )
           ("o" .  lem/markdown-to-org                          )
           ("O" .  lem/goto-org-files                           )
           ("p" .  run-pandoc                                   )
           ("P" .  lem/pandoc-pdf-open                          )
           ("s" .  sb-expand-current-file                       )
           ("S" .  just-one-space                               )
           ("t" .  lem/jump-to-org-agenda-all-todos             )
           ("j" .  lem/goto-journal                             )
           ("u" .  lem/straight-update-packages-asynchronously  )
           ("w" .  count-words                                  )
           ("W" .  lem/jump-to-week-agenda                      ))

;;;;; Version Control (Git) Keybindings
(bind-keys :prefix-map  lem+vc-keys
           :prefix (concat lem-prefix " g")
           ("b" .  magit-blame                 )
           ("c" .  magit-commit                )
           ("d" .  magit-diff                  )
           ("h" .  hydra-git-gutter/body       )
           ("l" .  magit-log                   )
           ;; show history of selected region
           ("L" .  magit-log-buffer-file       )
           ("n" .  git-gutter:next-hunk        )
           ("p" .  git-gutter:previous-hunk    )
           ;; quick commit file
           ("q" .  vc-next-action              )
           ("r" .  magit-reflog                )
           ("s" .  magit-status                ))

;;;;; Window Keybindings
(bind-keys :prefix-map lem+window-keys
           :prefix (concat lem-prefix " w")
           ("0" .  winum-select-window-0           )
           ("1" .  winum-select-window-1           )
           ("2" .  winum-select-window-2           )
           ("3" .  winum-select-window-3           )
           ("4" .  winum-select-window-4           )
           ("a" .  ace-window                      )
           ("f" .  lem/toggle-window-split         )
           ("c" .  delete-window                   )
           ("d" .  delete-window                   )
           ("h" .  lem/split-window-below-and-focus)
           ("H" .  split-window-below              )
           ("m" .  delete-other-windows            )
           ("o" .  lem/other-window                )
           ("r" .  lem/rotate-windows              )
           ("R" .  lem/rotate-windows-backward     )
           ("t" .  tear-off-window                 )
           ("u" .  winner-undo                     )
           ("U" .  winner-redo                     )
           ("v" .  lem/split-window-right-and-focus)
           ("V" .  split-window-right              )
           ("w" .  lem/other-window                )
           ("x" .  lem/window-exchange-buffer      )
           ("-" .  split-window-below              )
           ("_" .  lem/split-window-below-and-focus))

;;;;; Workspace Keybindings
(bind-keys :prefix-map lem+workspace-keys
           :prefix (concat lem-prefix " W")
           ("c"  .  emacs-workspaces/create-workspace                   )
           ("d"  .  emacs-workspaces/close-workspace                    )
           ("k"  .  emacs-workspaces/kill-buffers-close-workspace       )
           ("n"  .  emacs-workspaces/create-new-project-and-workspace   )
           ("p"  .  emacs-workspaces/project-switch-project-open-file   )
           ("s"  .  emacs-workspaces/switch-to-or-create-workspace      )
           ("w"  .  emacs-workspaces/open-existing-project-and-workspace))

;;;;; Zettelkasten/Notes/Wiki
(bind-keys :prefix-map lem+notes-keys
           :prefix (concat lem-prefix " n")
           ("c"  .  org-roam-capture        )
           ("C"  .  citar-open-notes        )
           ("i"  .  org-roam-node-insert    )
           ("f"  .  org-roam-node-find      )
           ("g"  .  org-roam-graph          )
           ("n"  .  consult-notes           )
           ("N"  .  org-roam--new-file-named)
           ("r"  .  lem/find-note-relation  )
           ("s"  .  consult-notes-search-all)
           ("t"  .  org-roam-buffer-toggle))



;;;; Which Key
(use-package which-key
  ;; :after general
  :defer 1
  :diminish ""
  :config
  ;; Allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  ;; Set the time delay (in seconds) for the which-key popup to appear.
  ;; make sure which-key doesn't show normally but refreshes quickly after it is
  ;; triggered.
  (setq which-key-idle-delay .75)
  (setq which-key-idle-secondary-delay 0.05)
  ;; use widow
  (setq which-key-popup-type 'side-window)
  (setq which-key-allow-imprecise-window-fit t)
  (setq which-key-side-window-location 'top)
  ;; use minibuffer
  ;; (which-key-setup-minibuffer)
  ;; separator
  (setq which-key-separator " → ")
  (which-key-mode))

;;;; Hydras
;;;;; Hydra Rectangle
(with-eval-after-load 'hydra
  (defhydra hydra-rectangle (:body-pre (rectangle-mark-mode 1)
                             :color pink
                             :hint nil
                             :post (deactivate-mark))
    "
  ^_k_^       _w_ copy      _o_pen       _N_umber-lines            |\\     -,,,--,,_
_h_   _l_     _y_ank        _t_ype       _e_xchange-point          /,`.-'`'   ..  \-;;,_
  ^_j_^       _d_ kill      _c_lear      _r_eset-region-mark      |,4-  ) )_   .;.(  `'-'
^^^^          _u_ndo        _g_ quit     ^ ^                     '---''(./..)-'(_\_)
"
    ("k" rectangle-previous-line)
    ("j" rectangle-next-line)
    ("h" rectangle-backward-char)
    ("l" rectangle-forward-char)
    ("d" kill-rectangle)                    ;; C-x r k
    ("y" yank-rectangle)                    ;; C-x r y
    ("w" copy-rectangle-as-kill)            ;; C-x r M-w
    ("o" open-rectangle)                    ;; C-x r o
    ("t" string-rectangle)                  ;; C-x r t
    ("c" clear-rectangle)                   ;; C-x r c
    ("e" rectangle-exchange-point-and-mark) ;; C-x C-x
    ("N" rectangle-number-lines)            ;; C-x r N
    ("r" (if (region-active-p)
             (deactivate-mark)
           (rectangle-mark-mode 1)))
    ("u" undo nil)
    ("g" nil)))

;;; Markdown Keybindings

;; (lem-leader-def
;;   ""   '(nil :which-key "Local Leader")
;;   "c"  '(:ignore t :which-key "command")
;;   "h"  '(:ignore t :which-key "insert")
;;   "i"  '(:ignore t :which-key "lists")
;;   "t"  '(:ignore t :which-key "text")

;;   ;; Movement
;;   "{"   'markdown-backward-paragraph
;;   "}"   'markdown-forward-paragraph

;;   ;; Completion, and Cycling
;;   "]"   'markdown-complete

;;   ;; Indentation
;;   ">"   'markdown-indent-region
;;   "<"   'markdown-exdent-region

;;   ;; Buffer-wide commands
;;   "c]"  'markdown-complete-buffer
;;   "cb"  'lem/clone-buffer-and-narrow
;;   "cc"  'multi-compile-run
;;   "cl"  'markdown-live-preview-mode
;;   "cm"  'markdown-other-window
;;   "cn"  'markdown-cleanup-list-numbers
;;   "co"  'markdown-open
;;   "cp"  'markdown-preview
;;   "cr"  'markdown-check-refs
;;   "cv"  'markdown-export-and-preview
;;   "cw"  'markdown-kill-ring-save

;;   ;; headings
;;   "hi"  'markdown-insert-header-dwim
;;   "hI"  'markdown-insert-header-setext-dwim
;;   "h1"  'markdown-insert-header-atx-1
;;   "h2"  'markdown-insert-header-atx-2
;;   "h3"  'markdown-insert-header-atx-3
;;   "h4"  'markdown-insert-header-atx-4
;;   "h5"  'markdown-insert-header-atx-5
;;   "h6"  'markdown-insert-header-atx-6
;;   "h!"  'markdown-insert-header-setext-1
;;   "h@"  'markdown-insert-header-setext-2

;;   ;; Insertion of common elements
;;   "-"   'markdown-insert-hr
;;   "if"  'markdown-insert-footnote
;;   "ii"  'markdown-insert-image
;;   "ik"  'spacemacs/insert-keybinding-markdown
;;   "iI"  'markdown-insert-reference-image
;;   "il"  'markdown-insert-link
;;   "iL"  'markdown-insert-reference-link-dwim
;;   "iw"  'markdown-insert-wiki-link
;;   "iu"  'markdown-insert-uri

;;   ;; Element removal
;;   "k"   'markdown-kill-thing-at-point

;;   ;; Numbering
;;   "n"   #'markdown-cleanup-list-numbers
;;   ;; List editing
;;   "li"  'markdown-insert-list-item

;;   ;; region manipulation
;;   "tb"  'markdown-insert-bold
;;   "ti"  'markdown-insert-italic
;;   "tc"  'markdown-insert-code
;;   "tC"  'markdown-insert-gfm-code-block
;;   "tq"  'markdown-insert-blockquote
;;   "tQ"  'markdown-blockquote-region
;;   "tp"  'markdown-insert-pre
;;   "tP"  'markdown-pre-region
;;   "tn"  'lem/narrow-or-widen-dwim

;;   ;; Following and Jumping
;;   "N"   'markdown-next-link
;;   "f"   'markdown-follow-thing-at-point
;;   "P"   'markdown-previous-link
;;   "<RET>" 'markdown-do

;;   "gj"    #'markdown-next-visible-heading
;;   "gk"    #'markdown-previous-visible-heading
;;   ;; Assumes you have a markdown renderer plugin in chrome
;;   "M-r"   #'browse-url-of-file
;;   "h]"    #'markdown-next-visible-heading
;;   "h["    #'markdown-previous-visible-heading
;;   "p["    #'markdown-promote
;;   "p]"    #'markdown-demote
;;   "l["    #'markdown-next-link
;;   "l]"    #'markdown-previous-link
;;   )



;; (general-define-key
;;  :keymaps 'markdown-mode-map
;;  "RET"    #'markdown-follow-thing-at-point)

;; ;; Show which-key top-level bindings
;; (global-set-key (kbd "H-k") 'which-key-show-top-level)
;; ;; override evil insert for kill line
;; (general-define-key :states '(insert) "C-k" 'kill-line)

;; ;; ;;; Package Keybindings
;; ;; (lem-leader-def
;; ;;   "P" '(:ignore t :which-key "Packages")
;; ;;   "Pl" 'paradox-list-packages
;; ;;   "Pu" 'paradox-upgrade-packages
;; ;;   "Pc" 'finder-commentary
;; ;;   )


;;; Org Keybindings
;;   ;; normal & insert state shortcuts.


;; (general-define-key
;;  :states '(emacs)
;;  :keymaps 'org-mode-map
;;  :prefix "C-c C-o"
;;  ""    '(nil :which-key "Local Leader")
;;  "<tab>" #'org-cycle
;;  "RET" #'lem/org-archive-done-tasks
;;  "SPC" #'org-toggle-checkbox
;;  "."   #'org-cycle-agenda-files
;;  "/"   #'org-sparse-tree
;;  "="   #'org-align-tags
;;  "?"   #'org-tags-view
;;  ":"   #'org-set-tags
;;  "a"   #'lem/jump-to-org-super-agenda
;;  "A"   #'org-archive-subtree
;;  "b"   #'lem/clone-buffer-and-narrow
;;  "B"   #'org-babel-tangle
;;  "c"   #'org-capture
;;  "d"   #'org-time-stamp
;;  "D"   #'org-deadline
;;  "e"   #'org-edit-special
;;  "f"   #'org-fill-paragraph
;;  "n"   #'lem/narrow-or-widen-dwim
;;  "r"   #'org-refile
;;  "s"   #'org-schedule
;;  "t"   #'lem/org-select-tags-completing-read
;;  "T"   #'org-todo
;;  "v"   #'variable-pitch-mode
;;  "l"   #'org-insert-link
;;  "L"   #'org-store-link
;;  "+"   #'org-timestamp-up-day
;;  "-"   #'org-timestamp-down-day
;;  "<"   #'org-metaleft
;;  ">"   #'org-metaright

;;  "i"  '(:ignore t :which-key "Insert...")
;;  "il" #'org-insert-link
;;  "if" #'org-footnote-new

;;  "R"  '(:ignore t :which-key "RevealJS..." )
;;  "Rr" #'lem/reveal-to-html-open
;;  "Rs" #'lem/narrowed-subtree-to-html
;;  "RS" #'org-reveal-export-current-subtree
;;  "Rp" #'lem/reveal-to-pdf)

;; (general-define-key
;;  :states '(emacs)
;;  :keymaps 'org-agenda-mode-map
;;  :prefix "C-c C-a"
;;  "" nil
;;  "<escape>" #'org-agenda-Quit
;;  "E"   #'org-agenda-entry-text-mode
;;  "m"   #'org-agenda-month-view
;;  "C-j" #'org-agenda-next-item
;;  "C-k" #'org-agenda-previous-item
;;  "C-n" #'org-agenda-next-item
;;  "C-p" #'org-agenda-previous-item)

;; ;; (general-define-key :states '(normal) :keymaps 'org-mode-map
;; ;;   "RET" 'org-open-at-point     ;; Open with return in evil
;; ;;   "p"   'org-yank ;; better pasting behavior
;; ;;   "s-J" 'crux-top-join-line)

;; ;; ;;   normal, insert, visual shortcuts
;; ;; (general-define-key :states '(normal insert visual) :keymaps 'org-mode-map
;; ;;   "M-q" #'lem/fill-or-unfill
;; ;;   "C-t" #'transpose-chars)




;;; End keybindings
(provide 'lem-setup-keybindings)

;;; lem-setup-keybindings.el ends here
