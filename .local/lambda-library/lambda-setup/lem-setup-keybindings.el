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

;; Keybindings for 𝛌-Emacs. This has its disadvantages (e.g. separating
;; functions or packages from keybindings) but it also makes it the place to go
;; to deal with all keybindings. I use `bind-key' for setting bindings, which
;; comes with `use-package'. All 𝛌-Emacs keybindings are under the prefix
;; specified by `lem-prefix'.

;;; Code:


;;;; Bind Key
;; Note that bind-key comes with use-package
(use-package bind-key
  :straight t
  :config
  (setq bind-key-describe-special-forms t))

;;;; Personal Keybindings Prefix
(defcustom lem-prefix "C-c C-SPC"
  "Prefix for all personal keybinds."
  :type 'string
  :group 'lambda-emacs)

;;;; Personal Leader Key

(defcustom lem+leader-map (make-sparse-keymap)
  "An overriding keymap for <leader> key, for use with modal keybindings."
  :type 'string
  :group 'lambda-emacs)

;; Use lem-prefix as leader namespace
(bind-keys :prefix-map lem+leader-map
           :prefix lem-prefix)

;;;; Personal Keybindings by Group
;;;;; Buffer Keys
(bind-keys :prefix-map lem+buffer-keys
           :prefix (concat lem-prefix " b")
           ("a" . ibuffer)
           ("b" . consult-buffer)
           ("c" . lem-copy-whole-buffer-to-clipboard )
           ("d" . kill-buffer-and-window             )
           ("E" . erase-buffer                       )
           ("f" . reveal-in-osx-finder               )
           ("i" . consult-imenu                      )
           ("j" . lem-jump-in-buffer                 )
           ("k" . lem-kill-this-buffer               )
           ("K" . crux-kill-other-buffers            )
           ("m" . consult-global-mark                )
           ("n" . lem-create-new-buffer              )
           ("N" . lem-new-buffer-new-frame           )
           ("p" . consult-project-buffer             )
           ("r" . revert-buffer                      )
           ("R" . crux-rename-buffer-and-file        )
           ("s" . consult-buffer-other-window        )
           ("S" . hydra-spelling/body                )
           ("t" . lem-open-dir-in-iterm              )
           ("[" . lem-previous-user-buffer           )
           ("]" . lem-next-user-buffer               )
           ("{" . tab-bar-switch-to-prev-tab         )
           ("}" . tab-bar-switch-to-next-tab         )
           ("<backtab>" . crux-switch-to-previous-buffer)
           ("TAB" . lem-tab-bar-select-tab-dwim      )
           ("C-M-t" . tab-bar-new-tab                ))

;;;;; Comment Keybindings
(bind-keys :prefix-map lem+comment-wrap-keys
           :prefix (concat lem-prefix " c")
           ("c" . comment-dwim)
           ("d" . crux-duplicate-and-comment-current-line-or-region)
           ("l" . comment-line)
           ("o" . org-block-wrap)
           ("y" . lem-yaml-wrap))

;;;;; Config Keybindings
;; FIXME: fix goto functions to make them more generic
;; FIXME: fix kill-and-archive so that it creates an archive file
(bind-keys :prefix-map lem+config-keys
           :prefix (concat lem-prefix " C")
           ("a" . lem-setup-kill-and-archive-region     )
           ("c" . goto-custom.el                        )
           ("d" . goto-dotfiles.org                     )
           ("D" . goto-emacs-dir                        )
           ("e" . goto-early-init.el                    )
           ("f" . lem-find-lambda-file                  )
           ("k" . lem-byte-compile-dotemacs             )
           ("K" . lem-delete-byte-compiled-files        )
           ("l" . load-config                           )
           ("i" . goto-init.el                          )
           ("I" . lem-load-init-file                    )
           ("o" . goto-org-files                        )
           ("p" . goto-pandoc-config                    )
           ("s" . lem-search-lambda-files               ))

;;;;; Compile Keybindings
;; TODO: remove complile-next-makefile in favor of project compile?
(bind-keys :prefix-map lem+compile-keys
           :prefix (concat lem-prefix " M")
           ("m"  . compile                  )
           ("M"  . multi-compile-run        )
           ("e"  . compile-goto-error       )
           ("k"  . lem-compile-next-makefile)
           ("K"  . kill-compilation         )
           ("r"  . recompile                )
           ("v"  . lem-make-move            ))

;;;;; Eval Keybindings
(bind-keys :prefix-map lem+eval-keys
           :prefix (concat lem-prefix " e")
           ("b"  . eval-buffer )
           ("c"  . lem-eval-current-form)
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
           ("y" . lem-show-and-copy-buffer-filename))

;;;;; Linting (Flymake)
(bind-keys :prefix-map lem+flymake-keys
           :prefix (concat lem-prefix " F"        )
           ("b" . flymake-start                   )
           ("c" . consult-flymake                 )
           ("d" . flymake-show-buffer-diagnostic  )
           ("p" . flymake-show-project-diagnostics)
           ("P" . package-lint-current-buffer     )
           ("u" . use-package-lint                ))

;;;;; Mail Keybindings
(bind-keys :prefix-map lem+mail-keys
           :prefix (concat lem-prefix " m")
           ("a" . mu4e-view-save-attachment-multi)
           ("c" . mu4e-compose-new           )
           ("e" . lem-email-save-and-kill    )
           ("i" . lem-go-to-mail-inbox       )
           ("k" . lem-mu-kill-server         )
           ("m" . lem-open-email-in-workspace)
           ("s" . mu4e-update-mail-and-index )
           ("S" . lem-swiftbar-email-update  )
           ("u" . lem-go-to-mail-unread      ))

;;;;; Quit Keybindings
(bind-keys :prefix-map lem+quit-keys
           :prefix (concat lem-prefix " q")
           ("d" . lem-kill-emacs-capture-daemon)
           ("q" . save-buffers-kill-emacs      )
           ("Q" . lem-kill-all-emacsen         )
           ("r" . restart-emacs                ))

;;;;; Search Keybindings
(bind-keys :prefix-map lem+search-keys
           :prefix (concat lem-prefix " s")
           ("a" . consult-org-agenda           )
           ;; search current buffer's directory
           ("d" . consult-ripgrep              )
           ;; search with directory input
           ("D" . lem-search-in-input-dir      )
           ("b" . consult-multi-occur          )
           ("f" . consult-line                 )
           ("h" . consult-org-heading          )
           ("j" . lem-forward-or-backward-sexp )
           ("k" . consult-yank-pop             )
           ("l" . selectrum-repeat             )
           ("n" . consult-notes-search-all     )
           ("r" . vr/query-replace             )
           ("s" . consult-line                 )
           ;; search for next spelling error
           ("S" . lem-flyspell-ispell-goto-next-error)
           ("t" . lem-hydra-todo/body          )
           ("." . consult-line-symbol-at-point ))

;;;;; Toggle Keybindings
(bind-keys :prefix-map lem+toggle-keys
           :prefix (concat lem-prefix " t")
           ("b" . buffer-line-mode            )
           ("g" . git-gutter-mode             )
           ("h" . hl-line-mode                )
           ("H" . hidden-mode-line-mode       )
           ("e" . toggle-indicate-empty-lines )
           ("E" . eldoc-mode                  )
           ("F" . flymake-mode                )
           ("m" . lem-toggle-display-markup   )
           ("n" . display-line-numbers-mode   )
           ("N" . org-numbers-overlay-mode    )
           ("o" . imenu-list-smart-toggle     )
           ("p" . puni-global-mode            )
           ("P" . show-paren-mode             )
           ("r" . rainbow-identifiers-mode    )
           ("s" . flyspell-mode               )
           ("S" . ispell-buffer               )
           ("t" . toggle-dark-light-theme     )
           ("T" . lem-load-theme              )
           ("w" . writeroom-mode              )
           ("z" . zone                        ))

;;;;; User Keybindings
;; NOTE: This keymap is for user-specific keybindings. Define this in your
;; `config.el' file.
(bind-keys :prefix-map lem+user-keys
           :prefix (concat lem-prefix " u"))

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
           ("f" .  lem-toggle-window-split         )
           ("c" .  delete-window                   )
           ("d" .  delete-window                   )
           ("h" .  lem-split-window-below-and-focus)
           ("H" .  split-window-below              )
           ("m" .  delete-other-windows            )
           ("o" .  lem-other-window                )
           ("r" .  lem-rotate-windows              )
           ("R" .  lem-rotate-windows-backward     )
           ("t" .  tear-off-window                 )
           ("u" .  winner-undo                     )
           ("U" .  winner-redo                     )
           ("v" .  lem-split-window-right-and-focus)
           ("V" .  split-window-right              )
           ("w" .  lem-other-window                )
           ("x" .  lem-window-exchange-buffer      )
           ("-" .  split-window-below              )
           ("_" .  lem-split-window-below-and-focus))

;;;;; Workspace Keybindings
(bind-keys :prefix-map lem+workspace-keys
           :prefix (concat lem-prefix " W")
           ("b"  .  tabspaces-switch-to-buffer)
           ("c"  .  tabspaces-clear-buffers)
           ("d"  .  tabspaces-close-workspace)
           ("k"  .  tabspaces-kill-buffers-close-workspace)
           ("o"  .  tabspaces-open-or-create-project-and-workspace)
           ("p"  .  tabspaces-project-switch-project-open-file)
           ("r"  .  tabspaces-remove-current-buffer)
           ("R"  .  tabspaces-remove-selected-buffer)
           ("s"  .  tabspaces-switch-or-create-workspace))

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
           ("r"  .  lem-find-note-relation  )
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

;;;;; Transpose hydra
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

;; TODO: Add org and markdown keybindings
;;; End keybindings
(provide 'lem-setup-keybindings)

;;; lem-setup-keybindings.el ends here
