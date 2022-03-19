;; setup-transient.el --- -*- lexical-binding: t -*-
;; https://github.com/olivertaylor/dotfiles/blob/master/emacs/init.el
;; and https://www.reddi 1t.com/r/emacs/comments/mujxm7/weekly_tipstricketc_thread/gv8jxz5?utm_source=share&utm_medium=web2x&context=3


;;; Commentary

;; I LOVE transient commands. Basically, I don't want to learn a lot of
;; keybindings, I want a simple set of bindings that reveal the vast power
;; at your fingertips. So any time I find myself using a mode/package that I
;; can't remember the bindings/commands for I take the time to comb through
;; the source code, find anything I think I might use, and pop it in a
;; transient.


;;; General Purpose Transients


(define-transient-command oht-transient-general ()
  "General-purpose transient.

I use this transient command as a jumping-off point. Many of the
more following specific transients are included here. The idea is
that, when lost, one can simply call this one transient and get
wherever you need to go."
  [[ "Misc"
     ("f" "Find File" find-file)
     ("m" "Magit Status" magit-status)
     ("o" "Outline" consult-outline)
     ("a" "AutoFill" auto-fill-mode)
     ("j" "Dired Jump" dired-jump)
     ("s" "Store Org Link" org-store-link)]
   ["Commands"
    ("c s" "Spelling..." oht-transient-spelling)
    ;; ("c d" "Dictionary" sdcv-search)
    ("c f" "Fonts..." oht-transient-fonts)
    ;; ("c c" "Composition Mode" composition-mode)
    ("c m" "Toggle Minor Modes" consult-minor-mode-menu)
    ("c h" "Help..." oht-transient-help)]
   [ "Bookmarks"
     ("b s" "Set" bookmark-set)
     ("b l" "List" list-bookmarks)
     ("b j" "Jump" consult-bookmark)]
   [ "Display"
     ("d h" "Highlight Line" hl-line-mode)
     ("d l" "Line Numbers" global-display-line-numbers-mode)
     ("d g" "Fill Column" global-display-fill-column-indicator-mode)
     ("d w" "Wrap" visual-line-mode)
     ("d t" "Truncate" toggle-truncate-lines)
     ("d W" "Whitespace" whitespace-mode)]
   ["Tab Bar"
    ("t t" "Tab Bar Mode" tab-bar-mode)
    ("t n" "New" tab-bar-new-tab)
    ("t k" "Kill" tab-bar-close-tab)
    ("t z" "Undo Kill" tab-bar-undo-close-tab)
    ("t ]" "Next" tab-bar-switch-to-next-tab)
    ("t [" "Previous" tab-bar-switch-to-prev-tab)]])


(define-transient-command oht-transient-dispatch ()
  "Jump directly to your most-used stuff."
  ["Work"
   [("t" "Today + Priority" oht-org-agenda-today)
    ("0" "Week + TODOs" oht-org-agenda-complete)
    ("a" "Agenda" oht-org-agenda-agenda)
    ("T" "TODOs" oht-org-agenda-todos)
    ("A" "Org Agenda Command..." org-agenda)]
   [("m" "Mail" mu4e)]]
  ["Browsing"
   [("e" "Elfeed"      elfeed)
    ("h" "Hacker News" hackernews)]
   [("E" "EWW"         prot-eww-browse-dwim)
    ("n" "NPR News"    oht-dispatch-NPR-news)
    ("c" "CNN News"    oht-dispatch-CNN-news)
    ("g" "Google News" oht-dispatch-google-news)]
   [("d" "Downloads"   oht-dispatch-downloads)
    ("r" "Reading"     oht-dispatch-reading)
    ("w" "Watch"       oht-dispatch-watch)]])


(define-transient-command oht-transient-window ()
  "Most commonly used window commands"
  [["Splits"
    ("s" "Horizontal" split-window-below)
    ("v" "Vertical"   split-window-right)
    ("b" "Balance"    balance-windows)
    ("f" "Fit"        fit-window-to-buffer)
    ("r" "Rotate"     oht/rotate-window-split)]
   ["Window"
    ("c" "Clone Indirect" clone-indirect-buffer)
    ("t" "Tear Off" tear-off-window)
    ("k" "Kill" delete-window)
    ("K" "Kill Buffer+Win"  kill-buffer-and-window)
    ("o" "Kill Others"  delete-other-windows)
    ("m" "Maximize" maximize-window)]
   ["Navigate"
    ("<left>"  "←" windmove-left  :transient t)
    ("<right>" "→" windmove-right :transient t)
    ("<up>"    "↑" windmove-up    :transient t)
    ("<down>"  "↓" windmove-down  :transient t)]
   ["Move"
    ("S-<left>"  "S-←" buf-move-left  :transient t)
    ("S-<right>" "S-→" buf-move-right :transient t)
    ("S-<up>"    "S-↑" buf-move-up    :transient t)
    ("S-<down>"  "S-↓" buf-move-down  :transient t)]
   ["Undo/Redo"
    ("s-z" "Winner Undo" winner-undo :transient t)
    ("s-Z" "Winner Redo" winner-redo :transient t)]])


(define-transient-command oht-transient-help ()
  "Transient for Helpful commands"
  [[("p" "At Point" helpful-at-point)]
   [("c" "Callable" helpful-callable)
    ("f" "Function" helpful-function)
    ("C" "Command" helpful-command)
    ("v" "Variable" helpful-variable)
    ("s" "Symbol" helpful-symbol)
    ("M" "Macro" helpful-macro)
    ("k" "Key" helpful-key)
    ("m" "Mode" helpful-mode)]
   [("u" "Update" helpful-update)
    ("V" "Visit Reference" helpful-visit-reference)
    ("K" "Kill Helpful Buffers" helpful-kill-buffers)]])


(define-transient-command oht-transient-fonts ()
  "Set Font Properties"
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Modes"
    ("v" "Var Mode" variable-pitch-mode)
    ("V" "V+ Mode" facedancer-vadjust-mode)
    ("o" "Olivetti" olivetti-mode)
    ("w" "Wrap" visual-line-mode)
    ("c" "Comp" composition-mode)
    ]
   ["Size"
    ("0" "Reset Size" text-scale-mode)
    ("=" "Larger" text-scale-increase)
    ("+" "Larger" text-scale-increase)
    ("-" "Smaller" text-scale-decrease)
    ]
   ["Other"
    ("s" "Line Spacing" facedancer-line-spacing)
    ("m" "Modus Toggle" modus-themes-toggle)]])


(define-transient-command oht-transient-2nd ()
  "Secondary Selection"
  [["Cut/Copy"
    ("xx" "Cut 2nd" oht/cut-secondary-selection)
    ("cc" "Copy 2nd" oht/copy-secondary-selection)]
   ["& Paste"
    ("xv" "Cut 2nd & Paste" oht/cut-secondary-selection-paste)
    ("cv" "Copy 2nd & Paste" oht/copy-secondary-selection-paste)]
   ["Mark"
    ("m"  "Mark Region as 2nd" oht/mark-region-as-secondary-selection)
    ("g"  "Make 2nd the Region" oht/mark-secondary-selection)
    ("d"  "Delete 2nd" oht/delete-secondary-selection)]])


(define-transient-command oht-transient-outline ()
  "Outline Navigation"
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Show/Hide"
    ("<backtab>" "Global Toggle" bicycle-cycle-global)
    ("<tab>" "Toggle Children" bicycle-cycle)
    ("o"     "Hide to This Sublevel" outline-hide-sublevels)
    ("a"     "Show All" outline-show-all)]
   ["Navigate"
    ("n" "Next" outline-next-visible-heading)
    ("p" "Previous" outline-previous-visible-heading)]
   ["Edit"
    ("M-<left>"  "Promote" outline-promote)
    ("M-<right>" "Demote"  outline-demote)
    ("M-<up>"    "Move Up" outline-move-subtree-up)
    ("M-<down>"  "Move Down" outline-move-subtree-down)]
   ["Other"
    ("s-z" "Undo" undo-fu-only-undo)
    ("s-Z" "Redo" undo-fu-only-redo)
    ("c" "Consult" consult-outline :transient nil)]])


(define-transient-command oht-transient-spelling ()
  "Spelling Interface"
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Toggle Modes"
    ("m" "Flyspell" flyspell-mode)
    ("M" "Prog Flyspell" flyspell-prog-mode)]
   ["Check"
    ("b" "Buffer" flyspell-buffer)
    ("r" "Region" flyspell-region)]
   ["Correction"
    ("n" "Next" flyspell-goto-next-error)
    ("<return>" "Fix" flyspell-correct-wrapper)
    ("<SPC>" "Auto Fix" flyspell-auto-correct-word)
    ("<DEL>" "Delete Word" kill-word)
    ("s-z" "Undo" undo-fu-only-undo)
    ("s-Z" "Redo" undo-fu-only-redo)]])


;;; Mode-Specific Transients

(define-transient-command oht-transient-org ()
  "Transient for Org Mode"
  [["Navigation"
    ("o" "Outline" consult-outline)
    ("n" "Narrow" org-narrow-to-subtree)
    ("w" "Widen" widen)
    ("g" "Go To" org-goto)
    ("m" "Visible Markup" visible-mode)]
   ["Item"
    ("t" "TODO" oht-transient-org-todo)
    ("I" "Clock In" org-clock-in)
    ("O" "Clock Out" org-clock-out)
    ("a" "Archive Subtree" org-archive-subtree)
    ("r" "Refile" org-refile)
    ("c" "Checkbox" org-toggle-checkbox)]
   ["Insert"
    ("." "Insert Date, Active" oht/org-insert-date-today)
    (">" "Insert Date, Inactive" oht/org-insert-date-today-inactive)
    ("<" "Structure Template" org-insert-structure-template)]
   ["Links"
    ("s" "Store Link" org-store-link)
    ("i" "Insert Link" org-insert-last-stored-link)]])


(define-transient-command oht-transient-org-agenda ()
  "A transient for setting org-agenda todo status.

I've created this because I don't like how org-todo messes with
windows. There is likely a much better way to automatically map
org-todo-keywords to a transient command."
  ["Change Status To..."
   [("t" "TODO"     org-agenda-todo-set-todo)
    ("l" "LATER"    org-agenda-todo-set-later)]
   [("d" "DONE"     org-agenda-todo-set-done)
    ("c" "CANCELED" org-agenda-todo-set-canceled)]])


(define-transient-command oht-transient-org-todo ()
  "A transient for setting org todo status.

I've created this because I don't like how org-todo messes with
windows. There is likely a much better way to automatically map
org-todo-keywords to a transient command."
  ["Change Status To..."
   [("t" "TODO"     org-todo-set-todo)
    ("l" "LATER"    org-todo-set-later)]
   [("d" "DONE"     org-todo-set-done)
    ("c" "CANCELED" org-todo-set-canceled)]])


(define-transient-command oht-transient-dired ()
  "Dired commands."
  [["Action"
    ("RET" "Open file"            dired-find-file)
    ("o" "  Open in other window" dired-find-file-other-window)
    ("C-o" "Open in other window (No select)" dired-display-file)
    ("v" "  Open file (View mode)"dired-view-file)
    ("=" "  Diff"                 dired-diff)
    ("w" "  Copy filename"        dired-copy-filename-as-kill)
    ("W" "  Open in browser"      browse-url-of-dired-file)
    ("y" "  Show file type"       dired-show-file-type)]
   ["Attribute"
    ("R"   "Rename"               dired-do-rename)
    ("G"   "Group"                dired-do-chgrp)
    ("M"   "Mode"                 dired-do-chmod)
    ("O"   "Owner"                dired-do-chown)
    ("T"   "Timestamp"            dired-do-touch)]
   ["Navigation"
    ("j" "  Goto file"            dired-goto-file)
    ("+" "  Create directory"     dired-create-directory)
    ("<" "  Jump prev directory"  dired-prev-dirline)
    (">" "  Jump next directory"  dired-next-dirline)
    ("^" "  Move up directory"    dired-up-directory)]
   ["Display"
    ("g" "  Refresh buffer"       revert-buffer)
    ("l" "  Refresh file"         dired-do-redisplay)
    ("k" "  Remove line"          dired-do-kill-lines)
    ("s" "  Sort"                 dired-sort-toggle-or-edit)
    ("(" "  Toggle detail info"   dired-hide-details-mode)
    ("i" "  Insert subdir"        dired-maybe-insert-subdir)
    ("$" "  Hide subdir"          dired-hide-subdir)
    ("M-$" "Hide subdir all"      dired-hide-subdir)]
   ["Extension"
    ("e"   "wdired"               wdired-change-to-wdired-mode)
    ("/"   "dired-filter"         ignore)
    ("n"   "dired-narrow"         ignore)]]
  [["Marks"
    ("m" "Marks..." oht-transient-dired-marks)]])


(define-transient-command oht-transient-dired-marks ()
  "Sub-transient for dired."
  [["Toggles"
    ("mm"  "Mark"                 dired-mark)
    ("mM"  "Mark all"             dired-mark-subdir-files)
    ("mu"  "Unmark"               dired-unmark)
    ("mU"  "Unmark all"           dired-unmark-all-marks)
    ("mc"  "Change mark"          dired-change-marks)
    ("mt"  "Toggle mark"          dired-toggle-marks)]
   ["Type"
    ("m*"  "Executables"          dired-mark-executables)
    ("m/"  "Directories"          dired-mark-directories)
    ("m@"  "Symlinks"             dired-mark-symlinks)
    ("m&"  "Garbage files"        dired-flag-garbage-files)
    ("m#"  "Auto save files"      dired-flag-auto-save-files)
    ("m~"  "backup files"         dired-flag-backup-files)
    ("m."  "Numerical backups"    dired-clean-directory)]
   ["Search"
    ("m%"  "Regexp"               dired-mark-files-regexp)
    ("mg"  "Regexp file contents" dired-mark-files-containing-regexp)]]
  [["Act on Marked"
    ("x"   "Do action"            dired-do-flagged-delete)
    ("C"   "Copy"                 dired-do-copy)
    ("D"   "Delete"               dired-do-delete)
    ("S"   "Symlink"              dired-do-symlink)
    ("H"   "Hardlink"             dired-do-hardlink)
    ("P"   "Print"                dired-do-print)
    ("A"   "Find"                 dired-do-find-regexp)
    ("Q"   "Replace"              dired-do-find-regexp-and-replace)
    ("B"   "Elisp bytecompile"    dired-do-byte-compile)
    ("L"   "Elisp load"           dired-do-load)
    ("X"   "Shell command"        dired-do-shell-command)
    ("Z"   "Compress"             dired-do-compress)
    ("z"   "Compress to"          dired-do-compress-to)
    ("!"   "Shell command"        dired-do-shell-command)
    ("&"   "Async shell command"  dired-do-async-shell-command)]])


(define-transient-command oht-transient-eww ()
  :transient-suffix 'transient--do-stay
  :transient-non-suffix 'transient--do-warn
  [["Actions"
    ("G" "Browse" prot-eww-browse-dwim)
    ("M-<return>" "Open in new buffer" oht-eww-open-in-new-buffer-bury)
    ("&" "Browse With External Browser" eww-browse-with-external-browser)
    ("w" "Copy URL" eww-copy-page-url)]
   ["Display"
    ("i" "Toggle Images" eww-inhibit-images-toggle)
    ("F" "Toggle Fonts" eww-toggle-fonts)
    ("R" "Readable" eww-readable)
    ("M-C" "Colors" eww-toggle-colors)]
   ["History"
    ("H" "History" eww-list-histories)
    ("l" "Back" eww-back-url)
    ("r" "Forward" eww-forward-url)]
   ["Bookmarks"
    ("a" "Add Eww Bookmark" eww-add-bookmark)
    ("b" "Bookmark" bookmark-set)
    ("B" "List Bookmarks" eww-list-bookmarks)
    ("M-n" "Next Bookmark" eww-next-bookmark)
    ("M-p" "Previous Bookmark" eww-previous-bookmark)]
   ])

(define-transient-command oht-transient-info ()
  [[("d" "Info Directory" Info-directory)
    ("m" "Menu" Info-menu)
    ("F" "Go to Node" Info-goto-emacs-command-node)]
   [("s" "Search regex Info File" Info-search)
    ("i" "Index" Info-index)
    ("I" "Index, Virtual" Info-virtual-index)]]
  ["Navigation"
   [("l" "Left, History" Info-history-back)
    ("r" "Right, History" Info-history-forward)
    ("L" "List, History" Info-history)]
   [("T" "Table of Contents" Info-toc)
    ("n" "Next Node" Info-next)
    ("p" "Previous Node" Info-prev)
    ("u" "Up" Info-up)]
   [("<" "Top Node" Info-top-node)
    (">" "Final Node" Info-final-node)
    ("[" "Forward Node" Info-backward-node)
    ("]" "Backward Node" Info-forward-node)]])

;;; Customize Transients

(transient-define-prefix shk-customize-locals-menu ()
  "A menu to manipulate file/dir local variables"
  ["Local variables"
   ["File"
    ("fa" "add" add-file-local-variable)
    ("fd" "delete" delete-file-local-variable)
    ("*a" "add prop" add-file-local-variable-prop-line)
    ("*d" "delete prop" delete-file-local-variable-prop-line)]
   ["Dir"
    ("da" "add" add-dir-local-variable)
    ("dd" "delete" delete-dir-local-variable)]
   ["Move"
    ("df" "dir → file" copy-dir-locals-to-file-locals)
    ("d*" "dir → prop" copy-dir-locals-to-file-locals-prop-line)
    ("fd" "file → dir" copy-file-locals-to-dir-locals)]])

(transient-define-prefix shk-customize-menu ()
  "A menu to open customize options"
  [["Custom"
    ("," "List groups" customize)
    ("/" "Search" customize-apropos)
    ("r" "Rogue" customize-rogue)
    ("s" "Saved" customize-saved)
    ("S" "Unsaved" customize-unsaved)]
   ["Settings"
    ("o" "Option" customize-option)
    ("g" "Group" customize-group)
    ("f" "Face" customize-face)
    ("l" "Local variables" shk-customize-locals-menu)]])

;; (global-set-key (kbd "A-,") 'shk-customize-menu)
;; (global-set-key (kbd "C-c ,") 'shk-customize-menu)


;;; Ideas for Further Development

;; https://github.com/jojojames/matcha
;; https://github.com/conao3/transient-dwim.el

;; org-agenda commands
;; ibuffer commands
;; query-replace
;; occur?
;; Help
;;   - info-apropos


(provide 'setup-transient)
