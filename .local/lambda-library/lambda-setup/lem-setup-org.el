;; Org Mode

;;;; New Org
;; Org package settings
(use-package org
  :straight t
  :commands (org-mode)
  :mode (("\\.org$" . org-mode))
  :bind (:map org-mode-map
         ("M-k" . org-metaup)
         ("M-j" . org-metadown)
         ("M-l" . org-metaright)
         ("M-h" . org-metaleft)
         ("M-J" . org-shiftdown)
         ("M-K" . org-shiftup)
         ("M-L" . org-shiftright)
         ("M-H" . org-shiftleft)
         ;; easily emphasize text
         ("s-b" . (lambda () (interactive) (org-emphasize-dwim ?*)))
         ("s-i" . (lambda () (interactive) (org-emphasize-dwim ?/)))
         ("s-M-`" . org-emphasize-with-verbatim-dwim)
         ("s-M-~" . org-emphasize-with-code-dwim)
         ;; better pasting behavior in org-mode
         ("s-v" . org-yank))
  :init
;;; Org Settings
;;;; Org Directories
  (setq-default org-directory "~/Dropbox/org-files/")
  (setq-default org-default-notes-file (concat org-directory "inbox.org"))
  (setq-default org-agenda-files (list org-directory))

;;;; Org Regex (Emphasis)
  ;; Set regex boundaries for emphasis.
  ;; See https://emacs.stackexchange.com/questions/54673/em-dash-before-italic-in-org-export
  ;; https://emacs.stackexchange.com/questions/54632/org-mode-monospaces-more-than-it-should

  (setq org-emphasis-regexp-components
        '("-—[:space:]('\"{["
          "\] - [:space:].,:!?;'\")}\\["
          "[:space:]"
          "."
          1))

;;;; Org Config Settings
  :config
  ;; for use with meow movement
  (modify-syntax-entry ?@ "_" org-mode-syntax-table)
  (setq-default org-footnote-section nil ;; place footnotes locally rather than in own section
                org-cycle-separator-lines 0 ;; no empty lines in collapsed view
                org-auto-align-tags nil ;; don't auto-align
                org-tags-column 0 ;; place tags directly next to headline text
                org-footnote-auto-adjust t ;; renumber footnotes
                org-startup-indented nil ;; don't start in indent mode
                org-adapt-indentation nil ;; don't adapt indentation
                org-image-actual-width  500 ;; show all images at 500px using imagemagik
                org-return-follows-link t ;; make RET follow links
                org-list-allow-alphabetical t ;; allow alphabetical list
                org-hide-emphasis-markers t  ;; hide markers
                org-pretty-entities t ;; make latex look good
                org-pretty-entities-include-sub-superscripts t
                org-hide-leading-stars t
                org-refile-use-cache t  ;; use cache for org refile
                org-startup-folded t
                org-special-ctrl-a/e t
                org-yank-adjusted-subtrees t  ;; adjust subtrees to depth when yanked
                org-yank-folded-subtrees t  ;; fold subtrees on yank
                org-insert-heading-respect-content t
                org-M-RET-may-split-line '((default . t))  ;; don't split line when creating a new headline, list item, or table field
                org-fontify-quote-and-verse-blocks t ;; make quotes stand out
                org-table-export-default-format "orgtbl-to-csv" ;; export for org-tables to csv
                org-ellipsis "…" ;; nicer elipses "↷" "↴" "▼"
                org-cycle-separator-lines 0 ;; Give a more compact and consistent view
                ;; prevent editing invisible area, and show an error message in echo area instead;
                ;; additionally expand text and move focus to the expected point.
                org-catch-invisible-edits 'smart
                org-use-fast-todo-selection 'expert ;; don't use popup window
                org-imenu-depth 8
                imenu-auto-rescan t
                ;; dont export postamble
                org-html-postamble nil
                ;; don't use caching (seems to be causing problems)
                ;; see https://emacs.stackexchange.com/questions/42006/trouble-with-org-mode
                org-element-use-cache nil)


;;;; Org Modules
  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit t)
    (add-to-list 'org-modules 'org-tempo t)
    (add-to-list 'org-modules 'org-protocol t)
    (add-to-list 'org-modules 'org-mac-link t))

;;;; Org ID
  (setq org-id-locations-file (concat lem-cache-dir ".org-id-locations"))
  (setq org-id-method 'ts) ;; use timestamp for id
  (setq org-id-link-to-org-use-id 'create-if-interactive) ;; create ids

;;;; Org State Settings
  (setq org-todo-keywords
        '((sequence "TODO(t)" "DOING(g)" "NEXT(n)" "WAITING(w@/!)" "MAYBE(m)" "SOMEDAY(s)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELED(c@/!)")))
;;;; Org Priority Settings
  (setq org-priority-faces '((?A . (:foreground "red"))
                             (?B . (:foreground "orange"))
                             (?C . (:foreground "DarkGoldenrod2"))
                             (?D . (:forefround "green"))))
;;;; Org Logging
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-state-notes-insert-after-drawers nil)
  ;; Don't log the time a task was rescheduled or redeadlined.
  (setq org-log-redeadline nil)
  (setq org-log-reschedule nil)
  ;; Prefer rescheduling to future dates and times:
  (setq org-read-date-prefer-future 'time)

;;;; Org Tags
  (setq org-tag-alist '((:startgrouptag)
                        ("@computer" . ?c)
                        (:grouptags)
                        ("emacs" . ?m)
                        (:endgrouptag)
                        ("@errand" . ?e)
                        ("@phone" . ?p)
                        ("@unl" . ?s)
                        ("email")
                        ("postal-mail")
                        ("@home" . ?h)))
  (setq org-fast-tag-selection-single-key nil)


;;;; Org Entities
  (setq org-entities-user
        '(("nec" "\Box" nil "◻" "" "" "◻")
          ("pos" "\Diamond" nil "◇" "" "" "◇")
          ("space" "~" nil "&nbsp;" " " " " " ")))

;;;; Org Template Expansions
  (setq new-structure-template-alist
        '(("el" . "src emacs-lisp")
          ("t" . "COMMENT \TODO:")
          ("b" . "REVEAL: split")
          ("f" . "ATTR_REVEAL: :frag (appear)")
          ("n" . "notes")))
  (dolist (ele new-structure-template-alist)
    (add-to-list 'org-structure-template-alist ele))

;;;; Org Hooks
  (add-hook 'org-mode-hook
            (lambda ()
              ;; (centered-cursor-mode)
              (turn-on-auto-fill)
              (visual-line-mode)))
  ;; this auto-save seems like overkill?
  ;; (add-hook 'auto-save-hook 'org-save-all-org-buffers)

;;; Org Agenda
  ;; Settings for the [[http://orgmode.org/manual/Agenda-Views.html][agenda]].
  ;; sorting
  '(org-agenda-sorting-strategy
    (quote
     ((agenda scheduled-up deadline-up)
      (todo scheduled-up deadline-up)
      (tags priority-down category-keep)
      (search category-keep))))

  ;; Display properties
  (setq org-agenda-tags-column org-tags-column
        org-agenda-window-setup 'only-window
        org-agenda-restore-windows-after-quit t
        org-agenda-todo-ignore-scheduled nil
        org-agenda-todo-ignore-deadlines nil
        org-agenda-block-separator ""
        ;; org-agenda-sticky t
        org-agenda-span 'day)

  ;; (bind-key "C-c a" #'org-agenda)
  (with-eval-after-load 'org-agenda
    (bind-keys* :map org-agenda-mode-map
      ("j" . org-agenda-next-item)
      ("k" . org-agenda-previous-item)))

  ;; automatically refresh the agenda after adding a task
  (defun lem/org-agenda-refresh ()
    (interactive)
    (when (get-buffer "*Org Agenda*")
      (with-current-buffer "*Org Agenda*"
        (org-agenda-redo)
        (message "[org agenda] refreshed!"))))
  (add-hook 'org-capture-after-finalize-hook 'lem/org-agenda-refresh)

  ;; show all todos
  (defun lem/jump-to-org-agenda-all-todos ()
    "open agenda with all unscheduled/non-deadline todos"
    (interactive)
    (org-agenda nil "z"))

  ;; jump to week agenda
  (defun lem/jump-to-week-agenda ()
    "open custom week agenda"
    (interactive)
    (org-agenda nil "W"))

  ;; from stack overflow https://stackoverflow.com/a/22900459/6277148
  ;; note that the formatting is nicer that just using '%b'
  (setq org-agenda-prefix-format
        '((agenda . " %i %-12:c%?-12t% s")
          (timeline . "  % s")
          (todo .
                " %i %-12:c %(concat \"\"(org-format-outline-path (org-get-outline-path)) \" \->\") ")
          (tags .
                " %i %-12:c %(concat \"\"(org-format-outline-path (org-get-outline-path)) \" \->\") ")
          (search . " %i %-12:c")))

;;;; Org Super-Agenda
  ;; Supercharge org-agenda: https://github.com/alphapapa/org-super-agenda
  ;; Settings courtesy of alphapapa: https://github.com/alphapapa/org-super-agenda/blob/master/examples.org#forward-looking
  (use-package org-super-agenda
    :straight (:type git :host github :repo "alphapapa/org-super-agenda")
    :commands org-super-agenda-mode
    :after org
    :bind (:map org-agenda-keymap
           ("," . lem/hydra-org-agenda/body))
    :config
    (org-super-agenda-mode)
    (setq org-super-agenda-date-format "%A, %e %b")
    (let ((two-weeks-from-today (format-time-string "%Y-%m-%d" (org-read-date nil t "+2w"))))
      (setq org-super-agenda-groups
            '(
              (:name "Today"
               :time-grid t
               :date today
               :order 1)
              (:name "Scheduled earlier"
               :scheduled past
               :order 4)
              (:name "Overdue"
               :deadline past
               :order 6)
              (:name "Due Today"
               :deadline today
               :order 8)
              (:name "Due Soon"
               :deadline future
               :order 10)
              )))

    (defun lem/jump-to-org-super-agenda ()
      (interactive)
      (require 'org)
      (require 'org-super-agenda)
      (org-agenda nil "A")))

;;;; Agenda Toggle
  (defun lem/toggle-org-agenda-file-set ()
    (interactive)
    (if (equal org-agenda-files (list org-directory))
        (setq org-agenda-files (list "~/Dropbox/Work/projects/notebook/content-org/"))
      (setq org-agenda-files (list org-directory)))
    (message "Using %s" org-agenda-files))

;;;; Agenda Navigation
  ;; Courtesy of [[https://blog.aaronbieber.com/2016/09/25/agenda-interactions-primer.html][Aaron Bieber]]
  (defun air-org-agenda-next-header ()
    "Jump to the next header in an agenda series."
    (interactive)
    (air--org-agenda-goto-header))

  (defun air-org-agenda-previous-header ()
    "Jump to the previous header in an agenda series."
    (interactive)
    (air--org-agenda-goto-header t))

  (defun air--org-agenda-goto-header (&optional backwards)
    "Find the next agenda series header forwards or BACKWARDS."
    (let ((pos (save-excursion
                 (goto-char (if backwards
                                (line-beginning-position)
                              (line-end-position)))
                 (let* ((find-func (if backwards
                                       'previous-single-property-change
                                     'next-single-property-change))
                        (end-func (if backwards
                                      'max
                                    'min))
                        (all-pos-raw (list (funcall find-func (point) 'org-agenda-structural-header)
                                           (funcall find-func (point) 'org-agenda-date-header)
                                           (funcall find-func (point) 'org-super-agenda-header)))
                        (all-pos (cl-remove-if-not 'numberp all-pos-raw))
                        (prop-pos (if all-pos (apply end-func all-pos) nil)))
                   prop-pos))))
      (if pos (goto-char pos))
      (if backwards (goto-char (line-beginning-position)))))

  ;; (with-eval-after-load 'org-agenda
  ;;   (general-define-key :keymaps 'org-agenda-mode-map :states '(normal motion)
  ;;     "J" 'air-org-agenda-next-header
  ;;     "K" 'air-org-agenda-previous-header))

  (defun air-org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
          subtree-end
        nil)))

  (defun air-org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))

;;;; Hydra for Agenda
  ;; Hydra for org agenda (graciously offered by Spacemacs)
  (with-eval-after-load 'org-agenda
    (org-super-agenda-mode)
    (defhydra lem/hydra-org-agenda (:color pink :hint none)
      "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
      ;; Entry
      ("hA" org-agenda-archive-default)
      ("hk" org-agenda-kill)
      ("hp" org-agenda-priority)
      ("hr" org-agenda-refile)
      ("h:" org-agenda-set-tags)
      ("ht" org-agenda-todo)
      ;; Visit entry
      ("o"   link-hint-open-link :exit t)
      ("<tab>" org-agenda-goto :exit t)
      ("TAB" org-agenda-goto :exit t)
      ("SPC" org-agenda-show-and-scroll-up)
      ("RET" org-agenda-switch-to :exit t)
      ;; Date
      ("dt" org-agenda-date-prompt)
      ("dd" org-agenda-deadline)
      ("+" org-agenda-do-date-later)
      ("-" org-agenda-do-date-earlier)
      ("ds" org-agenda-schedule)
      ;; View
      ("vd" org-agenda-day-view)
      ("vw" org-agenda-week-view)
      ("vt" org-agenda-fortnight-view)
      ("vm" org-agenda-month-view)
      ("vy" org-agenda-year-view)
      ("vn" org-agenda-later)
      ("vp" org-agenda-earlier)
      ("vr" org-agenda-reset-view)
      ;; Toggle mode
      ("ta" org-agenda-archives-mode)
      ("tA" (org-agenda-archives-mode 'files))
      ("tr" org-agenda-clockreport-mode)
      ("tf" org-agenda-follow-mode)
      ("tl" org-agenda-log-mode)
      ("td" org-agenda-toggle-diary)
      ;; Filter
      ("fc" org-agenda-filter-by-category)
      ("fx" org-agenda-filter-by-regexp)
      ("ft" org-agenda-filter-by-tag)
      ("fr" org-agenda-filter-by-tag-refine)
      ("fh" org-agenda-filter-by-top-headline)
      ("fd" org-agenda-filter-remove-all)
      ;; Clock
      ("cq" org-agenda-clock-cancel)
      ("cj" org-agenda-clock-goto :exit t)
      ("ci" org-agenda-clock-in :exit t)
      ("co" org-agenda-clock-out)
      ;; Other
      ("q" nil :exit t)
      ("gd" org-agenda-goto-date)
      ("." org-agenda-goto-today)
      ("gr" org-agenda-redo)))

;;;; Agenda Custom Commands
                                        ; https://orgmode.org/manual/Storing-searches.html#Storing-searches
  (setq org-agenda-custom-commands
        '(("x" agenda)
          ("y" agenda*) ; or agenda entries planned this week/day with an hour specification like [h]h:mm
          ("z" todo "TODO")
          ("i" todo "INPROGRESS")
          ("n" todo "NEXT")
          ("r" todo "REVISE")
          ("s" "Stuck Projects" (
                                 (tags-todo "-CANCELED/!"
                                            ((org-agenda-overriding-header "Stuck Projects")
                                             (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                                             (org-agenda-sorting-strategy
                                              '(category-keep))))))
          ("w" todo "WAITING")
          ("A" "Super Agenda" ((agenda "" ((org-agenda-span 'day)))))
          ;; (alltodo "" ((org-agenda-overriding-header "")
          ;; (org-super-agenda-groups
          ;;  '(
          ;;    (:name "Priority"
          ;;     :priority>= "C")
          ;;    (:name "Next to do"
          ;;     :todo "NEXT")
          ;;    (:name "In Progress"
          ;;     :todo "DOING")
          ;;    (:todo ("WAITING" "HOLD"))
          ;;    (:todo "MAYBE")
          ;;    (:name "Reading List"
          ;;     :todo "TOREAD")
          ;;    ))
          ;; )
          ("W" "Week's agenda and all TODOs"
           ((tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority unfinished tasks:")))
            (agenda "" ((org-agenda-span 'week)))
            (alltodo ""
                     ((org-agenda-skip-function '(or (air-org-skip-subtree-if-habit)
                                                     (air-org-skip-subtree-if-priority ?A)
                                                     (org-agenda-skip-if nil '(scheduled deadline))))
                      (org-agenda-overriding-header "ALL normal priority tasks:"))))
           ((org-agenda-compact-blocks nil)))))
;;; Org Capture
;;;; Capture Settings
  ;; (add-hook 'org-capture-mode-hook 'evil-insert-state)
  ;; (general-define-key
  ;;  :states '(insert normal motion emacs)
  ;;  :keymaps 'override
  ;;  "C-c c" #'org-capture)
  (setq org-capture-templates
        ;; Note the ` and , to get concat to evaluate properly
        `(("c" "Capture" entry (file ,(concat org-directory "inbox.org"))
           "* TODO %?\n %i")

          ("j" "Journal" entry (file+olp+datetree ,(concat org-directory "journal.org"))
           "**** %<%H:%M>\n%?")

          ("l" "A link, for reading later" entry (file ,(concat org-directory "inbox.org"))
           "* %? :link: \n%(grab-mac-link 'safari 'org)")

          ("m" "eMail Workflow")
          ("mc" "Comment" entry (file+olp ,(concat org-directory "Mail.org") "Mail Comment")
           "* TODO Comment re: %:fromname about %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%?  %i")
          ("mm" "Remember" entry (file+olp ,(concat org-directory "Mail.org") "Remember")
           "* TODO %:subject\nSCHEDULED:%t\nFrom: %:from \nMessage: %a \n\n  %i" :immediate-finish t)
          ("mr" "Respond" entry (file+olp ,(concat org-directory "Mail.org") "Respond")
           "* TODO Respond to %:from | %:subject \nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\nMessage: %a\n  %i" :immediate-finish t)

          ("r" "Reference" entry (file ,(concat org-directory "reference.org"))
           "* %?")

          ("s" "Music Review" entry  (file ,(concat org-directory "music.org"))
           ,(concat "\n** Artist - Album :Artist:Genre: %?\n"
	                "  - Date: %T\n"
	                "  - Listened While: \n"
	                "  - Suggested By: \n"
	                "  - Standout Tracks: \n"
	                "  - Rating: /10\n"
	                "  - Thoughts: \n"))

          ("M" "UNL Merit Review" entry (file ,(concat org-directory "merit-reviews.org"))
           (file ,(concat org-directory "templates/merit-review-template.org")))

          ("w" "Review: Weekly Review" entry (file+datetree ,(concat org-directory "reviews.org"))
           (file ,(concat org-directory "templates/weekly_review_template.org")))

          ("R" "Referee report" entry (file+datetree ,(concat org-directory "referee-reports.org"))
           (file ,(concat org-directory "templates/referee-report-template.org")))))

  ;; Add date to captured items
  (defun add-property-with-date-captured ()
    "Add DATE_CAPTURED property to the current item."
    (interactive)
    (org-set-property "DATE_CAPTURED" (format-time-string "%F %A")))

  (add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)

  ;; Add newline to captured items
  (defun lem/org-capture-newlines-at-end ()
    (goto-char (point-max))
    (insert "\n\n"))
  (add-hook 'org-capture-prepare-finalize 'lem/org-capture-newlines-at-end)

;;;; Org Journal Capture
  ;; Tell emacs what you're doing a few times a day. Depends on a
  ;; [[/Users/roambot/bin/scripts/emacs_journal.sh][shell script]] run in the
  ;; background. I got the idea from
  ;; [[http://www.diegoberrocal.com/blog/2015/08/19/org-protocol/][Diego Berrocal]].
  ;; Hat tip to
  ;; [[http://stackoverflow.com/questions/23517372/hook-or-advice-when-aborting-org-capture-before-template-selection][stack
  ;; overflow]] for help on hooks for the created frame.

  (defun lem/org-journal ()
    (interactive) (org-capture nil "j"))

;;;; Alfred Capture Workflow
  ;; Help alfred and org-capture play nice.
  ;; Courtesy of http://orgmode.org/worg/org-contrib/alfred-org-capture.html with some slight modifications.
  ;; Current functions also from https://github.com/Isimoro/org-global-capture.el/blob/master/org-global-capture.el

  (defadvice org-switch-to-buffer-other-window
      (after supress-window-splitting activate)
    "Delete the extra window if we're in a capture frame"
    (if (equal "capture" (frame-parameter nil 'name))
        (delete-other-windows)))

  (defadvice org-capture-finalize
      (after delete-capture-frame activate)
    "Advise capture-finalize to close the frame"
    (when (and (equal "capture" (frame-parameter nil 'name))
               (not (eq this-command 'org-capture-refile)))
      (delete-frame)))

  (defadvice org-capture-refile
      (after delete-capture-frame activate)
    "Advise org-refile to close the frame"
    (when (equal "capture" (frame-parameter nil 'name))
      (delete-frame)))

;;; Org Archive
  ;; Tell org where to archive completed tasks
  (setq org-archive-location (concat org-directory "/org-archive/archived.org::datetree/"))
  ;; Also tell org how to archive all the done tasks (DONE or CANCELED) in a file.
  ;; From [[https://changelog.complete.org/archives/9877-emacs-3-more-on-org-mode][here]] based on a stack overflow [[https://stackoverflow.com/a/27043756][answer]]
  (defun lem/org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'file)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/CANCELED" 'file))

;;; Org Refile
  ;; Set refile settings.  I got a lot of help on this from [[https://blog.aaronbieber.com/2017/03/19/organizing-notes-with-refile.html][Aaron Bieber's discussion]].

  ;; With this set, you can trigger Refile with C-c C-w in any Org file and
  ;; get a completing read of all headings up to three levels deep in all
  ;; files in =org-agenda-files=. You can also refile to the top header in a
  ;; document and create new parents.
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 8)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; fix refile
  (defun lem/fix-org-refile ()
    (interactive)
    (shell-command-to-string "cd ~/.emacs.d/.local/straight/build && find org*/*.elc -print0 | xargs -0 rm")
    (org-reload))

;;; Open Files in Default Application
  ;;Open files in their default applications (ms word being the prime example)
  (setq org-file-apps
        '(("\\.docx\\'" . default)
          ("\\.mm\\'" . default)
          ("\\.x?html?\\'" . default)
          ("\\.pdf\\'" . emacs)
          (auto-mode . emacs)))

  ;; Open bookends file links in bookends
  (org-add-link-type
   "bookends" 'lem/follow-bookends-link)
  (defun lem/follow-bookends-link (path)
    "run bookends link in org files"
    (shell-command-to-string (concat "open bookends:" path)))

;;; End Org Use-Package Config
  ;; end org use-package config settings
  )

;;; Org Indirect Buffer
(setq org-indirect-buffer-display 'current-window)
;; Some advice to automatically switch to a new indirect buffer upon creation
(defadvice org-tree-to-indirect-buffer (after org-tree-to-indirect-buffer-after activate) (other-window 1))

;;; Org Functions
;;;; Org Emphasis Functions
;; Adapted from https://emacs.stackexchange.com/a/14586
;; See https://emacstil.com/til/2021/11/29/org-emphasize-dwim/
(defun org-emphasize-dwim (&optional char)
  (interactive)
  (unless (region-active-p)
    (lem/maybe-mark-word))
  (org-emphasize char))

(defun org-emphasize-with-verbatim-dwim ()
  (interactive)
  (org-emphasize-dwim ?=))

(defun org-emphasize-with-code-dwim ()
  (interactive)
  (org-emphasize-dwim ?~))

(defun lem--cursor-outside-of-any-word ()
  (not (bounds-of-thing-at-point 'word)))

(defun lem--cursor-at-beginning-of-a-word ()
  (eq (point) (car (bounds-of-thing-at-point 'word))))


(defun lem/maybe-mark-word ()
  "Mark the current word. If cursor is outside of a word bounds, mark the empty position."
  (interactive)
  (unless (or (lem--cursor-outside-of-any-word) (lem--cursor-at-beginning-of-a-word))
    (backward-word))
  (unless (lem--cursor-outside-of-any-word)
    (mark-word)))

;;;; Org Fill Functions
;;  Functions to calculate apt offsets and call regular org fill stuff. There's a
;;  useful
;;  [[http://stackoverflow.com/questions/14351154/org-mode-outline-level-specific-fill-column-values][stack
;;  overflow thread]] on this.

(defun calc-offset-on-org-level ()
  "Calculate offset (in chars) on current level in org mode file."
  (* (or (org-current-level) 0) org-indent-indentation-per-level))

(defun lem-org-fill-paragraph (&optional JUSTIFY)
  "Calculate apt fill-column value and fill paragraph."
  (let* ((fill-column (- fill-column (calc-offset-on-org-level))))
    (org-fill-paragraph JUSTIFY)))

(defun lem-org-auto-fill-function ()
  "Calculate apt fill-column value and do auto-fill"
  (let* ((fill-column (- fill-column (calc-offset-on-org-level))))
    (org-auto-fill-function)))

(defun lem-org-mode-hook ()
  (setq fill-paragraph-function   'lem-org-fill-paragraph
        normal-auto-fill-function 'lem-org-auto-fill-function))
;; FIXME I think these functions might be causing org-cache problems
;; of the kind: The error was: (error "rx ‘**’ range error")
;; (add-hook 'org-load-hook 'lem-org-mode-hook)
;; (add-hook 'org-mode-hook 'lem-org-mode-hook)

;;;; Narrow & Advance/Retreat
;; Functions to advance forwards or backwards through narrowed tree
(defun lem/org-advance ()
  (interactive)
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (widen)
    (org-forward-heading-same-level 1))
  (org-narrow-to-subtree))

(defun lem/org-retreat ()
  (interactive)
  (when (buffer-narrowed-p)
    (goto-char (point-min))
    (widen)
    (org-backward-heading-same-level 1))
  (org-narrow-to-subtree))

;;;; Clone and Narrow
(defun lem/clone-buffer-and-narrow ()
  "Clone buffer and narrow outline tree"
  (interactive)
  (let ((buf (clone-indirect-buffer-other-window nil nil)))
    (with-current-buffer buf
      (cond ((derived-mode-p 'org-mode)
             (org-narrow-to-element))
            ((derived-mode-p 'markdown-mode)
             (markdown-narrow-to-subtree))))
    (switch-to-buffer-other-window buf)))

;;;; Goto Org Files
(defun lem/goto-org-files ()
  "goto org-files directory"
  (interactive)
  (let ((default-directory org-directory))
    (call-interactively 'find-file)))
(defun lem/goto-inbox.org ()
  "goto org-inbox"
  (interactive)
  (find-file (concat org-directory "inbox.org")))
(defun lem/goto-todo.org ()
  "goto org-todo"
  (interactive)
  (find-file (concat org-directory "todo.org")))
(defun lem/goto-conferences.org ()
  "goto org-conferences"
  (interactive)
  (find-file (concat org-directory "conferences.org")))
(defun lem/goto-referee-reports.org ()
  "goto org referee reports"
  (interactive)
  (find-file (concat org-directory "referee-reports.org")))
(defun lem/goto-reference.org ()
  "goto org reference notes"
  (interactive)
  (find-file (concat org-directory "reference.org")))
(defun lem/goto-someday.org ()
  "goto org-someday"
  (interactive)
  (find-file (concat org-directory "someday.org")))
(defun lem/goto-reading.org ()
  "goto reading list"
  (interactive)
  (find-file (concat org-directory "reading.org")))
(defun lem/goto-writing.org ()
  "goto writing list"
  (interactive)
  (find-file (concat org-directory "writing.org")))
(defun lem/goto-teaching.org ()
  "goto teaching file"
  (interactive)
  (find-file (concat org-directory "teaching.org")))


;;;; Export Headings as Seperate Files
;; export headlines to separate files
;; http://pragmaticemacs.com/emacs/export-org-mode-headlines-to-separate-files/ ; see also:
;; http://emacs.stackexchange.com/questions/2259/how-to-export-top-level-headings-of-org-mode-buffer-to-separate-files

;; FIXME: neither of these functions work right now for some reason.
(defun lem/org-export-headlines-to-docx ()
  "Export all subtrees that are *not* tagged with :noexport: to
    separate docx files.

    Subtrees that do not have the :EXPORT_FILE_NAME: property set
    are exported to a filename derived from the headline text."
  (interactive)
  (save-buffer)
  (let ((modifiedp (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (goto-char (re-search-forward "^*"))
      (set-mark (line-beginning-position))
      (goto-char (point-max))
      (org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
              "EXPORT_FILE_NAME"
              (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-pandoc-export-to-docx nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'region-start-level)))
  (shell-command-to-string "open ~/Dropbox/Work/Comments/Referee-Reports/ref-report.docx"))

(defun lem/org-export-headlines-to-pdf ()
  "Export all subtrees that are *not* tagged with :noexport: to
    separate pdf files.

    Subtrees that do not have the :EXPORT_FILE_NAME: property set
    are exported to a filename derived from the headline text."
  (interactive)
  (require 'ox-pandoc)
  (save-buffer)
  (let ((modifiedp (buffer-modified-p)))
    (save-excursion
      (goto-char (point-min))
      (goto-char (re-search-forward "^*"))
      (set-mark (line-beginning-position))
      (goto-char (point-max))
      (org-map-entries
       (lambda ()
         (let ((export-file (org-entry-get (point) "EXPORT_FILE_NAME")))
           (unless export-file
             (org-set-property
              "EXPORT_FILE_NAME"
              (replace-regexp-in-string " " "_" (nth 4 (org-heading-components)))))
           (deactivate-mark)
           (org-pandoc-export-to-latex-pdf nil t)
           (unless export-file (org-delete-property "EXPORT_FILE_NAME"))
           (set-buffer-modified-p modifiedp)))
       "-noexport" 'region-start-level))))

;;;; Export Top Level Trees to File
;; From a useful [[https://emacs.stackexchange.com/questions/27226/how-to-export-top-level-trees-in-an-org-file-to-corresponding-files][stack exchange]] post
(defun lem/org-map-entries (org-file in-tags func)
  (let ((tags (if (stringp in-tags)
                  (list in-tags)
                in-tags)))

    (with-temp-buffer
      (org-mode)
      (insert-file-contents org-file-main)

      ;; Execute func at each heading that matches tags.
      (while (< (point) (point-max))

        ;; If find a heading...
        (and (search-forward-regexp "^\* " nil "end")

             ;; ...that matches the given tags...
             (seq-reduce
              (lambda(a b) (and a b))
              (mapcar
               (lambda (tag)
                 (beginning-of-line)
                 (search-forward-regexp
                  (concat ":" tag ":") (line-end-position) "end"))
               tags)
              t)

             ;; ... then execute given function with cursor at beginning of
             ;; heading.
             (progn
               (beginning-of-line)
               (save-excursion
                 (funcall func))
               (end-of-line)))))))

;;;; Org demote/promote region
(defun endless/demote-everything (number beg end)
  "Add a NUMBER of * to all headlines between BEG and END.
    Interactively, NUMBER is the prefix argument and BEG and END are
    the region boundaries."
  (interactive "p\nr")
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (narrow-to-region beg end)
        (goto-char (point-min))
        (let ((string (make-string number ?*)))
          (while (search-forward-regexp "^\\*" nil t)
            (insert string)))))))

;;;; Org Hide Property Drawers
;; From [[https://www.reddit.com/r/emacs/comments/9htd0r/how_to_completely_hide_the_properties_drawer_in/e6fehiw][Reddit]]

(defun org-toggle-properties ()
  "Toggle visibility of properties in current header if it exists."
  (save-excursion
    (when (not (org-at-heading-p))
      (org-previous-visible-heading 1))
    (when (org-header-property-p)
      (let* ((a (re-search-forward "\n\\:" nil t)))
        (if (outline-invisible-p (point))
            (outline-show-entry)
          (org-cycle-hide-drawers 'all))))))

;;;; Org Return DWIM
;; Note that i've disabled this for now as it was causing issues
;; https://gist.github.com/alphapapa/61c1015f7d1f0d446bc7fd652b7ec4fe
(defun lem/org-return (&optional ignore)
  "Add new list item, heading or table row with RET.
    A double return on an empty element deletes it. Use a prefix arg
    to get regular RET. "
  ;; See https://gist.github.com/alphapapa/61c1015f7d1f0d446bc7fd652b7ec4fe and
  ;; http://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  (interactive "P")
  (if ignore
      (org-return)
    (cond ((eq 'link (car (org-element-context)))
           ;; Open links like usual
           (org-open-at-point-global))
          ((and (fboundp 'org-inlinetask-in-task-p) (org-inlinetask-in-task-p))
           ;; It doesn't make sense to add headings in inline tasks. Thanks Anders
           ;; Johansson!
           (org-return))
          ((org-at-item-checkbox-p)
           ;; Add checkboxes
           (org-insert-todo-heading nil))
          ((and (org-in-item-p) (not (bolp)))
           ;; Lists end with two blank lines, so we need to make sure we are also not
           ;; at the beginning of a line to avoid a loop where a new entry gets
           ;; created with only one blank line.
           (if (org-element-property :contents-begin (org-element-context))
               (org-insert-heading)
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))
             (org-return)))
          ((org-at-heading-p)
           (if (s-present? (org-element-property :title (org-element-context)))
               (progn
                 (org-end-of-meta-data)
                 (org-insert-heading))
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))))
          ((org-at-table-p)
           (if (--any? (string-empty-p it)
                       (nth (- (org-table-current-dline) 1) (org-table-to-lisp)))
               (org-return)
             ;; Empty row
             (beginning-of-line)
             (delete-region (line-beginning-position) (line-end-position))
             (org-return)))
          (t
           (org-return)))))

;; (general-define-key :keymaps 'org-mode-map "RET" #'lem/org-return)

;;;; Org Create Check Box From List Item
;; A useful macro for converting list items to checkboxes
(fset 'lem/org-checkbox-from-list
      [?a ?  ?\[ ?  ?\] escape ?\M-x return])

;;;; Org link Syntax
(defun org-update-link-syntax (&optional no-query)
  "Update syntax for links in current buffer.
Query before replacing a link, unless optional argument NO-QUERY
is non-nil."
  (interactive "P")
  (org-with-point-at 1
    (let ((case-fold-search t))
      (while (re-search-forward "\\[\\[[^]]*?%\\(?:2[05]\\|5[BD]\\)" nil t)
        (let ((object (save-match-data (org-element-context))))
          (when (and (eq 'link (org-element-type object))
                     (= (match-beginning 0)
                        (org-element-property :begin object)))
            (goto-char (org-element-property :end object))
            (let* ((uri-start (+ 2 (match-beginning 0)))
                   (uri-end (save-excursion
                              (goto-char uri-start)
                              (re-search-forward "\\][][]" nil t)
                              (match-beginning 0)))
                   (uri (buffer-substring-no-properties uri-start uri-end)))
              (when (or no-query
                        (y-or-n-p
                         (format "Possibly obsolete URI syntax: %S.  Fix? "
                                 uri)))
                (setf (buffer-substring uri-start uri-end)
                      (org-link-escape (org-link-decode uri)))))))))))


;;;; Org Table Wrap
;; see https://emacs.stackexchange.com/a/30871/11934
(defun org-table-wrap-to-width (width)
  "Wrap current column to WIDTH."
  (interactive (list (read-number "Enter column width: ")))
  (org-table-check-inside-data-field)
  (org-table-align)

  (let (cline (ccol (org-table-current-column)) new-row-count (more t))
    (org-table-goto-line 1)
    (org-table-goto-column ccol)

    (while more
      (setq cline (org-table-current-line))

      ;; Cut current field
      (org-table-copy-region (point) (point) 'cut)

      ;; Justify for width
      (setq org-table-clip
            (mapcar 'list (org-wrap (caar org-table-clip) width nil)))

      ;; Add new lines and fill
      (setq new-row-count (1- (length org-table-clip)))
      (if (> new-row-count 0)
          (org-table-insert-n-row-below new-row-count))
      (org-table-goto-line cline)
      (org-table-goto-column ccol)
      (org-table-paste-rectangle)
      (org-table-goto-line (+ cline new-row-count))

      ;; Move to next line
      (setq more (org-table-goto-line (+ cline new-row-count 1)))
      (org-table-goto-column ccol))

    (org-table-goto-line 1)
    (org-table-goto-column ccol)))

(defun org-table-insert-n-row-below (n)
  "Insert N new lines below the current."
  (let* ((line (buffer-substring (point-at-bol) (point-at-eol)))
         (new (org-table-clean-line line)))
    ;; Fix the first field if necessary
    (if (string-match "^[ \t]*| *[#$] *|" line)
        (setq new (replace-match (match-string 0 line) t t new)))
    (beginning-of-line 2)
    (setq new
          (apply 'concat (make-list n (concat new "\n"))))
    (let (org-table-may-need-update) (insert-before-markers new))  ;;; remove?
    (beginning-of-line 0)
    (re-search-forward "| ?" (point-at-eol) t)
    (and (or org-table-may-need-update org-table-overlay-coordinates) ;;; remove?
         (org-table-align))
    (org-table-fix-formulas "@" nil (1- (org-table-current-dline)) n)))

;;;; Org Export Last Subtree
;; bind f5 to keyboard macro of export-last-subtree
(fset 'export-last-subtree
      "\C-u\C-c\C-e")

(eval-after-load "org"
  '(progn
     (define-key org-mode-map (kbd "<f5>") 'export-last-subtree)))


;;;; Org Tag Selection

(defun lem/org-select-tags-completing-read ()
  "Select tags to add to headline."
  (interactive)
  (let* ((current (org-get-tags (point)))
         (selected (completing-read-multiple "Select org tag(s): " (org-get-buffer-tags))))
    (alet (-uniq (append (-difference current selected)
                         (-difference selected current)))
      (org-set-tags it))))

;;;; Org Copy Link
;; see https://emacs.stackexchange.com/a/63038/11934
(defun lem/org-link-copy-at-point ()
  (interactive)
  (save-excursion
    (let* ((ol-regex "\\[\\[.*?:.*?\\]\\(\\[.*?\\]\\)?\\]")
           (beg (re-search-backward "\\[\\["))
           (end (re-search-forward ol-regex))
           (link-string (buffer-substring-no-properties (match-beginning 0) (match-end 0))))
      (kill-new link-string)
      (message "Org link %s is copied." link-string))))

;;;; Remove Org Links
;; https://emacs.stackexchange.com/a/10714/11934
(defun lem/org-replace-link-by-link-description ()
  "Replace an org link by its description or, if empty, its address"
  (interactive)
  (if (org-in-regexp org-link-bracket-re 1)
      (save-excursion
        (let ((remove (list (match-beginning 0) (match-end 0)))
              (description
               (if (match-end 2)
                   (org-match-string-no-properties 2)
                 (org-match-string-no-properties 1))))
          (apply 'delete-region remove)
          (insert description)))))

;;;; Uncheck Org boxes
;;see https://www.reddit.com/r/emacs/comments/r107bg/comment/hlx54vf/?utm_source=share&utm_medium=web2x&context=3
(defun lem/copy-and-uncheck (start end)
  "copy a region of regularly repeating checkbox items forward from
one week to the next, unchecking them at the same time"
  (interactive "r")
  (kill-new (replace-regexp-in-string (rx "[X]") "[ ]" (buffer-substring start end)))
  (setq deactivate-mark t))

;;;; Org Archive
(defun lem/org-archive-done-tasks ()
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE" 'agenda))

;;;; Org Tree/Heading to New File
(defun lem/org-tree-to-new-file ()
  (interactive)
  "Move an org subtree to a new file"
  (org-copy-subtree nil t)
  (find-file-other-window
   (read-file-name "Move subtree to file:" ))
  (org-paste-subtree))

;;;; Org Wrap in Block Template
;; A helpful function I found for wrapping text in a block template.
;; http://pragmaticemacs.com/emacs/wrap-text-in-an-org-mode-block/

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-block-wrap ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(
                      ("a" . "ascii")
                      ("c" . "comment")
                      ("C" . "center")
                      ("e" . "example")
                      ("E" . "src emacs-lisp")
                      ("h" . "html")
                      ("l" . "laTeX")
                      ("n" . "notes")
                      ("q" . "quote")
                      ("s" . "src")
                      ("v" . "verse")
                      ))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+end_" choice "\n")
                (goto-char start)
                (insert "#+begin_" choice "\n")))
             (t
              (insert "#+begin_" choice "\n")
              (save-excursion (insert "#+end_" choice))))))))))


;;;; Org Export Body to HTML Buffer
(defun lem/org-export-to-buffer-html-as-body (&optional async subtreep visible-only body-only ext-plist)
  "Export org buffer body to html"
  (interactive)
  (org-export-to-buffer 'html "*Org HTML Export*"
    async body-only ext-plist (lambda () (html-mode)))
  (lem/copy-whole-buffer-to-clipboard)
  (delete-windows-on "*Org HTML Export*")
  (message "HTML copied!"))
;; (lem/previous-user-buffer))


;;; Org Contrib
(use-package org-contrib
  :straight t
  :after org)

;;; End Org Setup
(provide 'lem-setup-org)
