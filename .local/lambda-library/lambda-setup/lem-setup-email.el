;;; setup-email.el --- summary -*- lexical-binding: t -*-

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


;;;; Commentary:

;; Email settings; assembled from many sources. I use mbsync and mu4e. For
;; styling resources see: https://github.com/rougier/nano-emacs/blob/master/nano-mu4e.el

;;; Code:

;;;; Mu4e
(use-package mu4e
  ;; Tell straight to use homebrew mu4e
  :straight nil
  :load-path "/opt/homebrew/share/emacs/site-lisp/mu/mu4e"
  :commands (mu4e mu4e-compose-new mu4e-update-mail-and-index)
  :config
  ;; Finding the binary (installed w/homebrew)
  (setq mu4e-mu-binary (executable-find "mu"))

;;;;; Syncing
  ;; Maildir
  (setq mu4e-maildir "~/.maildir")
  ;; Sync imap servers w/mbsync (via isync installed w/homebrew):
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  ;; Change filenames when moving
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  ;; i.e. makes sure that moving a message (like to Trash) causes the
  ;; message to get a new file name.  This helps to avoid the
  ;; dreaded "UID is N beyond highest assigned" error.
  ;; See this link for more info: https://stackoverflow.com/a/43461973
  (setq mu4e-change-filenames-when-moving t)
  ;; Refresh mail using mbsync every 5 minutes
  (setq mu4e-update-interval (* 5 60))

;;;;; Attachments
  ;; Set default dir
  (setq mu4e-attachment-dir (concat (getenv "HOME") "/Downloads"))
  ;; Save all attachments to specified dir without asking about each one
  (setq mu4e-save-multiple-attachments-without-asking t)
  (bind-key "e" #'mu4e-views-mu4e-save-all-attachments mu4e-headers-mode-map)

;;;;; Viewing

;;;;;; Header View Functions
  ;; TODO: fix faces so they inherit and don't rely on lambda-themes
  (defun mu4e-get-account (msg)
    (let* ((maildir (mu4e-message-field msg :maildir))
           (maildir (substring maildir 1)))
      (nth 0 (split-string maildir "/"))))

  (defun mu4e-get-maildir (msg)
    (let* ((maildir (mu4e-message-field msg :maildir))
           (maildir (substring maildir 1)))
      (nth 0 (reverse (split-string maildir "/")))))

  (defun mu4e-get-mailbox (msg)
    (format "%s|%s" (mu4e-get-account msg) (mu4e-get-maildir msg)))

  (defun mu4e-headers-tag (text tag face help query)
    "Make a clickable button with specified FACE displaying TEXT.
   When hovered, HELP is displayed. When clicked, mu4e QUERY is executed."
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map 'mu4e-headers-mode-map)
      (define-key map [mouse-1] `(lambda ()
                                   (interactive) (mu4e-headers-search ,query)))
      (concat
       (propertize text
                   'display tag
                   'face face
                   'mouse-face '(:foreground homoglyph)
                   'local-map map
                   'help-echo `(lambda (window _object _point)
                                 (let (message-log-max) (message ,help))))
       " ")))

  ;; Buttons
  (defun mu4e-headers-button (text face help query)
    "Make a clickable button with specified FACE displaying TEXT.
   When hovered, HELP is displayed. When clicked, mu4e QUERY is executed."
    (let ((map (make-sparse-keymap)))
      (set-keymap-parent map 'mu4e-headers-mode-map)
      (define-key map [mouse-1] `(lambda ()
                                   (interactive) (mu4e-headers-search ,query)))
      (propertize text
                  'face face
                  'mouse-face `(:foreground lambda-bg
                                :background lambda-mild)
                  'local-map map
                  'help-echo `(lambda (window _object _point)
                                (let (message-log-max) (message ,help))))))

  (defun mu4e-headers-date-button (date face)
    (concat
     (mu4e-headers-button (format-time-string "%d" date)
                          face
                          (format-time-string "Mails from %d %B %Y" date)
                          (format-time-string "date:%Y%m%d" date))
     (propertize "/" 'face face)
     (mu4e-headers-button (format-time-string "%m" date)
                          face
                          (format-time-string "Mails from %B %Y" date)
                          (format-time-string "date:%Y%m" date))
     (propertize "/" 'face face)
     (mu4e-headers-button (format-time-string "%Y" date)
                          face
                          (format-time-string "Mails from %Y" date)
                          (format-time-string "date:%Y" date))))
  ;; Relative dates
  (defun mu4e-headers-is-today (date)
    (= (- (time-to-days (current-time)) (time-to-days date)) 0))

  (defun mu4e-headers-is-yesterday (date)
    (= (- (time-to-days (current-time)) (time-to-days date)) 1))

  (defun mu4e-headers-relative-date (msg)
    (let* ((thread  (mu4e-message-field msg :thread))
           (level (plist-get thread :level))
           (empty-parent (and thread (plist-get thread :empty-parent)))
           (child   (and thread (> (plist-get thread :level) 0)))
           (unread  (memq 'unread  (mu4e-message-field msg :flags)))
           (date (mu4e-msg-field msg :date))
           (diff (- (time-to-days (current-time)) (time-to-days date)))
           (face 'lambda-focus))
      (setq face 'lambda-meek)
      (cond ((mu4e-headers-is-today date)
             (mu4e-headers-button (format-time-string "     %H:%M" date)
                                  face
                                  (format-time-string "Mails from today")
                                  (format-time-string "date:%Y%m%d" date)))
            ((mu4e-headers-is-yesterday date)
             (mu4e-headers-button " Yesterday"
                                  face
                                  (format-time-string "Mails from yesterday")
                                  (format-time-string "date:%Y%m%d" date)))
            (t  (mu4e-headers-date-button date face)))))

  ;; Style & determine what flags to show
  (defun mu4e-headers-attach (msg)
    (cond ((memq 'flagged  (mu4e-message-field msg :flags))
           (propertize "!" 'face 'lambda-strong))
          ((memq 'attach  (mu4e-message-field msg :flags))
           (propertize "" 'face 'lambda-mild))
          (t " ")))

;;;;;; Headers
  ;; Add some custom headers
  (add-to-list 'mu4e-header-info-custom
               '(:empty . (:name "Empty"
                           :shortname ""
                           :function (lambda (msg) "  "))))

  (add-to-list 'mu4e-header-info-custom
               '(:relative-date . (:name "Relative date"
                                   :shortname ""
                                   :function mu4e-headers-relative-date)))

  (add-to-list 'mu4e-header-info-custom
               '(:mailbox-short . (:name "Mailbox"
                                   :shortname ""
                                   :function mu4e-get-mailbox)))

  (add-to-list 'mu4e-header-info-custom
               '(:attach . (:name "Attachment"
                            :shortname ""
                            :function mu4e-headers-attach)))
  ;; Set headers
  (setq mu4e-headers-date-format "%D";; "%Y-%m-%d %H:%M:%S"
        mu4e-headers-fields '(
                              (:flags          .  10)
                              (:relative-date  .  12)
                              (:from-or-to     .  40)
                              (:subject        .  85)
                              (:tags           .  20)
                              (:mailbox-short  .  15)
                              ))

  ;; Handle html-formatted emails
  ;; View in browser
  (add-to-list 'mu4e-view-actions '("view in browser" . mu4e-action-view-in-browser) t)

  ;; Other options for rendering
  ;; NOTE: superseded by xwidget support -- see mu4e-views below
  (setq mu4e-html2text-command "iconv -c -t utf-8 | pandoc -f html -t plain")
  ;; (setq mu4e-html2text-command 'mu4e-shr2text)
  ;; (setq mu4e-html2text-command "textutil -stdin -format html -convert txt -stdout")

  ;; other display settings
  (setq mu4e-speedbar-support t)
  (setq mu4e-use-fancy-chars t)
  (add-hook 'mu4e-view-mode-hook #'visual-line-mode)

;;;;;; Mail Tagging
  ;; Tag mail messages
  ;; See https://github.com/panjie/mu4e-goodies

  ;; Helper functions/vars
  ;;--------------------------------------------------
  (defsubst lem-mail~get-real-addr (addr)
    "Parse addr which is the result of mu4e-message-fields to get
the real email address"
    (if (listp addr)       ;; already parsed by mu4e
        (cdr (car addr))
      (if (stringp addr)   ;; raw address like: "ABC <abc@abc.com>"
          (car (mail-header-parse-address addr)))))

  (defvar lem-mail~header-handlers nil
    "Internal handlers of header view for mu >= 1.5")

  (defun lem-mail~header-advice (orig-func &rest args)
    "General advice for plugins for header view"
    (let* ((str (apply orig-func args))
           (msg (car args))
           (field (cadr args)))
      (dolist (func lem-mail~header-handlers)
        (setq str (funcall func msg field (mu4e-message-field msg field) str)))
      str))

  (when (functionp 'mu4e~headers-field-value) ; mu >= 1.5
    (advice-add 'mu4e~headers-field-value :around #'lem-mail~header-advice))

  ;; Tags for emails (from info pages of mu4e)
  ;;--------------------------------------------------
  ;; Add completing read
  (add-to-list 'mu4e-marks
               '(tag
                 :char       ("g" . " ")
                 :prompt     "gtag"
                 :ask-target (lambda () (lem-select-mail-tag))
                 :action      (lambda (docid msg target)
                                (mu4e-action-retag-message msg target))))
  (mu4e~headers-defun-mark-for tag)
  (define-key mu4e-headers-mode-map (kbd "G") 'mu4e-headers-mark-for-tag)
  (define-key-after (lookup-key mu4e-headers-mode-map [menu-bar headers])
    [mark-tag] '("Mark for tag" . mu4e-headers-mark-for-tag) 'mark-pattern)


  ;; Actions to add tags
  ;;--------------------------------------------------
  (add-to-list 'mu4e-view-actions
               '("add/remove tags" . mu4e-action-retag-message) t)

  ;; Tags & Completing read function
  ;;--------------------------------------------------
  (defvar lem-mail-tags
    '("casrac"
      "conferences"
      "edited-volume"
      "ergo"
      "grad-admissions"
      "phil105"
      "phil232"
      "phil871"
      "phil880"
      "phil971"
      "placement"
      "publications"
      "referee-reports"
      "supervision"
      )
    "List of email tags")

  (defun lem-select-mail-tag ()
    (interactive)
    (completing-read "Select Tag (+/-): " lem-mail-tags))

  ;; Quickly add/remove/search tag (named QT**) in header/message view
  ;;--------------------------------------------------
  (defvar lem-mail~quick-tag "QT**"
    "Quick tag.")

  (defun lem-mail-add-del-quick-tag ()
    "Quickly add/del tags."
    (interactive)
    (let* ((msg (mu4e-message-at-point))
           (oldtags (mu4e-message-field msg :tags)))
      (if (member lem-mail~quick-tag oldtags)
          (mu4e-action-retag-message msg (concat "-" lem-mail~quick-tag))
        (mu4e-action-retag-message msg (concat "+" lem-mail~quick-tag)))))

  ;;
  ;; Show tags in the header view
  ;;--------------------------------------------------
  ;;
  (defun lem-mail-header-add-tags-handler (msg field f-v str)
    "Add tags to header view's subject field like: [TAG][TAG] subject..."
    (let* ((val (or str f-v)))
      (if (eq field :subject)
          (let ((tags (mu4e-message-field msg :tags)))
            (if tags
                (setq val (concat
                           (mapconcat (function (lambda (x) (propertize (concat "[" x "]") 'face 'fringe)))
                                      tags "")
                           " "
                           val))
              val))
        val)))

  (cond ((functionp 'mu4e~headers-field-value) ; for mu>=1.5
         (add-to-list 'lem-mail~header-handlers 'lem-mail-header-add-tags-handler))
        ((listp 'mu4e~headers-field-handler-functions) ; for mu<1.5
         (add-to-list 'mu4e~headers-field-handler-functions (lambda (msg field val width)
                                                              "" (lem-mail-header-add-tags-handler msg field val nil))))
        (t nil))

;;;;;; Searching
  ;; Don't limit searches
  (setq mu4e-headers-results-limit -1)

;;;;; Composing Email

  ;; Use mu4e system-wide
  (setq mail-user-agent 'mu4e-user-agent)

  ;; List of your email adresses:
  (setq mu4e-user-mail-address-list '("mclear@fastmail.com"
                                      "mclear@unl.edu"))

  ;; Compose in new frame
  (setq mu4e-compose-in-new-frame t)

  ;; Don't keep message compose buffers around after sending:
  (setq message-kill-buffer-on-exit t)
  ;;; Make sure plain text mails flow correctly for recipients
  (setq mu4e-compose-format-flowed t)

  ;; Only ask if a context hasn't been previously picked
  (setq mu4e-compose-context-policy 'ask-if-none)

  ;; Possible fix for outlook client reading problems in inline messages
  ;; https://www.djcbsoftware.nl/code/mu/mu4e/Writing-messages.html#How-can-I-avoid-Outlook-display-issues_003f
  (setq  message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote...")

  ;; Check spelling
  (add-hook 'mu4e-compose-mode-hook 'flyspell-mode)


;;;;; Sending Mail
;;;;;; Send Settings

  ;; Configure the function to use for sending mail
  (setq message-send-mail-function 'smtpmail-send-it)
  (setq smtpmail-queue-dir (concat mu4e-maildir "/queued-mail/"))
  (setq smtpmail-debug-info t)
  ;; NOTE: Only use this if you have set up a GPG key!
  ;; Automatically sign all outgoing mails
  ;; (add-hook 'message-send-hook 'mml-secure-message-sign-pgpmime)

  ;; Move messages to trash
  ;; See https://rakhim.org/fastmail-setup-with-emacs-mu4e-and-mbsync-on-macos/
  ;; FIXME: This wont work since I have two email accounts
  ;; (fset 'lem--email-move-to-trash "mTrash")
  ;; (define-key mu4e-headers-mode-map (kbd "d") 'lem--email-move-to-trash)
  ;; (define-key mu4e-view-mode-map (kbd "d") 'lem--email-move-to-trash)

;;;;;; Check Attachments
  ;; See https://github.com/panjie/mu4e-goodies

  (require 'hi-lock)

  (defvar lem-mail-rule-func
    '((check-attach . lem-mail-draft-attach-p)
      (check-cc . lem-mail-draft-cc-p)))

  (defvar lem-mail-keywords
    '(("[aA]ttachment" . check-attach)
      ("[aA]ttached" . check-attach)
      ("[cC]c'd" . check-cc)
      ("C[cC]'d" . check-cc)
      ("CCd" . check-cc))
    "Keywords to be alerted. An alist like:
\( (regexp-of-keywords . rules-for-keywords) ... )")

  (defun lem-mail-draft-attach-p ()
    "Check if current email draft has at least one attachment."
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "\<#part .*filename=.*" (point-max) t)))

  (defun lem-mail-draft-cc-p ()
    "Check if current email draft has cc field."
    (message-fetch-field "Cc"))

  (defun lem-mail-search-body-subject (keyword &optional start)
    "Search for keyword in the current mail's subject and body. Return
the pos of the keyword which is a cons cell, nil if not found."
    ;; check for subject
    (save-excursion
      (if (and start (<= start (point-max)))
          (goto-char start)
        (message-goto-subject))
      (if (re-search-forward keyword (point-max) t)
          ;; check if the keyword is found in a cited line
          (let ((current-pos (point)))
            (beginning-of-line)
            (if (or (search-forward message-yank-prefix
                                    (+ (point) (length message-yank-prefix))
                                    t)
                    (search-forward message-yank-cited-prefix
                                    (+ (point) (length message-yank-cited-prefix))
                                    t))
                (lem-mail-search-body-subject keyword current-pos)
              (cons (match-beginning 0) (match-end 0))))
        nil)))

  (add-hook 'message-send-hook
            (defun lem-mail-check-keywords ()
              (interactive "P")
              (let ((it (car lem-mail-keywords))
                    (list (cdr lem-mail-keywords))
                    (key-pos)
                    (msg))
                (while (and (not key-pos) it)
                  (unless (and (setq key-pos (lem-mail-search-body-subject (car it)))
                               (not (funcall (cdr (assoc (cdr it) lem-mail-rule-func)))))
                    (setq key-pos nil)
                    (setq it (car list)
                          list (cdr list))))
                (when key-pos
                  (goto-char (car key-pos))
                  (overlay-put (make-overlay (car key-pos) (cdr key-pos)) 'face 'hi-yellow)
                  (cond
                   ((eq (cdr it) 'check-attach) (setq msg "You may forget your attachment!"))
                   ((eq (cdr it) 'check-cc) (setq msg "You may forget your Cc!")))
                  (setq msg (concat msg " Really send message?"))
                  (or (y-or-n-p msg)
                      (keyboard-quit))))))

  (defun lem-mail-check-keywords ()
    (interactive "P")
    (let ((it (car lem-mail-keywords))
          (list (cdr lem-mail-keywords))
          (key-pos)
          (msg))
      (while (and (not key-pos) it)
        (unless (and (setq key-pos (lem-mail-search-body-subject (car it)))
                     (not (funcall (cdr (assoc (cdr it) lem-mail-rule-func)))))
          (setq key-pos nil)
          (setq it (car list)
                list (cdr list))))
      (when key-pos
        (goto-char (car key-pos))
        (overlay-put (make-overlay (car key-pos) (cdr key-pos)) 'face 'hi-yellow)
        (cond
         ((eq (cdr it) 'check-attach) (setq msg "You may have forgotten your attachment!"))
         ((eq (cdr it) 'check-cc) (setq msg "You may have forgotten your Cc!")))
        (setq msg (concat msg " Really send message?"))
        (or (y-or-n-p msg)
            (keyboard-quit)))))


;;;;; Contexts

  (setq mu4e-contexts
        (list
         ;; Work account
         (make-mu4e-context
          :name "UNL"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/UNL" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "mclear@unl.edu")
                  (user-full-name    . "Colin McLear")
                  ;; use Davmail for exchange handshake
                  (smtpmail-smtp-server  . "localhost")
                  ;; this address needs to be the original not the alias
                  (smtpmail-user-mail-address . "cmclear2@unl.edu")
                  ;; use keychain for credentials
                  (smtpmail-smtp-service . 1025)
                  (smtpmail-stream-type  . nil)
                  (mu4e-compose-signature . (concat
                                             "Colin McLear\n"
                                             "Associate Professor\n"
                                             "Department of Philosophy\n"
                                             "University of Nebraska–Lincoln\n"
                                             "[[https://www.colinmclear.net]]"))
                  (mu4e-drafts-folder  . "/UNL/Drafts")
                  (mu4e-sent-folder  . "/UNL/Sent")
                  (mu4e-refile-folder  . "/UNL/Archive")
                  (mu4e-trash-folder  . "/UNL/Trash")))

         ;; Personal account
         (make-mu4e-context
          :name "Fastmail"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Fastmail" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "mclear@fastmail.com")
                  (user-full-name    . "Colin McLear")
                  (smtpmail-smtp-server  . "smtp.fastmail.com")
                  (smtpmail-smtp-service . 465)
                  (smtpmail-stream-type  . ssl)
                  ;; use keychain for credentials
                  (smtp-auth-credentials "security find-generic-password -s mbsync-fastmail-password -w")
                  (mu4e-compose-signature . (concat
                                             "Colin McLear\n"
                                             "[[https://www.colinmclear.net]]"))
                  (mu4e-drafts-folder  . "/Fastmail/Drafts")
                  (mu4e-sent-folder  . "/Fastmail/Sent Items")
                  (mu4e-refile-folder  . "/Fastmail/Archive")
                  (mu4e-trash-folder  . "/Fastmail/Trash")))))

  ;; Ask for context if none is set
  (setq mu4e-context-policy 'pick-first)

;;;;; Quick Actions

  ;; Helpful discussion at
  ;; https://github.com/daviwil/emacs-from-scratch/blob/master/show-notes/Emacs-Mail-05.org
  (defun lem-capture-mail-comment (msg)
    "Capture for message follow-up"
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mc"))

  (defun lem-capture-mail-respond (msg)
    "Capture for message follow-up"
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mr"))

  (defun lem-capture-mail-remind (msg)
    "Capture for message read-later"
    (interactive)
    (call-interactively 'org-store-link)
    (org-capture nil "mm"))

  ;; Add custom actions for our capture templates
  (add-to-list 'mu4e-headers-actions
               '("Comment on" . lem-capture-mail-comment) t)
  (add-to-list 'mu4e-view-actions
               '("Comment on" . lem-capture-mail-comment) t)
  (add-to-list 'mu4e-headers-actions
               '("respond" . lem-capture-mail-respond) t)
  (add-to-list 'mu4e-view-actions
               '("respond" . lem-capture-mail-respond) t)
  (add-to-list 'mu4e-headers-actions
               '("Remind" . lem-capture-mail-remind) t)
  (add-to-list 'mu4e-view-actions
               '("Remind" . lem-capture-mail-remind) t)

;;;;; Mail Custom Bookmarks/Searches

  (setq mu4e-bookmarks '((:name "Inbox"       :query "m:/UNL/inbox or m:/Fastmail/inbox"      :key ?i)
                         (:name "Unread"      :query "flag:unread AND NOT flag:trashed"       :key ?u)
                         (:name "Drafts"      :query "m:/UNL/drafts or m:/Fastmail/drafts"    :key ?d)
                         (:name "Sent Mail"   :query "m:/UNL/sent or m:/Fastmail/sent"        :key ?s)
                         (:name "Trash"       :query "m:/UNL/Trash or m:/Fastmail/Trash"      :key ?T)
                         (:name "-----"       :query "m:/UNL/inbox" :hide-unread t            :key ?-)
                         (:name "Today"       :query "date:today..now"                        :key ?t)
                         (:name "Yesterday"   :query "date:2d..today and not date:today..now" :key ?y)
                         (:name "Last Week"   :query "date:7d..now"                           :key ?w)
                         (:name "Last Month"  :query "date:4w..now"                           :key ?m)
                         (:name "-----"       :query "m:/UNL/inbox" :hide-unread t            :key ?-)
                         (:name "Archive"     :query "m:/UNL/archive or m:/Fastmail/archive"  :key ?a)
                         (:name "Important"   :query "flag:flagged"                           :key ?!)
                         (:name "Attachments" :query "flag:attach"                            :key ?A)
                         (:name "Images"      :query "mime:image/*"                           :key ?I)))


;;;;; Maildirs
  ;; NOTE: Use maildir-extensions for now
  ;; Eventually this will be incorporated into mu, but right now it doesn't show mail counts for some reason

  ;; ;; the maildirs you use frequently; access them with 'j' ('jump')
  (setq mu4e-maildir-shortcuts '((:maildir "/Fastmail/Archive"    :key ?a)
                                 (:maildir "/Fastmail/Inbox"      :key ?i)
                                 (:maildir "/Fastmail/Starred"    :key ?w)
                                 (:maildir "/Fastmail/Sent Items" :key ?s)
                                 (:maildir "/UNL/Inbox"           :key ?I)
                                 (:maildir "/UNL/Archive"         :key ?A)
                                 (:maildir "/UNL/Archive1"        :key ?1)
                                 (:maildir "/UNL/Sent"            :key ?S)))

  ;; Show maildirs with 0 messages
  (setq mu4e-main-hide-fully-read nil)


;;;;; Better Marking (w/Icons & SVG Tags)

;;;;;; All-the-icons for marking
  ;; Use all-the-icons
  ;;https://github.com/emacsmirror/mu4e-marker-icons
  ;;https://github.com/djcb/mu/issues/1795

  ;; Depends on all-the-icons
  (when (featurep 'all-the-icons)
    (require 'all-the-icons)

    (defgroup mu4e-marker-icons nil
      "Display icons for mu4e markers."
      :group 'mu4e-marker-icons)

    (defvar mu4e-marker-icons-marker-alist
      '((mu4e-headers-seen-mark      . mu4e-marker-icons-saved-headers-seen-mark)
        (mu4e-headers-new-mark       . mu4e-marker-icons-saved-headers-new-mark)
        (mu4e-headers-unread-mark    . mu4e-marker-icons-saved-headers-unread-mark)
        (mu4e-headers-signed-mark    . mu4e-marker-icons-saved-headers-signed-mark)
        (mu4e-headers-encrypted-mark . mu4e-marker-icons-saved-headers-encrypted-mark)
        (mu4e-headers-draft-mark     . mu4e-marker-icons-saved-headers-draft-mark)
        (mu4e-headers-attach-mark    . mu4e-marker-icons-saved-headers-attach-mark)
        (mu4e-headers-passed-mark    . mu4e-marker-icons-saved-headers-passed-mark)
        (mu4e-headers-flagged-mark   . mu4e-marker-icons-saved-headers-flagged-mark)
        (mu4e-headers-replied-mark   . mu4e-marker-icons-saved-headers-replied-mark)
        (mu4e-headers-trashed-mark   . mu4e-marker-icons-saved-headers-trashed-mark))
      "An alist of markers used in mu4e.")

    (defun mu4e-marker-icons--store (l)
      "Store mu4e header markers value from L."
      (mapcar (lambda (x) (set (cdr x) (symbol-value (car x)))) l))

    (defun mu4e-marker-icons--restore (l)
      "Restore mu4e header markers value from L."
      (let ((lrev (mapcar (lambda (x) (cons (cdr x) (car x))) l)))
        (mu4e-marker-icons--store lrev)))

    (defun mu4e-marker-icons-enable ()
      "Enable mu4e-marker-icons."
      (mu4e-marker-icons--store mu4e-marker-icons-marker-alist)
      (setq mu4e-use-fancy-chars t)
      (setq mu4e-headers-precise-alignment t)
      (setq mu4e-headers-seen-mark       `("S" . ,(propertize
                                                   (all-the-icons-material "mail_outline")
                                                   'face `(:family ,(all-the-icons-material-family)
                                                           :foreground ,(face-background 'default))))
            mu4e-headers-new-mark        `("N" . ,(propertize
                                                   (all-the-icons-material "markunread")
                                                   'face `(:family ,(all-the-icons-material-family)
                                                           :foreground ,(face-background 'default))))
            mu4e-headers-unread-mark     `("u" . ,(propertize
                                                   (all-the-icons-material "notifications_none")
                                                   'face 'mu4e-unread-face))
            mu4e-headers-draft-mark      `("D" . ,(propertize
                                                   (all-the-icons-material "drafts")
                                                   'face 'mu4e-draft-face))
            mu4e-headers-attach-mark     `("a" . ,(propertize
                                                   (all-the-icons-material "attachment")
                                                   'face 'mu4e-attach-number-face))
            mu4e-headers-passed-mark     `("P" . ,(propertize ; ❯ (I'm participating in thread)
                                                   (all-the-icons-material "center_focus_weak")
                                                   'face `(:family ,(all-the-icons-material-family)
                                                           :foreground lambda-focus)))
            mu4e-headers-flagged-mark    `("F" . ,(propertize
                                                   (all-the-icons-material "flag")
                                                   'face 'mu4e-flagged-face))
            mu4e-headers-replied-mark    `("R" . ,(propertize
                                                   (all-the-icons-material "reply_all")
                                                   'face 'mu4e-replied-face))
            mu4e-headers-trashed-mark    `("T" . ,(propertize
                                                   (all-the-icons-material "delete_forever")
                                                   'face 'mu4e-trashed-face))
            mu4e-headers-encrypted-mark `("x" . ,(propertize
                                                  (all-the-icons-material "enhanced_encryption")
                                                  'face `(:family ,(all-the-icons-material-family)
                                                          :foreground lambda-blue)))
            mu4e-headers-signed-mark     `("s" . ,(propertize
                                                   (all-the-icons-material "check")
                                                   'face `(:family ,(all-the-icons-material-family)
                                                           :foreground lambda-green)))))

    (defun mu4e-marker-icons-disable ()
      "Disable mu4e-marker-icons."
      (mu4e-marker-icons--restore mu4e-marker-icons-marker-alist))

    (define-minor-mode mu4e-marker-icons-mode
      "Display icons for mu4e markers."
      :require 'mu4e-marker-icons-mode
      :init-value nil
      :global t
      (if mu4e-marker-icons-mode
          (mu4e-marker-icons-enable)
        (mu4e-marker-icons-disable)))

    (with-eval-after-load 'mu4e
      (mu4e-marker-icons-mode)))

;;;;;; Add SVG tags
  ;; Don't show refile target; use svg instead
  (setq mu4e-headers-show-target nil)

  ;; FIXME: unmarking doesn't remove SVG tags
  (defun mu4e-mark-at-point-advice (mark target)
    (interactive)
    (require 'svg-tag-mode)
    (let* ((msg (mu4e-message-at-point))
           (docid (mu4e-message-field msg :docid))
           (overlay (make-overlay (- (line-end-position) 10)
                                  (- (line-end-position) 0))))
      (save-excursion
        ;; (remove-overlays (line-beginning-position) (line-end-position))
        (delete-overlay (make-overlay (line-beginning-position) (line-end-position)))
        (if (eql mark 'unmark)
            (delete-overlay overlay)
          (cond ((eql mark 'refile)
                 (overlay-put overlay 'display (svg-tag-make "ARCHIVE" 'success 3 0)))
                ((eql mark 'trash)
                 (overlay-put overlay 'display (svg-tag-make "TRASH" 'error 5 0)))
                ((eql mark 'untrash)
                 (overlay-put overlay 'display (svg-tag-make "UNTRASH" 3 0)))
                ((eql mark 'delete)
                 (overlay-put overlay 'display (svg-tag-make "DELETE" 'error 4 0)))
                ((eql mark 'unread)
                 (overlay-put overlay 'display (svg-tag-make "UNREAD" 4 0)))
                ((eql mark 'flag)
                 (overlay-put overlay 'display (svg-tag-make "FLAG" 'warning 6 0)))
                ((eql mark 'unflag)
                 (overlay-put overlay 'display (svg-tag-make "UNFLAG" 4 0)))
                ((eql mark 'move)
                 (overlay-put overlay 'display (svg-tag-make "MOVE" 'success 6 0)))
                ((eql mark 'tag)
                 (overlay-put overlay 'display (svg-tag-make "TAG" 'shadow 7 0))))))))

  (advice-add 'mu4e-mark-at-point :after #'mu4e-mark-at-point-advice)


;;;;; Mu4e & Swiftbar

  (defun lem-swiftbar-email-update ()
    "Update swiftbar mail plugin"
    (interactive)
    (call-process-shell-command "open -g 'swiftbar://refreshplugin?name=mail-mu'" nil 0))

  (add-hook 'mu4e-index-updated-hook #'lem-swiftbar-email-update)

;;;;; Miscellaneous

  ;; :TEST: Try a fix for encoding issues with sentmail especially
  (setq message-default-charset 'utf-8)
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . base64))

  ;; Updating
  ;; FIXME: right now this causes an updating loop for some reason
  ;; (add-hook 'mu4e-main-mode-hook #'mu4e-update-index)
  (bind-key "u" #'mu4e-update-index mu4e-main-mode-map)

  ;; Use completing-read
  (setq mu4e-completing-read-function 'completing-read)

  ;; Store link to message if in header view, not to header query
  (setq mu4e-org-link-query-in-headers-mode nil)

  ;; Quickly store links for search queries
  (defun lem-store-link-to-mu4e-query ()
    (interactive)
    (let ((org-mu4e-link-query-in-headers-mode t))
      (call-interactively 'org-store-link)))

  ;; Go to unread
  (defvar lem-mu4e-unread-query
    "flag:unread")
  (defun lem-go-to-mail-unread ()
    (interactive)
    (if (member "Email" (workspaces--list-workspaces))
        (progn
          (tab-bar-switch-to-tab "Email")
          (mu4e-headers-search lem-mu4e-unread-query))
      (progn
        (workspaces-create-workspace)
        (tab-bar-rename-tab "Email")
        (find-file (concat org-directory "mail.org"))
        (mu4e)
        (mu4e-headers-search lem-mu4e-unread-query))))

  ;; Go to inbox
  (defvar lem-mu4e-inbox-query
    "(maildir:/UNL/INBOX OR maildir:/Fastmail/INBOX)")
  (defun lem-go-to-mail-inbox ()
    (interactive)
    (if (member "Email" (workspaces--list-workspaces))
        (progn
          (tab-bar-switch-to-tab "Email")
          (mu4e-headers-search lem-mu4e-inbox-query))
      (progn
        (workspaces-create-workspace)
        (tab-bar-rename-tab "Email")
        (find-file (concat org-directory "mail.org"))
        (mu4e)
        (mu4e-headers-search lem-mu4e-inbox-query))))

  (defun lem-mu-kill-server ()
    "Forcefully kill the mu server process. This is especially
useful when mu4e-quit doesn't kill the server and the whole
things ends up hanging. See also this issue:
https://github.com/djcb/mu/issues/2198"
    (interactive)
    (let* ((buf (get-buffer mu4e~proc-name))
           (proc (and (buffer-live-p buf) (get-buffer-process buf))))
      (when proc
        (let ((delete-exited-processes t))
          (mu4e~call-mu '(quit)))
        ;; try sending SIGKILL to process, so it can exit gracefully
        (ignore-errors
          (signal-process proc 'SIGKILL))))
    (setq
     mu4e~proc-process nil
     mu4e~proc-buf nil)))

;;;;; End Mu4e


;;;; Better Viewing – Mu4e Views
;; This makes mu4e render html emails in emacs via xwidgets.
;; It basically reproduces a modern email client experience. Depends on compiling emacs with xwidgets
;; to check that exwidgets are installed
;; evaluate (xwidget-webkit-browse-url "https://www.gnu.org/")
;; NOTE: need to add something about not loading remote images (mu4e-view-show-images ?)

(use-package mu4e-views
  :straight (mu4e-views :type git :host github :repo "lordpretzel/mu4e-views")
  :after mu4e
  :defer nil
  :bind (:map mu4e-headers-mode-map
	     ("v" . mu4e-views-mu4e-select-view-msg-method) ;; select viewing method
         ("C-v" . lem-mu4e-text-view-toggle)
	     ("M-n" . mu4e-views-cursor-msg-view-window-down) ;; from headers window scroll the email view
	     ("M-p" . mu4e-views-cursor-msg-view-window-up) ;; from headers window scroll the email view
         ("f" . mu4e-views-toggle-auto-view-selected-message) ;; toggle opening messages automatically when moving in the headers view
         ("i" . mu4e-views-mu4e-view-as-nonblocked-html) ;; show currently selected email with all remote content
	     )
  :config
  (setq mu4e-views-completion-method 'default) ;; use default completion
  (setq mu4e-views-default-view-method "text") ;; make text the default
  (mu4e-views-mu4e-use-view-msg-method "text") ;; select the default
  ;; when pressing n and p stay in the current window
  (setq mu4e-views-next-previous-message-behaviour 'stick-to-current-window)
  ;; automatically open messages when moving in the headers view
  (setq mu4e-views-auto-view-selected-message t)

  (add-to-list 'mu4e-headers-actions
               '("e: export message" . mu4e-views-export-msg-action) t)
  (add-to-list 'mu4e-view-actions
               '("e: export message" . mu4e-views-export-msg-action) t))

(defun lem-mu4e-text-view-toggle ()
  "Toggle between text and html-block views in mu4e."
  (interactive)
  (let ((view (mu4e-views--get-current-viewing-method-name)))
    (if (string= "text" view)
        (mu4e-views-view-current-msg-with-method "html-nonblock")
      (mu4e-views-view-current-msg-with-method "text"))))

;;;; Using Org & HTML (Org-MSG)
(use-package org-msg
  :straight (:type git :host github :repo "jeremy-compostella/org-msg")
  :after mu4e
  :config
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} ':t toc:nil author:nil email:nil \\n:t"
	    org-msg-startup "hidestars inlineimages"
	    org-msg-greeting-fmt nil
	    org-msg-recipient-names nil
	    org-msg-greeting-name-limit 3
	    org-msg-default-alternatives '((new		        . (text html))
				                       (reply-to-html	. (html))
				                       (reply-to-text	. (text)))
	    org-msg-convert-citation t)

  (defun lem-org-msg-hooks ()
    "Hooks for org-msg"
    (progn
      (auto-fill-mode -1)
      (hl-line-mode 1)
      ;; FIXME: Try remove auto-save hook *locally* to avoid multiple saved drafts
      (remove-hook 'auto-save-hook #'lem-full-auto-save t)))
  (add-hook 'org-msg-edit-mode-hook #'lem-org-msg-hooks)

  (org-msg-mode))

;;;; Email Addressing
;; function to return first name of email recipients
;; used by yasnippet
;; inspired by
;;http://blog.binchen.org/posts/how-to-use-yasnippets-to-produce-email-templates-in-emacs.html
;; http://pragmaticemacs.com/emacs/email-templates-in-mu4e-with-yasnippet/

(defun lem-mu4e-get-names-for-yasnippet ()
  "Return comma separated string of names for an email"
  (interactive)
  (let ((email-name "") str email-string email-list email-name2 tmpname)
    (save-excursion
      (goto-char (point-min))
      ;; first line in email could be some hidden line containing NO to field
      (setq str (buffer-substring-no-properties (point-min) (point-max))))
    ;; take name from TO field - match series of names
    (when (string-match "^To: \"?\\(.+\\)" str)
      (setq email-string (match-string 1 str)))
    ;;split to list by comma
    (setq email-list (split-string email-string " *, *"))
    ;;loop over emails
    (dolist (tmpstr email-list)
      ;;get first word of email string
      (setq tmpname (car (split-string tmpstr " ")))
      ;;remove whitespace or ""
      (setq tmpname (replace-regexp-in-string "[ \"]" "" tmpname))
      ;;join to string
      (setq email-name
            (concat email-name ", " tmpname)))
    ;;remove initial comma
    (setq email-name (replace-regexp-in-string "^, " "" email-name))

    ;;see if we want to use the name in the FROM field
    ;;get name in FROM field if available, but only if there is only
    ;;one name in TO field
    (if (< (length email-list) 2)
        (when (string-match "^\\([^ ,\n]+\\).+writes:$" str)
          (progn (setq email-name2 (match-string 1 str))
                 ;;prefer name in FROM field if TO field has "@"
                 (when (string-match "@" email-name)
                   (setq email-name email-name2))
                 )))
    email-name))

;;; Provide
(provide 'lem-setup-email)
;;; setup-email.el ends here
