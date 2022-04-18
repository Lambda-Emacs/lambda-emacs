;; PDF Management

;;;; Doc-View Mode
(use-package doc-view
  :disabled
  :config
  (fset 'doc-prev "\C-xo\C-x[\C-xo")
  (fset 'doc-next "\C-xo\C-x]\C-xo")
  (global-set-key (kbd "M-[") 'doc-prev)
  (global-set-key (kbd "M-]") 'doc-next)
  ;; (evil-set-initial-state 'doc-view-mode 'normal)
  ;; (evil-define-key 'normal doc-view-mode-map
  ;; "/"  'spacemacs/doc-view-search-new-query
  ;; "?"  'spacemacs/doc-view-search-new-query-backward
  ;; "gg" 'doc-view-first-page
  ;; "f"  'doc-view-autofit-mode
  ;; "G"  'doc-view-last-page
  ;; "gt" 'doc-view-goto-page
  ;; "h"  'doc-view-previous-page
  ;; "j"  'doc-view-next-line-or-next-page
  ;; "k"  'doc-view-previous-line-or-previous-page
  ;; "K"  'doc-view-kill-proc-and-buffer
  ;; "l"  'doc-view-next-page
  ;; "n"  'doc-view-search
  ;; "N"  'doc-view-search-backward
  ;; "-"  'doc-view-shrink
  ;; "+"  'doc-view-enlarge
  ;; (kbd "C-d") 'doc-view-scroll-up-or-next-page
  ;; (kbd "C-k") 'doc-view-kill-proc
  ;; (kbd "C-u") 'doc-view-scroll-down-or-previous-page)
  (progn
    (defun spacemacs/doc-view-search-new-query ()
      "Initiate a new query."
      (interactive)
      (doc-view-search 'newquery))

    (defun spacemacs/doc-view-search-new-query-backward ()
      "Initiate a new query."
      (interactive)
      (doc-view-search 'newquery t))

    (defcustom doc-view-autofit-timer-start 1.0
      "Initial value (seconds) for the timer that delays the fitting when
  `doc-view-autofit-fit' is called (Which is when a window
  configuration change occurs and a document needs to be fitted)."
      :type 'number
      :group 'doc-view)

    (defcustom doc-view-autofit-timer-inc 0.02
      "Value to increase (seconds) the timer (see `doc-view-autofit-timer-start')
  by, if there is another window configuration change occuring, before
  it runs out."
      :type 'number
      :group 'doc-view)

    (defcustom doc-view-autofit-default-fit 'width
      "The fitting type initially used when mode is enabled.
  Valid values are: width, height, page."
      :type 'symbol
      :group 'doc-view)

    (defvar doc-view-autofit-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c W") 'doc-view-autofit-width)
        (define-key map (kbd "C-c H") 'doc-view-autofit-height)
        (define-key map (kbd "C-c P") 'doc-view-autofit-page)
        map)
      "Keymap used by `doc-view-autofit-mode'.")

    (defun doc-view-autofit-set (type)
      "Set autofitting to TYPE for current buffer."
      (when doc-view-autofit-mode
        (setq doc-view-autofit-type type)
        (doc-view-autofit-fit)))

    (defun doc-view-autofit-width ()
      "Set autofitting to width for current buffer."
      (interactive) (doc-view-autofit-set 'width))

    (defun doc-view-autofit-height ()
      "Set autofitting to height for current buffer."
      (interactive) (doc-view-autofit-set 'height))

    (defun doc-view-autofit-page ()
      "Set autofitting to page for current buffer."
      (interactive) (doc-view-autofit-set 'page))

    (defun doc-view-autofit-fit ()
      "Fits the document in the selected window's buffer
  delayed with a timer, so multiple calls in succession
  don't cause as much overhead."
      (lexical-let
       ((window (selected-window)))
       (if (equal doc-view-autofit-timer nil)
           (setq doc-view-autofit-timer
                 (run-with-timer
                  doc-view-autofit-timer-start nil
                  (lambda ()
                    (if (window-live-p window)
                        (save-selected-window
                          (select-window window)
                          (cancel-timer doc-view-autofit-timer)
                          (setq doc-view-autofit-timer nil)
                          (cond
                           ((equal 'width doc-view-autofit-type)
                            (doc-view-fit-width-to-window))
                           ((equal 'height doc-view-autofit-type)
                            (doc-view-fit-height-to-window))
                           ((equal 'page doc-view-autofit-type)
                            (doc-view-fit-page-to-window))))))))
         (timer-inc-time doc-view-autofit-timer doc-view-autofit-timer-inc))))

    (define-minor-mode doc-view-autofit-mode
      "Minor mode for automatic (timer based) fitting in DocView."
      :lighter " AFit" :keymap doc-view-autofit-mode-map :group 'doc-view
      (when doc-view-autofit-mode
        (set (make-local-variable 'doc-view-autofit-type)
             doc-view-autofit-default-fit)
        (set (make-local-variable 'doc-view-autofit-timer) nil)
        (add-hook 'window-configuration-change-hook
                  'doc-view-autofit-fit nil t)
        (doc-view-autofit-fit))
      (when (not doc-view-autofit-mode)
        (remove-hook 'window-configuration-change-hook
                     'doc-view-autofit-fit t)
        (when doc-view-autofit-timer
          (cancel-timer doc-view-autofit-timer)
          (setq doc-view-autofit-timer nil))
        (setq doc-view-autofit-type nil)))

    (add-hook 'doc-view-mode-hook 'doc-view-autofit-mode)
    ;; reload when file changes
    (add-hook 'doc-view-mode-hook 'auto-revert-mode)
    ;; continuous scroll mode
    (setq doc-view-continuous t)))

;;;;; PDF-Tools
;; good but often problematic pdf reader and annotator
(use-package pdf-tools
  ;; use maintained fork
  :straight (:host github :repo "vedang/pdf-tools")
  :mode (("\\.pdf$" . pdf-view-mode))
  :commands (pdf-view-mode)
  :init
  ;; initialise
  (pdf-loader-install :no-query)
  :bind (:map pdf-view-mode-map
         ;; Navigation
         ("j"  . pdf-view-next-line-or-next-page)
         ("k"  . pdf-view-previous-line-or-previous-page)
         ("l"  . pdf-view-next-page-command)
         ("h"  . pdf-view-previous-page-command)
         ("g"  . pdf-view-first-page)
         ("G"  . pdf-view-last-page)
         ("t"  . pdf-view-goto-page)
         ("l"  . pdf-view-goto-label)
         ;; Search
         ("/"  . isearch-forward)
         ("?"  . isearch-backward)
         ;; Actions
         ("-"  . pdf-view-shrink)
         ("+"  . pdf-view-enlarge)
         ("="  . pdf-view-fit-page-to-window)
         ("r"  . pdf-view-revert-buffer)
         ("o"  . pdf-links-action-perform)
         ("O"  . pdf-outline)
         ("!"  . lem-pdf-no-filter)
         ("#"  . lem-pdf-midnight-dark)
         ("@"  . lem-pdf-midnight-amber)
         ("$"  . lem-pdf-midnight-green))
  :config
  ;; HiDPI
  (setq pdf-view-use-scaling t)

  (defun lem-pdf-no-filter ()
    "View pdf without colour filter."
    (interactive)
    (pdf-view-midnight-minor-mode -1))

  (defun lem-pdf-midnight-mode ()
    "View pdf with colour filter."
    (interactive)
    (pdf-view-midnight-minor-mode))

  (defun lem-pdf-color-theme ()
    (if (eq active-theme 'light-theme)
        (lem-pdf-no-filter)
      (lem-pdf-midnight-mode)))

  ;; Set midnight mode colour functions
  (defun lem-pdf-midnight-dark ()
    "Set pdf-view-midnight-colors to dark & low contrast colours."
    (interactive)
    (setq pdf-view-midnight-colors '("#ECEFF4" . "#434C5E" ))
    (pdf-view-midnight-minor-mode))

  (defun lem-pdf-midnight-amber ()
    "Set pdf-view-midnight-colors to amber on dark slate blue."
    (interactive)
    (setq pdf-view-midnight-colors '("#ff9900" . "dark slate blue")) ; amber
    (pdf-view-midnight-minor-mode))

  (defun lem-pdf-midnight-green ()
    "Set pdf-view-midnight-colors to green on black."
    (interactive)
    (setq pdf-view-midnight-colors '("#00B800" . "#000000" )) ; green
    (pdf-view-midnight-minor-mode))

  ;; tex hook
  ;; see https://github.com/politza/pdf-tools#auto-revert
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  ;; other hooks
  (add-hook 'pdf-view-mode-hook (lambda ()
                                  (lem-pdf-color-theme)
                                  (blink-cursor-mode -1)
                                  (linum-mode -1)
                                  (line-number-mode -1)
                                  (column-number-mode -1)
                                  (auto-revert-mode -1))))

;;;; PDF Notetaking Tools
(defvar user-pdf-dir
  "~/Library/Mobile Documents/iCloud~com~sonnysoftware~bot/Documents/be-library")

(use-package org-noter
  :after org
  :commands (org-noter)
  :config
  (setq org-noter-auto-save-last-location t
        org-noter-insert-selected-text-inside-note t
        org-noter-insert-note-no-questions t
        org-noter-kill-frame-at-session-end t
        org-noter-default-notes-file-names '("noter-notes.org")
        org-noter-notes-search-path '("~/Dropbox/Work/projects/notebook/content-org")))

;; Couldn't get either of these working
(use-package org-pdfview
  :disabled
  :config
  (org-link-set-parameters "pdfview"
                           :follow #'org-pdfview-open
                           :export #'org-pdfview-export
                           :complete #'org-pdfview-complete-link
                           :store #'org-pdfview-store-link)
  (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (file link) (org-pdfview-open link))))
  (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (file link) (org-pdfview-open link)))))

;; I can't seem to get this to work...
(use-package org-pdftools
  :disabled
  :after org
  :demand t
  :init (setq org-pdftools-search-string-separator "??")
  :config
  (setq org-pdftools-root-dir user-pdf-dir)
  (org-link-set-parameters "pdftools"
                           :follow #'org-pdftools-open
                           :complete #'org-pdftools-complete-link
                           :store #'org-pdftools-store-link
                           :export #'org-pdftools-export)
  (add-hook 'org-store-link-functions 'org-pdftools-store-link))

(use-package org-noter-pdftools
  :disabled
  :after (org-noter))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lem-setup-pdf)
