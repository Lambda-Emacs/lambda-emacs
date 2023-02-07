;; List packages for testing with clean emacs

(vertico-mode)
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(setq lem-user-elisp-dir "~/bin/lisp-projects/")
(setq lem-notes-dir (concat (getenv "HOME") "/Documents/notes/"))


(use-package consult-notes
  :load-path (lambda () (concat lem-user-elisp-dir "consult-notes"))
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :config

  (when (locate-library "org-roam")
    (consult-notes-org-roam-mode))
  (require 'lem-setup-org-roam)

  ;; (when (locate-library "denote")
  ;;   (consult-notes-denote-mode))

  (setq consult-notes-file-dir-sources
        `(("Org"             ?o "~/Dropbox/org-files/"))))


;; (use-package denote
;;   :commands (denote denote-create-note denote-link-ol-store)
;;   :custom
;;   (denote-directory lem-notes-dir)
;;   (denote-known-keywords '("emacs" "teaching" "unl" "workbook"))
;;   (denote-prompts '(title keywords subdirectory))
;;   ;; Use org-read-date
;;   (denote-date-prompt-use-org-read-date t)
;;   (denote-file-type 'org) ;; use org
;;   (denote-allow-multi-word-keywords nil) ;; single word keywords only
;;   ;; Better backlink display
;;   (denote-link-backlinks-display-buffer-action
;;    (quote ((display-buffer-reuse-window
;;             display-buffer-in-side-window)
;;            (inhibit-same-window . t)
;;            (side . bottom)
;;            (slot . 99)
;;            (window-height . 10)))))





;;; Provide
(provide 'lem-setup-test)
