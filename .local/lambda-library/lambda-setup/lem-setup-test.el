;; List packages for testing with clean emacs

;;;; Tab Bar
;; Use tab-bar for window grouping and configuration within a project (replaces eyebrowse)
(use-package tab-bar
  :straight (:type built-in)
  :custom
  (tab-bar-show t)
  ;; Unless another file/buffer is designated, start from scratch buffer
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-close-tab-select 'recent)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-separator " ")
  (tab-bar-new-button nil)
  (tab-bar-close-button nil)
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs
                    tab-bar-separator
                    tab-bar-format-add-tab))
  :config
  ;; https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/
  (defun lem-tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Otherwise use completion to select the tab."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t)))))))

;;;; Tab Workspaces

(use-package tabspaces
  ;; :straight (:type git :host github :repo "mclear-tools/emacs-workspaces")
  :straight (:local-repo "/Users/roambot/bin/lisp-projects/tabspaces")
  :hook (after-init . tabspaces-mode)
  ;; Add some functions to the project map
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Home")


  )


;;; Provide
(provide 'lem-setup-test)
