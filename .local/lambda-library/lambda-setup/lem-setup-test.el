;; List packages for testing with clean emacs

(use-package emacs-workspaces
  :straight (:local-repo "/Users/roambot/.emacs.d/.local/elisp/emacs-workspaces")
  :commands (emacs-workspaces/create-workspace
             emacs-workspaces/create-new-project-and-workspace
             emacs-workspaces/open-existing-project-and-workspace))

;;; Provide
(provide 'lem-setup-test)
