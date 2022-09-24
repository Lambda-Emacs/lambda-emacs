;;; Server
;; start server for emacsclient
(use-package server
  :if window-system
  ;; :hook (after-init . server-mode)
  :defer 2
  :config
  ;; t/nil for instructions
  (setq server-client-instructions nil)
  ;; avoid warning screen
  (or (server-running-p)
      (server-start)))

;; functions for killing server-related emacsen
(defun lem-kill-all-emacsen ()
  (interactive)
  (progn
    (save-buffers-kill-emacs)
    (shell-command-to-string "pkill -i emacs")))

(defun lem-kill-emacs-capture-daemon ()
  (interactive)
  (shell-command-to-string "pkill -f /Applications/Emacs.app/Contents/MacOS/emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lem-setup-server)
