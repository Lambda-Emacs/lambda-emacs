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


;; have these functions available for server
(defun cpm/activate-capture-frame ()
  "run org-capture in capture frame"
  (require 'org)
  (select-frame-by-name "capture")
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (org-capture))

(defun cpm/weather-journal-capture ()
  (interactive)
  (require 'org)
  (select-frame-by-name "capture")
  (switch-to-buffer (get-buffer-create "*scratch*"))
  (cpm/org-journal)
  (cpm/insert-weather)
  (goto-char (point-max)))

;; functions for killing server-related emacsen
(defun cpm/kill-all-emacsen ()
  (interactive)
  (progn
    (save-buffers-kill-emacs)
    (shell-command-to-string "pkill -i emacs")))

(defun cpm/kill-emacs-capture-daemon ()
  (interactive)
  (shell-command-to-string "pkill -f /Applications/Emacs.app/Contents/MacOS/emacs"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lem-setup-server)
