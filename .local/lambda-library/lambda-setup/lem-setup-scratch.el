;; Better scratch settings
;; See https://www.reddit.com/r/emacs/comments/4cmfwp/scratch_buffer_hacks_to_increase_its_utility/

(defun bury-scratch ()
  (if (eq (current-buffer) (get-buffer "*scratch*"))
      (progn (bury-buffer)
             nil)
    t))

(add-hook 'kill-buffer-query-functions 'bury-scratch)

(defun save-persistent-scratch ()
  "Save the contents of *scratch*"
  (with-current-buffer (get-buffer-create "*scratch*")
    (write-region (point-min) (point-max)
                  (concat lem-cache-dir "scratch"))))

(defun load-persistent-scratch ()
  "Reload the scratch buffer"
  (let ((scratch-file (concat lem-cache-dir "scratch")))
    (if (file-exists-p scratch-file)
        (with-current-buffer (get-buffer "*scratch*")
          (delete-region (point-min) (point-max))
          (insert-file-contents scratch-file))))
  ;; don't auto-save scratch in home dir; put them all in the system tmp
  ;; directory
  (with-current-buffer (get-buffer "*scratch*")
    (setq-local default-directory "/tmp/")))


;; Hooks for loading and saving the scratch buffer
(add-hook 'after-init-hook 'load-persistent-scratch)
(add-hook 'kill-emacs-hook 'save-persistent-scratch)
;; Save scratch buffer every 5 minutes (300 seconds)
(run-with-idle-timer 300 t 'save-persistent-scratch)

;;; End Setup-Scratch
(provide 'lem-setup-scratch)
