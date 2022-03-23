;; -*- lexical-binding: t -*-
;;; Search
;;;; Ag
(use-package ag
  :defer 2
  :config
  (progn
    (defun ag/jump-to-result-if-only-one-match ()
      "Jump to the first ag result if that ag search came up with just one match."
      (let (only-one-match)
        (when (member "--stats" ag-arguments)
          (save-excursion
            (goto-char (point-min))
            (setq only-one-match (re-search-forward "^1 matches\\s-*$" nil :noerror)))
          (when only-one-match
            (next-error)
            (kill-buffer (current-buffer))
            (message (concat "ag: Jumping to the only found match and "
                             "killing the *ag* buffer."))))))
    (add-hook 'ag-search-finished-hook #'ag/jump-to-result-if-only-one-match)

    ;; Set default ag arguments
    ;; It looks like the ~/.agignore is used when launching ag from emacs too.
    ;; So the ignores from ~/.agignore don't have to be set here again.
    (setq ag-highlight-search t)
    ;; By default, ag.el will open results in a different window in the frame, so
    ;; the results buffer is still visible. You can override this so the results
    ;; buffer is hidden and the selected result is shown in its place:
    (setq ag-reuse-window nil)
    ;; reuse the same *ag* buffer for all your searches
    (setq ag-reuse-buffers t)
    ;; ;; To save buffer automatically when `wgrep-finish-edit'
    (setq wgrep-auto-save-buffer t)))

;;;; Deadgrep
(use-package deadgrep
  :bind (:map lem+search-keys
         ("g" . deadgrep)))

;;;; Ripgrep
(use-package rg :commands rg)

;;;; Search given directory
(defun lem/search-in-input-dir ()
  "Grep for a string in the input directory using completing read function"
  (interactive)
  (let ((current-prefix-arg '(4))) (call-interactively #'consult-ripgrep)))

;;; End Setup Search
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'lem-setup-search)
