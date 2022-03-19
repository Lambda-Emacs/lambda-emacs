;;; lem-setup-buffers.el --- summary -*- lexical-binding: t -*-

;; Author: Colin McLear
;; Maintainer: Colin McLear

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


;;; Commentary:

;; Buffer settings & useful functions

;;; Code:

;;; Switch to Buffer Preserve Window

;; switch-to-buffer tries to preserve window-point
(setq switch-to-buffer-preserve-window-point t)

;;; Unique buffers
(use-package uniquify
  :straight (:type built-in)
  :defer 3
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;;; Buffer Modes
;; from http://www.jurta.org/en/emacs/dotemacs, set the major mode
;; of buffers that are not visiting a file
(setq-default major-mode (lambda ()
                           (if buffer-file-name
                               (fundamental-mode)
                             (let ((buffer-file-name (buffer-name)))
                               (set-auto-mode)))))

;;; Autorevert
(use-package autorevert
  :straight (:type built-in)
  :hook (after-init . global-auto-revert-mode)
  :init
  (setq auto-revert-interval .5)
  :config
  (setq auto-revert-verbose nil ; Shut up, please!
        revert-without-query '(".*") ;; disable revert query
        ;; Revert Dired buffers, too
        global-auto-revert-non-file-buffers t)
  (global-auto-revert-mode))
;;; Revert All Buffers

(use-package revert-buffer-all
  :straight (:type git :host gitlab :repo "ideasman42/emacs-revert-buffer-all")
  :commands (revert-buffer-all))

;;; iBuffer
(use-package ibuffer
  :straight (:type built-in)
  :commands (ibuffer)
  :custom
  (ibuffer-default-sorting-mode 'major-mode)
  (ibuffer-filter-group-name-face 'outline-1)
  (ibuffer-movement-cycle t)
  (ibuffer-old-time 12)
  (ibuffer-modified-char ?*)
  (ibuffer-read-only-char ?R)
  (ibuffer-marked-char ?➤)
  (ibuffer-locked-char ?L)
  (ibuffer-deletion-char ?🗙)
  (ibuffer-use-header-line nil)
  :config
  ;; Fix function for displaying groups
  (defun ibuffer-insert-filter-group (name display-name filter-string format bmarklist)
    (add-text-properties
     (point)
     (progn
       (insert display-name)
       (point))
     `(ibuffer-filter-group-name
       ,name
       font-lock-face ,ibuffer-filter-group-name-face
       keymap ,ibuffer-mode-filter-group-map
       mouse-face highlight
       help-echo ,(let ((echo '(if tooltip-mode
				                   "mouse-1: toggle marks in this group\nmouse-2: hide/show this filtering group"
			                     "mouse-1: toggle marks  mouse-2: hide/show")))
		            (if (> (length filter-string) 0)
		                `(concat ,filter-string
			                     (if tooltip-mode "\n" " ")
			                     ,echo)
		              echo))))
    (insert "\n")
    (when bmarklist
      (put-text-property
       (point)
       (progn
         (dolist (entry bmarklist)
	       (ibuffer-insert-buffer-line (car entry) (cdr entry) format))
         (point))
       'ibuffer-filter-group
       name)))
  )

(use-package ibuffer-vc
  :straight (:host github :repo "purcell/ibuffer-vc")
  :defer 2
  :config
  ;; To include vc status info in the ibuffer list, add either
  ;; vc-status-mini or vc-status to `ibuffer-formats':
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                vc-relative-file)))

  ;; Don't need to display "Git" for every project; use a symbol instead
  (defun ibuffer-vc-generate-filter-groups-by-vc-root ()
    "Create a set of ibuffer filter groups based on the vc root dirs of buffers."
    (let ((roots (ibuffer-remove-duplicates
                  (delq nil (mapcar 'ibuffer-vc-root (buffer-list))))))
      (mapcar (lambda (vc-root)
                (cons (format " %s" (cdr vc-root))
                      `((vc-root . ,vc-root))))
              roots)))

  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

;;;; Blank Buffer New Frame
;; Make a blank buffer when opening a new frame. From
;; https://stackoverflow.com/a/25792276.

(defun lem/new-buffer-new-frame ()
  "Create a new frame with a new empty buffer."
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (set-buffer-major-mode buffer)
    (display-buffer buffer '(display-buffer-pop-up-frame . nil))))


;;;; Create new buffer
(defun lem/create-new-buffer ()
  (interactive)
  (let ((buffer (generate-new-buffer "*new*")))
    (set-window-buffer nil buffer)
    (with-current-buffer buffer
      (funcall (default-value 'major-mode)))))

;;;; Make Temp Buffer
(defun lem/tmp-buffer()
  "Make a temporary buffer and switch to it"
  (interactive)
  (switch-to-buffer (get-buffer-create (concat "tmp-" (format-time-string "%m.%dT%H.%M.%S"))))
  (delete-other-windows))

;;;; Revert all buffers
;;
(defun lem/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

;;;; Clipboard to/from Buffer
;; http://stackoverflow.com/a/10216338/4869
(defun lem/copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max)))

(defun lem/copy-clipboard-to-whole-buffer ()
  "Copy clipboard and replace buffer"
  (interactive)
  (delete-region (point-min) (point-max))
  (clipboard-yank)
  (deactivate-mark))

;;;; Useful Buffers
;; TODO: make this respect workspace buffers
(defun lem/user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
  Typically, if buffer name starts with *, it's not considered a user buffer.
  This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
  You can override this function to get your idea of “user buffer”.
  version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))

(defun lem/next-user-buffer ()
  "Switch to the next user buffer.
  “user buffer” is determined by `lem/user-buffer-q'.
  URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
  Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (lem/user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun lem/previous-user-buffer ()
  "Switch to the previous user buffer.
  “user buffer” is determined by `lem/user-buffer-q'.
  URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
  Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (lem/user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

;;;; Eval emacs buffer until error

(defun lem/eval-buffer-until-error ()
  "Evaluate emacs buffer until error occured."
  (interactive)
  (goto-char (point-min))
  (while t (eval (read (current-buffer)))))

;;;; Kill Current Buffer
;; (kill-this-buffer) is unreliable when not invoked from the menubar. So here's a
;; wrapper on (kill-buffer) to kill the current buffer. This is sometimes better
;; than (evil-delete-buffer) since it keeps the window.

(defun lem/kill-this-buffer ()
  (interactive)
  (kill-buffer))

;;;; Show Filename of Buffer

;; http://camdez.com/blog/2013/11/14/emacs-show-buffer-file-name/
(defun lem/show-and-copy-buffer-full-filename ()
  "Show the full path to the current file in the minibuffer and copy to clipboard."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

(defun lem/show-and-copy-buffer-filename ()
  "Show the abbreviated path to the current file in the minibuffer and copy to clipboard."
  (interactive)
  (let ((file-name (abbreviate-file-name buffer-file-name)))
    (if file-name
        (progn
          (message file-name)
          (kill-new file-name))
      (error "Buffer not visiting a file"))))

;;;; Switch previous buffer

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(provide 'lem-setup-buffers)
;;; lem-setup-buffers.el ends here
