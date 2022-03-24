;;;; lem-setup-skeleton.el --- Setup file for skeletons   -*- lexical-binding: t; -*-
;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT
;; Author: Colin McLear
;;; Commentary:
;; Setup file for skeletons & example for setting up lem-setup files.
;;; Code:

;;;; Auto-insert
;; Skeleton depends on auto-insert, so set that up first.

(use-package auto-insert
  :straight (:type built-in)
  :hook (after-init . auto-insert-mode))

;;;; Package/Library Setup Skeleton
;; When writing lem-modules, insert header from skeleton
(with-eval-after-load "autoinsert"
  (define-auto-insert
    (cons (concat (expand-file-name lem-setup-dir) "lem-setup-.*\\.el")
          "𝛌-Emacs Lisp Skeleton")
    '("𝛌-Emacs Module Description: "
      ";;;; " (file-name-nondirectory (buffer-file-name)) " --- " str
      (make-string (max 2 (- 80 (current-column) 27)) ?\s)
      "-*- lexical-binding: t; -*-" '(setq lexical-binding t)
      "
;; Copyright (C) " (format-time-string "%Y") "
;; SPDX-License-Identifier: MIT
;; Author: Colin McLear
;;; Commentary:
;; " _ "
;;; Code:
(provide '"
      (file-name-base (buffer-file-name))
      ")
;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n"
      )))



(provide 'lem-setup-skeleton)
;;; lem-setup-skeleton.el ends here
