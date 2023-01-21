;;; lem-setup-vc.el --- setup for version control -*- lexical-binding: t -*-

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

;; Version Control -- I use git for version control. Magit is a great interface
;; for git projects. It's much more pleasant to use than the standard git
;; interface on the command line. I've set up some easy keybindings to access
;; magit and related packages.

;;; Code:

;;;; VC
(use-package vc
  :hook (after-init . vc-mode)
  :custom (vc-follow-symlinks t))

(use-package vc-git
  :after vc
  :config
  (setq vc-git-diff-switches "--patch-with-stat")
  (setq vc-git-print-log-follow t))

(use-package vc-annotate
  :after vc
  :config
  (setq vc-annotate-display-mode 'scale))

;;;; Magit
(use-package magit
  :commands
  (magit-blame-mode
   magit-commit
   magit-diff
   magit-log
   magit-status)
  :hook (git-commit-mode . turn-on-flyspell)
  :bind ((:map magit-log-mode-map
          ;; Keybindings for use with updating packages interactively
          ("Q" . #'exit-recursive-edit)))
  :init
  ;; Suppress the message we get about "Turning on
  ;; magit-auto-revert-mode" when loading Magit.
  (setq magit-no-message '("Turning on magit-auto-revert-mode..."))
  :config
  (setq magit-log-margin '(t "%Y-%m-%d.%H:%M:%S "  magit-log-margin-width nil 18))
  (setq magit-refresh-status-buffer t)
  ;; Set git for macos & homebrew
  (if (and sys-mac homebrew-bin)
      (setq magit-git-executable (concat homebrew-bin "/git"))
    (setq magit-git-executable "/usr/bin/git"))
  ;; Fine grained diffs
  (setq magit-diff-refine-hunk t)
  ;; control magit initial visibility
  (setq magit-section-initial-visibility-alist
        '((stashes . hide) (untracked . hide) (unpushed . hide) ([unpulled status] . show)))
  (global-git-commit-mode t) ; use emacs as editor for git commits

  ;; refresh status buffer
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t)
  ;; no magit header line as it conflicts w/bespoke-modeline
  (advice-add 'magit-set-header-line-format :override #'ignore)
  ;; display magit setting
  (setq magit-display-buffer-function #'lem-display-magit-in-other-window)
  ;; (setq magit-display-buffer-function #'lem-magit-display-buffer-pop-up-frame)
  )

;; optional: display magit status in new frame
(defun lem-magit-display-buffer-pop-up-frame (buffer)
  (if (with-current-buffer buffer (eq major-mode 'magit-status-mode))
      (display-buffer buffer
                      '((display-buffer-reuse-window
                         display-buffer-pop-up-frame)
                        (reusable-frames . t)))
    (magit-display-buffer-traditional buffer)))

;; optional: display magit in other window & create one if only 1 window
(defun lem-display-magit-in-other-window (buffer)
  (if (one-window-p)
      (progn
        (split-window-right)
        (other-window 1)
        (display-buffer buffer
                        '((display-buffer-reuse-window))))
    (magit-display-buffer-traditional buffer)))

;; settings for committing using magit
(use-package git-commit
  :after magit
  :hook (git-commit-mode . cpm/git-commit-auto-fill-everywhere)
  :custom (git-commit-summary-max-length 50)
  :preface
  (defun cpm/git-commit-auto-fill-everywhere ()
    "Ensures that the commit body does not exceed 80 characters."
    (setq fill-column 80)
    (setq-local comment-auto-fill-only-comments nil))
  :config
  (with-eval-after-load 'meow
    (add-hook 'git-commit-mode-hook
              (lambda ()
                (meow-insert-mode)))))

;;;; Git Gutter HL (Diff-HL)
;; Nice vc highlighting in margin/fringe
;; See https://www.reddit.com/r/emacs/comments/suxc9b/modern_gitgutter_in_emacs/
;; And https://github.com/jimeh/.emacs.d/blob/master/modules/version-control/siren-diff-hl.el

(use-package diff-hl
  :ensure t
  :hook
  ((prog-mode . diff-hl-mode)
   (text-mode . diff-hl-mode)
   (dired-mode . diff-hl-dired-mode)
   (magit-pre-refresh . diff-hl-magit-pre-refresh)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :custom
  (diff-hl-side 'left)
  (diff-hl-fringe-bmp-function 'cpm--diff-hl-fringe-bmp-from-type)
  (diff-hl-fringe-face-function 'cpm--diff-hl-fringe-face-from-type)
  (diff-hl-margin-symbols-alist
   '((insert . "┃")
     (delete . "┃")
     (change . "┃")
     (unknown . "?")
     (ignored . "i")))
  :init
  (defun cpm--diff-hl-fringe-face-from-type (type _pos)
    (intern (format "cpm--diff-hl-%s" type)))

  (defun cpm--diff-hl-fringe-bmp-from-type(type _pos)
    (intern (format "cpm--diff-hl-%s" type)))

  (defun cpm--diff-hl-set-render-mode ()
    (diff-hl-margin-mode (if window-system -1 1)))
  :config
  (diff-hl-margin-mode 1)
  (define-fringe-bitmap 'diff-hl-insert
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'diff-hl-change
    [#b00000011] nil nil '(center repeated))
  (define-fringe-bitmap 'diff-hl-delete
    [#b00000011] nil nil '(center repeated)))

;;;; Diff Files with Vdiff
(use-package vdiff-magit
  :defer t
  :init
  (with-eval-after-load 'magit
    (define-key magit-mode-map "e" #'vdiff-magit-dwim)
    (define-key magit-mode-map "E" #'vdiff-magit)
    (transient-suffix-put 'magit-dispatch "e" :description "vdiff (dwim)")
    (transient-suffix-put 'magit-dispatch "e" :command 'vdiff-magit-dwim)
    (transient-suffix-put 'magit-dispatch "E" :description "vdiff")
    (transient-suffix-put 'magit-dispatch "E" :command 'vdiff-magit)))

;;;; Ediff
;; Don't open ediff in new frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;;;; Quick Commits
;; Make a quick commit without opening magit. This is a version of a
;; workflow I used to use in Sublime Text. Perfect for short commit messages.
(defun lem-quick-commit ()
  "Quickly commit the current file-visiting buffer from the mini-buffer."
  (interactive)
  (shell-command (concat "Git add " (buffer-file-name) " && Git commit -m '" (read-string "Enter commit message: ") "'")))

;;; End Setup VC
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lem-setup-vc)
;;; lem-setup-vc.el ends here
