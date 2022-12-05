;;; lem-setup-buffers.el --- summary -*- lexical-binding: t -*-

;; Author: Colin McLear
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

;; Setup for buffers

;;; Code:

;;;; Hooks
;; See https://github.com/doomemacs/doomemacs/blob/master/lisp/doom-ui.el
(defvar lem-switch-buffer-hook nil
  "A list of hooks run after changing the current buffer.")

(defun lem-run-switch-buffer-hooks-h (&optional _)
  (let ((gc-cons-threshold most-positive-fixnum)
        (inhibit-redisplay t))
    (run-hooks 'lem-switch-buffer-hook)))

;; Initialize `lem-switch-buffer-hook'
(add-hook 'window-buffer-change-functions #'lem-run-switch-buffer-hooks-h)
;; `window-buffer-change-functions' doesn't trigger for files visited via the server.
(add-hook 'server-visit-hook #'lem-run-switch-buffer-hooks-h)

;;;; Scrolling
(use-package emacs
  :straight (:type built-in)
  :config
  (message "*Loading scrolling settings...*")
  ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
  ;; for tall lines.
  (setq auto-window-vscroll nil)
  ;; For centered cursor scrolling
  ;; see https://two-wrongs.com/centered-cursor-mode-in-vanilla-emacs
  ;; Smooth Vertical Scroll
  (setq scroll-step 1)
  (setq scroll-margin 3)
  (setq scroll-conservatively 101)
  (setq scroll-up-aggressively 0.01)
  (setq scroll-down-aggressively 0.01)
  (setq auto-window-vscroll nil)
  (setq fast-but-imprecise-scrolling nil)
  ;; Horizontal Scroll
  (setq hscroll-step 1)
  (setq hscroll-margin 1))

(use-package mwheel
  :straight (:type built-in)
  :config
  ;; Optimize mouse wheel scrolling for smooth-scrolling trackpad use.
  ;; Trackpads send a lot more scroll events than regular mouse wheels,
  ;; so the scroll amount and acceleration must be tuned to smooth it out.
  (setq
   ;; If the frame contains multiple windows, scroll the one under the cursor
   ;; instead of the one that currently has keyboard focus.
   mouse-wheel-follow-mouse 't
   ;; Completely disable mouse wheel acceleration to avoid speeding away.
   mouse-wheel-progressive-speed nil
   mwheel-coalesce-scroll-events t
   ;; The most important setting of all! Make each scroll-event move 1 line at a
   ;; time (instead of 5 at default). Simply hold down shift to move twice as
   ;; fast. Perfect for trackpads.
   mouse-wheel-scroll-amount '(1 ((shift) . 2))))

;; Don't use pixel-scroll by default -- it causes janky behavior on MacOS
(use-package pixel-scroll
  :straight (:type built-in)
  :config
  (pixel-scroll-mode -1))


;;;; Mouse
;; Don't be afraid of the mouse!
;; For ideas see https://ruzkuku.com/texts/emacs-mouse.html
(use-package mouse
  :straight (:type built-in)
  :config
  ;; Focus follows mouse
  (setq mouse-autoselect-window t
        focus-follows-mouse t)
  (setq context-menu-functions
        '(context-menu-ffap
          occur-context-menu
          context-menu-region
          context-menu-undo
          context-menu-dictionary)))


;;;; Switch to Buffer Preserve Window

;; switch-to-buffer tries to preserve window-point
(setq switch-to-buffer-preserve-window-point t)

;;;; Unique buffers
(use-package uniquify
  :straight (:type built-in)
  :defer 3
  :config
  (setq uniquify-buffer-name-style 'reverse
        uniquify-separator " • "
        uniquify-after-kill-buffer-p t
        uniquify-ignore-buffers-re "^\\*"))

;;;; Buffer Modes
;; from http://www.jurta.org/en/emacs/dotemacs, set the major mode
;; of buffers that are not visiting a file
(setq-default major-mode (lambda ()
                           (if buffer-file-name
                               (fundamental-mode)
                             (let ((buffer-file-name (buffer-name)))
                               (set-auto-mode)))))

;;;; Autorevert
(use-package autorevert
  :straight (:type built-in)
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-interval .5)
  (revert-without-query '(".*")) ;; disable revert query
  (global-auto-revert-non-file-buffers t)
  :config
  (global-auto-revert-mode))


;;;; Revert All Buffers
(use-package revert-buffer-all
  :straight (:type git :host codeberg :repo "ideasman42/emacs-revert-buffer-all")
  :commands (revert-buffer-all))

;;;; iBuffer
;; A better list of buffers
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
       name))))

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

;;;; Popper (Pop-up Buffers)
(use-package popper
  :straight (:type git :host github :repo "karthink/popper")
  :hook (after-init . popper-mode)
  :bind (("M-`"   . popper-toggle-latest)
         ("C-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :custom
  (popper-window-height 20)
  (popper-display-control t)
  ;; set display to top -- if user prefers they can set this to `bottom'
  (popper-display-function #'popper-select-popup-at-top)
  ;; group by project.el project root, with fall back to default-directory
  (popper-group-function #'popper-group-by-directory)
  ;; Set popper buffers
  (popper-reference-buffers
   '("\\*Messages\\*"
     "Output\\*$"
     "\\*Async Shell Command\\*"
     help-mode
     compilation-mode))
  :init
  ;; For echo area hints
  (popper-echo-mode +1)
  :config
  ;; Display functions for popup buffers at top, where they are easy to read.
  ;; This is just a slight modification of the existing functions.
  (defun popper-select-popup-at-top (buffer &optional _alist)
    "Display and switch to popup-buffer BUFFER at the top of the screen."
    (let ((window (popper-display-popup-at-top buffer _alist)))
      (select-window window)))

  (defun popper-display-popup-at-top (buffer &optional _alist)
    "Display popup-buffer BUFFER at the top of the screen."
    (display-buffer-in-side-window
     buffer
     `((window-height . ,popper-window-height)
       (side . top)
       (slot . 1)))))

;;;; Xwidget Browser
(use-package xwidget
  :straight (:type built-in)
  :defer 1
  :config
  ;; No query on kill
  (remove-hook 'kill-buffer-query-functions #'xwidget-kill-buffer-query-function)
  ;; NOTE: Fix load progress error
  (defun xwidget-webkit-estimated-load-progress (session)
    1.0))

(use-package xwwp-follow-link
  :straight (:host github :repo "canatella/xwwp")
  :custom
  (xwwp-follow-link-completion-system 'default)
  :bind (:map xwidget-webkit-mode-map
         ("v" . xwwp-follow-link)))

;;;; Fringe
(use-package fringe
  :straight (:type built-in)
  :custom
  ;; allow fringe indicators
  (fringe-mode '(1 . 0)))

;;; Provide
(provide 'lem-setup-buffers)
;;; lem-setup-buffers.el ends here
