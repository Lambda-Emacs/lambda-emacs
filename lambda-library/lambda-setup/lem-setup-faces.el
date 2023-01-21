;;; lem-setup-faces.el --- Setup for faces          -*- lexical-binding: t; -*-
;; Copyright (C) 2022
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Colin McLear
;;; Commentary:
;;
;;; Code:
;;;; Outline Faces
;; Make outline faces look better
(use-package outline-minor-faces
  :after outline
  :hook ((emacs-lisp-mode lisp-interaction-mode lisp-mode) . outline-minor-faces-mode))

;;;; What Face?
;; https://stackoverflow.com/a/66287459/6277148
(defun what-face (pos)
  "State the face at point POS in the minibuffer."
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;;;; Underline
(customize-set-variable 'x-underline-at-descent-line t)

;;;; Dim inactive windows
(use-package dimmer
  :hook (after-init . dimmer-mode)
  :custom
  (dimmer-prevent-dimming-predicates '(window-minibuffer-p))
  (dimmer-fraction 0.5)
  (dimmer-adjustment-mode :foreground)
  (dimmer-use-colorspace :rgb)
  (dimmer-watch-frame-focus-events nil)
  :config
  (dimmer-configure-which-key)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  (dimmer-configure-vertico))

(defun dimmer-configure-vertico ()
  "Convenience settings for Dimmer & Vertico users."
  (with-no-warnings
    (add-to-list
     'dimmer-buffer-exclusion-regexps "^ \\*Vertico\\*$")))

;;;; Cursor
;; don't show cursor in inactive windows
(customize-set-variable 'cursor-in-non-selected-windows nil)

;;;; Reveal Mode
;; Toggle uncloaking of invisible text near point, including folded org headlines (Reveal mode).
(use-package reveal
  :defer 1
  :config
  (setq reveal-auto-hide nil)
  (global-reveal-mode))

;;;; SVG Library (For Tags/Labels/etc.)
  ;;; SVG Tag Mode
(use-package svg-tag-mode
  :when (image-type-available-p 'svg)
  :hook (prog-mode . svg-tag-mode)
  :config
  (setq svg-tag-tags
        '(;; Replaces any occurence of :XXX: with a dynamic SVG tag displaying XXX
          ("\\(:[A-Z]+:\\)" . ((lambda (tag)
                                 (svg-tag-make tag :face 'success :inverse t :beg 1 :end -1))))
          ;; other tags
          ("DONE:"  . ((lambda (tag) (svg-tag-make "DONE:"  :face 'fringe  :inverse t ))))
          ("FIXME:" . ((lambda (tag) (svg-tag-make "FIXME:" :face 'error :inverse t))))
          ("HACK:"  . ((lambda (tag) (svg-tag-make "HACK:"  :face 'warning :inverse t))))
          ("NOTE:"  . ((lambda (tag) (svg-tag-make "NOTE:"  :face 'warning :inverse t))))
          ("TODO:"  . ((lambda (tag) (svg-tag-make "TODO:"  :face 'warning :inverse t)))))))


;;;; Widgets
(use-package wid-edit
  :ensure nil
  :defer 1
  :custom
  ;; No ugly button for checkboxes
  (widget-image-enable nil))

;;;; Highlight

;;;;; LIN (Make HL Line Better)
(use-package lin
  :disabled
  :config
  (setq lin-mode-hooks
        '(dired-mode-hook
          elfeed-search-mode-hook
          git-rebase-mode-hook
          grep-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          mu4e-headers-mode
          occur-mode-hook
          org-agenda-mode-hook
          proced-mode-hook
          tabulated-list-mode-hook))
  (lin-global-mode 1))

;;;;; Highlight Numbers & TODOS
(use-package highlight-numbers
  :defer t
  :commands highlight-numbers-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package hl-todo
  :defer t
  :commands hl-todo-mode
  :init
  ;; (add-hook 'org-mode-hook #'hl-todo-mode)
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'markdown-mode-hook #'hl-todo-mode))

;; hydra for TODOs
(with-eval-after-load 'hydra
  (defhydra lem-hydra-todo
    (:pre
     (hl-todo-mode 1)
     :post
     (hl-todo-mode -1))
    "Todo"
    ("n" hl-todo-next "Next")
    ("p" hl-todo-previous "Previous")
    ("o" hl-todo-occur "Occur")
    ("q" nil "Quit" :color blue :exit t)))

;; ;;https://github.com/erickgnavar/dotfiles/tree/master/.emacs.d#highlight-todo-fixme-etc
;; (defun lem-highlight-todo-like-words ()
;;   (font-lock-add-keywords
;;    nil `(("\\<\\(FIXME\\|TODO\\|NOTE\\)"
;;           1 font-lock-warning-face t))))


;; (add-hook 'prog-mode-hook 'my/highlight-todo-like-words)


;;;;; Highlight Cursor Line with Pulse
;; From https://karthinks.com/software/batteries-included-with-emacs/
;; Replace external package with internal command

(use-package pulse
  :defer 1
  :bind
  ("C-<return>" . pulse-line)
  :commands (pulse-line pulse-momentary-highlight-one-line)
  :config
  (setq pulse-delay 0.08)
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (interactive)
    (pulse-momentary-highlight-one-line (point)))
  ;; pulse for commands
  (dolist (command '(scroll-up-command scroll-down-command
                                       recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line))
  ;; pulse on window change
  (push 'pulse-line window-selection-change-functions))

;;;; Goggles (Highlight Changes)
(use-package goggles
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)) ;; set to nil to disable pulsing

;;;; Empty Lines
;; Don't show empty lines.
;; .. Allows you to tell if there are blank lines at the end of the file.
(setq-default indicate-empty-lines nil)


(provide 'lem-setup-faces)
;;; lem-setup-faces.el ends here
