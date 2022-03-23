;;;; lem-setup-fonts.el ---Setup for fonts           -*- lexical-binding: t; -*-
;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT
;; Author: Colin McLear

;;; Commentary:
;;
;;; Code:

;;;; Fonts

(use-package fontset
  :straight (:type built-in)
  :custom
  ;; Set this to nil to set symbols entirely separately
  (use-default-font-for-symbols t)
  :config
  ;; Use symbola for proper symbol glyphs
  (when (member "Symbola" (font-family-list))
    (set-fontset-font
     t 'symbol "Symbola" nil))
  ;; Use Apple emoji
  ;; NOTE that emoji here must be set to unicode to get color emoji
  (when (member "Apple Color Emoji" (font-family-list))
    (set-fontset-font
     t 'unicode (font-spec :family "Apple Color Emoji") nil 'append)))

(use-package faces
  :straight (:type built-in)
  :config
  (set-face-attribute 'default nil
                      :font   "SF Mono"
                      :height 130
                      :weight 'normal)
  (set-face-attribute 'variable-pitch nil
                      :font "Avenir Next"
                      :height 200
                      :weight 'normal))

;; Set default line spacing (in pixels)
(setq-default line-spacing 0.05)


;;;;; Font Lock
(use-package font-lock
  :straight (:type built-in)
  :custom
  ;; Max font lock decoration (set nil for less)
  (font-lock-maximum-decoration t)
  ;; No limit on font lock
  (font-lock-maximum-size nil))


;;;;; Scale Text
;; When using `text-scale-increase', this sets each 'step' to about one point size.
(setq text-scale-mode-step 1.08)
(global-set-key (kbd "s-=") 'text-scale-increase)
(global-set-key (kbd "s--") 'text-scale-decrease)
(global-set-key (kbd "s-0") 'text-scale-adjust)

;;;;; Bidirectional Text
;; Disable bidirectional text support. Why?
;; .. slight performance improvement.
(setq bidi-display-reordering nil)

;;;;; Icons
(use-package all-the-icons
  :defer t)

(use-package font-lock+
  :defer 1)
;; icons for dired
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :defer t
  :commands all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; No ugly button for checkboxes
(setq widget-image-enable nil)

;;;;; Emoji
(use-package emojify
  :commands (emojify-mode emojify-apropos-emoji)
  :hook ((prog-mode markdown-mode org-mode) . emojify-mode)
  :config
  (setq emojify-emojis-dir (concat lem-etc-dir "emojis")))

(provide 'lem-setup-fonts)
;;; lem-setup-fonts.el ends here
