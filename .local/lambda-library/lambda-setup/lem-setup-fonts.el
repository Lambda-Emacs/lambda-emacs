;;;; lem-setup-fonts.el ---Setup for fonts           -*- lexical-binding: t; -*-
;; Copyright (C) 2022
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Colin McLear

;;; Commentary:
;;
;;; Code:

;;;; Fonts

(use-package fontset
  :straight (:type built-in)
  :defer 1
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
    (set-fontset-font t 'emoji
                      '("Apple Color Emoji" . "iso10646-1") nil 'prepend))
  ;; Fall back font for missing glyph
  (defface fallback '((t :family "Fira Code"
                         :inherit fringe)) "Fallback")
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'fallback))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'fallback)))

;;;; Set Default & Variable Pitch Fonts
(defun lem-ui--set-default-font (spec)
  "Set the default font based on SPEC.
SPEC is expected to be a plist with the same key names
as accepted by `set-face-attribute'."
  (when spec
    (apply 'set-face-attribute 'default nil spec)))

(defun lem-ui--set-variable-width-font (spec)
  "Set the default font based on SPEC.
SPEC is expected to be a plist with the same key names
as accepted by `set-face-attribute'."
  (when spec
    (apply 'set-face-attribute 'variable-pitch nil spec)))

(defcustom lem-ui-default-font nil
  "The configuration of the `default' face.
Use a plist with the same key names as accepted by `set-face-attribute'."
  :group 'lambda-emacs
  :type '(plist :key-type: symbol)
  :tag "Lambda-Emacs Default font"
  :set (lambda (sym val)
         (let ((prev-val (if (boundp 'lem-ui-default-font)
                             lem-ui-default-font
                           nil)))
           (set-default sym val)
           (when (and val (not (eq val prev-val)))
             (lem-ui--set-default-font val)))))

(defcustom lem-ui-variable-width-font nil
  "The configuration of the `default' face.
Use a plist with the same key names as accepted by `set-face-attribute'."
  :group 'lambda-emacs
  :type '(plist :key-type: symbol)
  :tag "Lambda-Emacs Variable width font"
  :set (lambda (sym val)
         (let ((prev-val (if (boundp 'lem-ui-variable-width-font)
                             lem-ui-variable-width-font
                           nil)))
           (set-default sym val)
           (when (and val (not (eq val prev-val)))
             (lem-ui--set-variable-width-font val)))))

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

(provide 'lem-setup-fonts)
;;; lem-setup-fonts.el ends here
