;;;; lem-setup-fonts.el ---Setup for fonts           -*- lexical-binding: t; -*-
;; Copyright (C) 2022
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Colin McLear

;;; Commentary:
;;
;;; Code:

;;;; Fonts

(use-package fontset
  :ensure nil
  :custom
  ;; Set this to nil to set symbols entirely separately
  ;; Need it set to `t` in order to display org-modern-indent faces properly
  (use-default-font-for-symbols t)
  :config
  ;; Use symbola for proper symbol glyphs, but have some fallbacks
  (cond ((lem-font-available-p "Symbola")
         (set-fontset-font
          t 'symbol "Symbola" nil))
        ((lem-font-available-p "Apple Symbols")
         (set-fontset-font
          t 'symbol "Apple Symbols" nil))
        ((lem-font-available-p "Symbol")
         (set-fontset-font
          t 'symbol "Symbol" nil))
        ((lem-font-available-p "Segoe UI Symbol")
         (set-fontset-font
          t 'symbol "Segoe UI Symbol" nil)))
  ;; Use Apple emoji
  ;; NOTE that emoji here may need to be set to unicode to get color emoji
  (when (and (>= emacs-major-version 28)
             (lem-font-available-p "Apple Color Emoji"))
    (set-fontset-font t 'emoji
                      '("Apple Color Emoji" . "iso10646-1") nil 'prepend))
  ;; Fall back font for missing glyph
  (defface fallback '((t :family "Fira Code"
                         :inherit fringe)) "Fallback")
  (set-display-table-slot standard-display-table 'truncation
                          (make-glyph-code ?… 'fallback))
  (set-display-table-slot standard-display-table 'wrap
                          (make-glyph-code ?↩ 'fallback)))


;; Set default line spacing. If the value is an integer, it indicates
;; the number of pixels below each line. A decimal number is a scaling factor
;; relative to the current window's default line height. The setq-default
;; function sets this for all buffers. Otherwise, it only applies to the current
;; open buffer
(setq-default line-spacing 0.1)

;;;;; Font Lock
(use-package font-lock
  :ensure nil
  :defer 1
  :custom
  ;; Max font lock decoration (set nil for less)
  (font-lock-maximum-decoration t)
  ;; No limit on font lock
  (font-lock-maximum-size nil))

;;;;; Scale Text
;; When using `text-scale-increase', this sets each 'step' to about one point size.
(setq text-scale-mode-step 1.08)
(bind-key* "s-=" #'text-scale-increase)
(bind-key* "s--" #'text-scale-decrease)
(bind-key* "s-0" #'text-scale-adjust)

;;;;; Icons
;; Check for icons FIXME: this should be less verbose but haven't been able to
;; get a `dolist` function working ¯\_(ツ)_/¯
(defun lem-font--icon-check ()
  (cond ((and (lem-font-available-p "Weather Icons")
              (lem-font-available-p "github-octicons")
              (lem-font-available-p "FontAwesome")
              (lem-font-available-p "all-the-icons")
              (lem-font-available-p "file-icons")
              (lem-font-available-p "Material Icons"))
         (message "Icon fonts already installed!"))
        ((and (not (member unicode-fonts (font-family-list)))
              (not sys-win))
         (message "Installing necessary fonts")
         (all-the-icons-install-fonts 'yes))
        (t
         (message "Please install fonts."))))

(defun lem-font--init-all-the-icons-fonts ()
  (when (fboundp 'set-fontset-font)
    (dolist (font (list "Weather Icons"
                        "github-octicons"
                        "FontAwesome"
                        "all-the-icons"
                        "file-icons"
                        "Material Icons"))
      (set-fontset-font t 'unicode font nil 'prepend))))

(use-package all-the-icons
  :if (display-graphic-p)
  :commands (all-the-icons-octicon
             all-the-icons-faicon
             all-the-icons-fileicon
             all-the-icons-wicon
             all-the-icons-material
             all-the-icons-alltheicon)
  :init
  (add-hook 'after-setting-font-hook #'lem-font--icon-check)
  :config
  (add-hook 'after-setting-font-hook #'lem-font--init-all-the-icons-fonts))

;; icons for dired
(use-package all-the-icons-dired
  :if (display-graphic-p)
  :defer t
  :commands all-the-icons-dired-mode
  :init
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; Completion Icons
(use-package all-the-icons-completion
  :if (display-graphic-p)
  :load-path (lambda () (concat lem-user-elisp-dir "all-the-icons-completion/"))
  :hook (emacs-startup . all-the-icons-completion-mode)
  :config
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))


(provide 'lem-setup-fonts)
;;; lem-setup-fonts.el ends here
