;; List packages for testing with clean emacs
(use-package humanoid-themes
  :straight t
  :custom
  (humanoid-themes-custom-colors '((act1 . "#ff0000")
                                   (act2 . "#0000ff")
                                   (base . "#0000ff")
                                   (bg2  . "#ff0000")))
  :config
  (load-theme 'humanoid-dark t))

;; (use-package all-the-icons
;;   :straight t)

;; (defun lem-display-time ()
;;   "Display the time when `display-time-mode' is non-nil."
;;   (when display-time-mode
;;     (require 'all-the-icons)
;;     (let* ((hour (string-to-number (format-time-string "%I")))
;;            (icon (all-the-icons-wicon (format "time-%s" hour) :height 1.3 :v-adjust 0.0)))
;;       (concat
;;        (propertize
;;         (format " %s " icon) 'face `(:height 1.0 ,(all-the-icons-wicon-family)) 'display '(raise -0.0))))))

;; (setq display-time-format (concat " " (lem-display-time) " %I:%M:%S"))


;; (setq display-time-string-forms
;;       '((propertize (concat (lem-display-time) " " 24-hours ":" minutes " ")
;;  		            )))

;; (display-time-mode 1)

;;; Provide
(provide 'lem-setup-test)
