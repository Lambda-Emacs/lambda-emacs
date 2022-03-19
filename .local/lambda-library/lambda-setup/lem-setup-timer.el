;;; Night Timer
;; Inspired by https://github.com/hmatheisen/theme-switcher
;; When emacs is launched in the evening automatically load the dark theme
;; set to dark theme after 6pm
(defvar day-hour 08
  "The hour when the theme goes from dark to light in the morning. Default is 8am. ")

(defvar night-hour 18
  "The hour when the theme goes from light to dark in the evening. Default is 6pm.")

(let ((now (string-to-number (format-time-string "%H"))))
  (if (and (>= now day-hour) (< now night-hour))
      (nano-theme-light)
    (progn
      (cpm/osx-menubar-theme-dark)
      (nano-theme-dark))))



(provide 'setup-timer)
