;;; lem-setup-splash.el --- An alternative splash screen -*- lexical-binding: t; -*-

;; Author: Colin McLear
;; Keywords: startup
;; Version: 0.2
;; Package-Requires:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;  An alternative splash screen:

;; Note: The screen is not shown if there are opened file buffers. For
;;       example, if you start emacs with a filename on the command
;;       line, the splash is not shown.

;;; Code:
(require 'cl-lib)
(require 'all-the-icons)

(defgroup lem-splash nil
  "Extensible splash screen."
  :group 'applications)

;;;; Splash Faces
(defface lem-splash-title-face nil
  "Face for splash title."
  :group 'faces)

(defface lem-splash-header-face nil
  "Face for splash header."
  :group 'faces)

(defface lem-splash-footer-face nil
  "Face for splash footer."
  :group 'faces)

(defface lem-splash-menu-face nil
  "Face for splash menus."
  :group 'faces)

(defface lem-splash-image-face nil
  "Face for splash image."
  :group 'faces)

;;;; Splash Variables

;; Init info
;; See https://github.com/emacs-dashboard/emacs-dashboard/blob/master/dashboard-widgets.el
;; https://github.com/hlissner/doom-emacs/blob/eddaae40e84b5eb1f0136aaba23d918f71b6a986/core/core.el#L479
(defcustom lem-splash-init-info
  (lambda ()
    (let ((package-count 0) (time (emacs-init-time)))
      (cond ((bound-and-true-p package-alist)
             (setq package-count (length package-activated-list)))
            ((boundp 'straight--profile-cache)
             (setq package-count (+ (hash-table-count straight--profile-cache) package-count))))
      (if (zerop package-count)
          (format "Emacs started in %s" time)
        (format "%d packages loaded in %s" package-count time))))
  "Init info with packages loaded and init time."
  :type '(function string)
  :group 'lambda-splash)

;; Banner

;; Banner
(defcustom lem-splash-banner nil
  "Banner picture for splash-screen"
  :type '(string)
  :group 'lambda-splash)

(defvar isep "   "
  "Separator for icon and title.")
(defvar ksep "   "
  "Separator for title and key.")

;;;; Define Splash

(defun lem-splash--erase ()
  ;; check if splash exists and switch if so
  (when-let ((buf (get-buffer "*splash*")))
    (let ((inhibit-read-only t))
      (switch-to-buffer "*splash*")
      (erase-buffer))))

(defun lem-splash-terminal ()
  "A custom splash screen for terminal Emacs"
  (interactive)
  ;; Switch to or create splash buffer
  (or (when (get-buffer "*splash*")
        (switch-to-buffer "*splash*"))
      ;; Hide modeline before window-body-height is computed
      (let* ((splash-buffer (get-buffer-create "*splash*")))
        (with-current-buffer splash-buffer
          (setq header-line-format nil)
          (setq mode-line-format nil)
          (erase-buffer))

        (let* ((buffer-read-only t)
               (window-height (window-height))
               (window-width (window-width))
               (padding-top (max 1 (/ window-height 8)))
               ;; Original ascii image from here:
               ;; https://github.com/Triagle/emax/blob/master/boot.txt
               (banner (lem-get-string-from-file (concat lem-library-dir "lambda-splash.txt"))))

          (with-current-buffer splash-buffer
            ;; Buffer local settings
            (if (one-window-p)
                (setq mode-line-format nil))
            (setq-local cursor-type nil)
            (setq vertical-scroll-bar nil)
            (setq horizontal-scroll-bar nil)
            (setq fill-column window-width)
            (face-remap-add-relative 'link :underline nil)
            (unless (and (display-graphic-p) sys-mac) (menu-bar-mode 0))
            ;; Remove margins to avoid interference with centering
            (setq-local left-margin-width 0
                        right-margin-width 0)
            (set-window-buffer nil (current-buffer))
            ;; Add padding at top
            (insert-char ?\n padding-top)
            (unless lem-splash-banner
              ;; Insert text banner
              (insert banner))
            ;; Position point
            (goto-char (point-min))
            (forward-line (- (/ (window-height) 2) 10))
            (end-of-line)
            ;; Insert header text
            (let ((welcome-line (propertize "           Welcome to 𝛌-Emacs"  'face 'lem-splash-title-face))
                  (version-line (concat (propertize "         GNU Emacs version" 'face 'lem-splash-header-face)
                                        " "
                                        (propertize (format "%d.%d" emacs-major-version emacs-minor-version) 'face 'lem-splash-header-face)))
                  (init-line (let ((init-info (funcall lem-splash-init-info)))
                               (concat " " (propertize init-info 'face 'lem-splash-header-face)))))
              (insert-rectangle (list welcome-line version-line init-line)))
            ;; ;; Vertical padding to bottom
            (goto-char (point-max))
            ;; Footer text
            (defvar lem-splash-footer "   " "Footer text.")
            (save-excursion (insert-char ?\n 2)
                            (insert-char ?\s (- (/ (window-width) 4) 10))
                            (insert
                             (propertize lem-splash-footer 'face 'lem-splash-footer-face)))
            (goto-char (point-min))
            (display-buffer-same-window splash-buffer nil))
          (switch-to-buffer "*splash*"))
        (lem-splash-mode))

(defun lem-splash-screen ()
  "A custom splash screen for Emacs"
  (interactive)
  ;; Switch to or create splash buffer
  (or (when (get-buffer "*splash*")
        (switch-to-buffer "*splash*"))
      ;; Hide modeline before window-body-height is computed
      (let* ((splash-buffer (get-buffer-create "*splash*")))
        (with-current-buffer splash-buffer
          (setq header-line-format nil)
          (setq mode-line-format nil)
          (erase-buffer))

        (let* ((buffer-read-only t)
               (window-height (window-height))
               (window-width (window-width))
               (padding-top (max 1 (/ window-height 8)))
               (image-scaling-factor 2.2)
               ;; scale icons
               (all-the-icons-scale-factor 1.10)
               (all-the-icons-default-adjust -0.02))

          (with-current-buffer splash-buffer
            ;; Buffer local settings
            (if (one-window-p)
                (setq mode-line-format nil))
            (setq-local cursor-type nil)
            (setq vertical-scroll-bar nil)
            (setq horizontal-scroll-bar nil)
            (setq fill-column window-width)
            (face-remap-add-relative 'link :underline nil)
            (unless (and (display-graphic-p) sys-mac) (menu-bar-mode 0))

            ;; Remove margins to avoid interference with centering
            (setq-local left-margin-width 0
                        right-margin-width 0)
            (set-window-buffer nil (current-buffer))

            ;; Add padding at top
            (insert-char ?\n padding-top)

            ;; Create a layout with lambda on the left and text on the right
            (when (and (> window-width 80)
                       (> window-height 20))
              (let* ((image-file (if (eq active-theme 'dark-theme)
                                     (concat lem-library-dir "lambda-logo-white.png")
                                   (concat lem-library-dir "lambda-logo-black.png")))
                     (image (create-image image-file 'png nil :width 300 :height 300))
                     (content-start-col (max 10 (/ (- window-width 60) 2)))
                     (image-col (max 0 (- content-start-col 20)))
                     (text-col (+ content-start-col 15)))
                
                ;; Position and insert the lambda image
                (insert-char ?\s image-col)
                (insert-image image)
                
                ;; Move cursor to same line as image for text
                (forward-line -8)  ; Move up to align with middle of image
                (end-of-line)
                
                ;; Insert header text centered relative to available space
                (let ((welcome-text "Welcome to λ-Emacs")
                      (version-text (format "GNU Emacs version %d.%d" emacs-major-version emacs-minor-version))
                      (init-info (funcall lem-splash-init-info)))
                  
                  ;; Calculate centering for each line
                  (let ((welcome-padding (max 0 (/ (- 40 (length welcome-text)) 2)))
                        (version-padding (max 0 (/ (- 40 (length version-text)) 2)))
                        (init-padding (max 0 (/ (- 40 (length init-info)) 2))))
                    
                    ;; Welcome text
                    (insert-char ?\s (+ welcome-padding 5))
                    (insert (propertize welcome-text 'face 'lem-splash-title-face))
                    (insert-char ?\n 2)
                    
                    ;; Version text  
                    (insert-char ?\s text-col)
                    (insert-char ?\s version-padding)
                    (insert (propertize version-text 'face 'lem-splash-header-face))
                    (insert-char ?\n 2)
                    
                    ;; Init info
                    (insert-char ?\s text-col)
                    (insert-char ?\s init-padding)
                    (insert (propertize init-info 'face 'lem-splash-header-face))
                    (insert-char ?\n 4))))

            ;; Insert menu buttons centered
            (goto-char (point-max))
            (let ((menu-center (max 0 (/ (- window-width 25) 2)))
                  (menu-items '(("calendar" "Agenda" "a" lem-open-agenda-in-workspace "Open Agenda")
                                ("code" "Config" "c" lem-open-emacsd-in-workspace "Visit config directory")
                                ("envelope-o" "Mail" "m" lem-open-email-in-workspace "Open Email in Mu4e")
                                ("book" "Notes" "n" lem-open-notes-in-workspace "Open notes directory")
                                ("folder" "Projects" "p" tabspaces-open-existing-project-and-workspace "Open project & workspace"))))
              (dolist (item menu-items)
                (let ((icon (nth 0 item))
                      (label (nth 1 item))
                      (key (nth 2 item))
                      (action (nth 3 item))
                      (help (nth 4 item)))
                  (insert-char ?\s menu-center)
                  (insert-text-button (concat (all-the-icons-faicon icon) isep label ksep "(" key ")")
                                      'action (lambda (_) (call-interactively action))
                                      'help-echo help
                                      'face 'lem-splash-menu-face
                                      'follow-link t)
                  (insert-char ?\n 1))))


            ;; Add footer with proper spacing
            (goto-char (point-max))
            (let ((footer-padding (max 1 (- window-height (line-number-at-pos) 3))))
              (insert-char ?\n footer-padding))
            
            ;; Footer text centered
            (defvar lem-splash-footer "   " "Footer text.")
            (let ((footer-center (max 0 (/ (- window-width (length lem-splash-footer)) 2))))
              (insert-char ?\s footer-center)
              (insert (propertize lem-splash-footer 'face 'lem-splash-footer-face)))

            (goto-char (point-min))
            (display-buffer-same-window splash-buffer nil)))
          (switch-to-buffer "*splash*"))
        (lem-splash-mode))))))

(defun lem-splash-screen-bury ()
  "Bury the splash screen buffer (immediately)."
  (interactive)
  (when (get-buffer "*splash*")
    (bury-buffer)))

(defun lem-splash-screen-kill ()
  "Kill the splash screen buffer (immediately)."
  (interactive)
  (when (get-buffer "*splash*")
    (kill-buffer)))

;;;; Define Minor Mode
;; Custom splash screen
(defvar lem-splash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'lem-open-agenda-in-workspace)
    (define-key map (kbd "c") 'lem-open-emacsd-in-workspace)
    (define-key map (kbd "m") 'lem-open-email-in-workspace)
    (define-key map (kbd "n") 'lem-open-notes-in-workspace)
    (define-key map (kbd "p") 'lem-open-existing-project-and-workspace)
    (define-key map (kbd "q") 'lem-splash-screen-kill)
    (define-key map (kbd "esc") 'lem-splash-screen-bury)
    (define-key map (kbd "k") 'lem-splash-screen-kill)
    map)
  "Keymap for lem-splash-mode.")

(define-minor-mode lem-splash-mode
  "Emacs minor mode for splash screen."
  :global nil
  :keymap lem-splash-mode-map
  :group 'lambda-emacs
  :require 'lem-setup-splash
  (buffer-disable-undo)
  (auto-revert-mode -1)
  (whitespace-mode -1)
  (when (fboundp 'linum-mode)
    (linum-mode -1))
  (when (>= emacs-major-version 26)
    (display-line-numbers-mode -1))
  (setq-local buffer-read-only t)
  (setq-local cursor-type nil)
  (setq-local hl-line-mode -1)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)
  ;; No margin or fringe in splash buffer
  (setq-local left-margin-width nil
              right-margin-width nil)
  (set-window-fringes (selected-window) 0 0 nil)
  (setq inhibit-startup-screen t
        truncate-lines nil
        inhibit-startup-message t
        inhibit-startup-echo-area-message t)
  (goto-char (point-min)))

;;;; Splash Setup & Refresh Functions 
(defun lem-splash-refresh ()
  "Refresh & recenter splash after window switch."
  (interactive)
  (when (string= (buffer-name) "*splash*")
    (progn
      (ignore-errors
        (lem-splash-screen-kill)
        (lem-splash-screen)))))

(defun lem-splash--setup-splash-hooks ()
  "Initialize splash and setup hooks."
  (progn
    (if (display-graphic-p)
        (lem-splash-screen)
      (lem-splash-terminal)) 
    (add-hook 'window-state-change-hook #'lem-splash-refresh)
    (add-hook 'window-configuration-change-hook  #'lem-splash-refresh)
    (add-hook 'lem-switch-buffer-hook #'lem-splash-refresh)))

;; Install hook after frame parameters have been applied and only if
;; no option on the command line
(when (and (not (member "--no-splash" command-line-args))
           (not (member "--file"      command-line-args))
           (not (member "--insert"    command-line-args))
           (not (member "--find-file" command-line-args)))
  (add-hook 'window-setup-hook #'lem-splash--setup-splash-hooks))

(provide 'lem-setup-splash)

;;; lem-setup-splash.el ends here

