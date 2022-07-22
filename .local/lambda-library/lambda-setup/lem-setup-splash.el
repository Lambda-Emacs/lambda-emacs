;;; setup-splash.el --- An alternative splash screen -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Nicolas .P Rougier

;; Author: Nicolas P. Rougier <nicolas.rougier@inria.fr>
;; Mondifications: Colin McLear
;; URL: https://github.com/rougier/emacs-splash
;; Keywords: startup
;; Version: 0.1
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

(defgroup lem-splash nil
  "Extensible splash screen."
  :group 'applications)

(defun point-calc-lines-offset (pt lines)
  (save-excursion
    (goto-char pt)
    (forward-line lines)
    (point)))

;; Init info
;; See https://github.com/emacs-dashboard/emacs-dashboard/blob/master/dashboard-widgets.el
;; https://github.com/hlissner/doom-emacs/blob/eddaae40e84b5eb1f0136aaba23d918f71b6a986/core/core.el#L479

(defcustom splash-init-info
  (lambda ()
    (let ((package-count 0) (time (emacs-init-time)))
      (setq package-count (- (length load-path) (length (get 'load-path 'initial-value))))
      (if (zerop package-count)
          (format "Emacs started in %s" time)
        (format "%d packages loaded in %s" package-count time))))
  "Init info with packages loaded and init time."
  :type '(function string)
  :group 'lambda-splash)

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

;;; Splash Variables

(defvar lem-splash--max-rows 300
  "Maximum number of rows a window can have")

(defvar lem-splash--max-columns 500
  "Maximum number of columns a window can have")

(defvar lem-splash--box-dimensions nil
  "Variable used to store dimensions (rows columns) of banner text.")

;;; Define Splash
(defun lem-splash-screen ()
  "A custom splash screen for Emacs"

  (interactive)

  ;; check if splash exists and switch if so
  (if (get-buffer "*splash*")
      (switch-to-buffer "*splash*")

    ;; Otherwise create splash and go...
    ;; Hide modeline before window-body-height is computed
    (let* ((splash-buffer (get-buffer-create "*splash*")))
      (with-current-buffer splash-buffer
        (setq header-line-format nil)
        (setq mode-line-format nil)))

    (let* ((buffer-read-only t)
           (splash-buffer  (get-buffer-create "*splash*"))
           (height (/ lem-splash--max-rows 2))
	       (width (/ lem-splash--max-columns 2))
           ;; ascii image from here:
           ;; https://github.com/Triagle/emax/blob/master/boot.txt
           (image          (lem-get-string-from-file (concat lem-library-dir "lambda-splash.txt"))))


      (with-current-buffer splash-buffer
        (erase-buffer)

        ;; Buffer local settings
        (if (one-window-p)
            (setq mode-line-format nil))
        (setq-local cursor-type nil)
        (setq vertical-scroll-bar nil)
        (setq horizontal-scroll-bar nil)
        (setq fill-column width)
        (face-remap-add-relative 'link :underline nil)
        (unless (and (display-graphic-p) sys-mac) (menu-bar-mode 0))
        ;; Set padding
        (setq-local left-margin-width 10 right-margin-width 0) ; Define new widths.
        (set-window-buffer nil (current-buffer))

        ;; Add padding at top
        (insert-char ?\n 2)

        ;; Insert image
        (goto-char width)
        (save-excursion
          (insert (propertize image 'face 'lem-splash-image-face)))

        ;; Insert text
        (goto-char (+ width 68))
        (save-excursion
          (insert (propertize "Welcome to 𝛌-Emacs"  'face 'lem-splash-title-face)))

        (goto-char (+ (* height 3) 8))
        (save-excursion (insert (concat (propertize "GNU Emacs version" 'face 'lem-splash-header-face)
                                        " "
                                        (propertize (format "%d.%d" emacs-major-version emacs-minor-version) 'face 'lem-splash-header-face))))

        (goto-char (- (* height 4) 4))
        (save-excursion (let ((init-info (funcall splash-init-info)))
                          (insert (propertize init-info 'face 'lem-splash-header-face))))

        (goto-char (- (* height 6) 4))
        (save-excursion (insert-text-button " [a] Agenda "
                                            'action (lambda (_) (lem-open-agenda-in-workspace))
                                            'help-echo "Visit setup directory"
                                            'face 'lem-splash-menu-face
                                            'follow-link t))
        (goto-char (- (* height 7) 18))
        (save-excursion (insert-text-button " [c] Config "
                                            'action (lambda (_) (lem-open-emacsd-in-workspace))
                                            'help-echo "Visit setup directory"
                                            'face 'lem-splash-menu-face
                                            'follow-link t))
        (goto-char (- (* height 8) 32))
        (save-excursion (insert-text-button " [m] Mail "
                                            'action (lambda (_)  (lem-open-email-in-workspace))
                                            'help-echo "Open Email in Mu4e"
                                            'face 'lem-splash-menu-face
                                            'follow-link t))

        (goto-char (- (* height 9) 48))
        (save-excursion (insert-text-button " [n] Notes "
                                            'action (lambda (_)  (lem-open-notes-in-workspace))
                                            'help-echo "Visit setup directory"
                                            'face 'lem-splash-menu-face
                                            'follow-link t))

        (goto-char (- (* height 10) 63))
        (save-excursion (insert-text-button " [p] Projects "
                                            'action (lambda (_)  (tabspaces-open-existing-project-and-workspace))
                                            'help-echo "Open project & workspace"
                                            'face 'lem-splash-menu-face
                                            'follow-link t))

        ;; Vertical padding to bottom
        (goto-char (point-max))

        ;; Footer text
        (defvar lem-splash-footer "   " "Footer text.")
        (save-excursion (insert-char ?\n 4)
                        (insert
                         (propertize lem-splash-footer 'face 'lem-splash-footer-face)))



        (goto-char (point-min))
        (display-buffer-same-window splash-buffer nil))
      (switch-to-buffer "*splash*")))
  (lem-splash-mode))

(defun splash-screen-bury ()
  "Bury the splash screen buffer (immediately)."
  (interactive)
  (when (get-buffer "*splash*")
    (bury-buffer)))

(defun splash-screen-kill ()
  "Kill the splash screen buffer (immediately)."
  (interactive)
  (when (get-buffer "*splash*")
    (kill-buffer)))

;;; Define Minor Mode
;; Custom splash screen
(defvar lem-splash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'lem-open-agenda-in-workspace)
    (define-key map (kbd "c") 'lem-open-emacsd-in-workspace)
    (define-key map (kbd "m") 'lem-open-email-in-workspace)
    (define-key map (kbd "n") 'lem-open-notes-in-workspace)
    (define-key map (kbd "p") 'lem-open-existing-project-and-workspace)
    (define-key map (kbd "q") 'splash-screen-bury)
    (define-key map (kbd "esc") 'splash-screen-bury)
    (define-key map (kbd "k") 'splash-screen-kill)
    map)
  "Keymap for lem-splash-mode.")

(define-minor-mode lem-splash-mode
  "Emacs minor mode for splash screen."
  :global nil
  :keymap lem-splash-mode-map
  :group 'lambda-emacs
  :require 'lem-setup-splash.el

  (buffer-disable-undo)
  (whitespace-mode -1)
  (linum-mode -1)
  (setq-local buffer-read-only t)
  (setq-local cursor-type nil)
  (setq-local hl-line-mode -1)
  (setq-local mode-line-format nil)
  (setq-local header-line-format nil)

  ;; No margin or fringe in splash buffer
  (setq-local left-margin-width nil
              right-margin-width nil)
  (set-window-fringes (selected-window) 0 0 nil)

  (when (>= emacs-major-version 26)
    (display-line-numbers-mode -1))
  (setq inhibit-startup-screen t
        truncate-lines nil
        inhibit-startup-message t
        inhibit-startup-echo-area-message t)
  (goto-char (point-min)))

;; Suppress any startup message in the echo area
(run-with-idle-timer 0.05 nil (lambda() (message nil)))

;; Install hook after frame parameters have been applied and only if
;; no option on the command line
(if (and (not (member "--no-splash" command-line-args))
         (not (member "--file"      command-line-args))
         (not (member "--insert"    command-line-args))
         (not (member "--find-file" command-line-args)))
    (progn
      (add-hook 'window-setup-hook 'lem-splash-screen)))

(provide 'lem-setup-splash)
;;; setup-splash.el ends here
