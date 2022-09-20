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

;;; Splash Variables

;; Init info
;; See https://github.com/emacs-dashboard/emacs-dashboard/blob/master/dashboard-widgets.el
;; https://github.com/hlissner/doom-emacs/blob/eddaae40e84b5eb1f0136aaba23d918f71b6a986/core/core.el#L479
(defcustom lem-splash-init-info
  (lambda ()
    (let ((package-count 0) (time (emacs-init-time)))
      (setq package-count (- (length load-path) (length (get 'load-path 'initial-value))))
      (if (zerop package-count)
          (format "Emacs started in %s" time)
        (format "%d packages loaded in %s" package-count time))))
  "Init info with packages loaded and init time."
  :type '(function string)
  :group 'lambda-splash)

;; Banner
(defcustom lem-splash-banner
  "
              ``                                         
        :oyhdmmmmdhs+-                                   
        sMMMMMMMMMMMMNdo.                                
        .NMMMMMMMMMMMMMMm+                               
         oMNNNMMMMMMMMMMMMh.                             
         n:sdMMMMMMMMMMMMMMd.                            
                 `+NMMMMMMMMm.                           
                   -mMMMMMMMMm.                          
                    -NMMMMMMMMd`                         
                     :NMMMMMMMMy                         
                      oMMMMMMMMM+                        
                       hMMMMMMMMN-                       
                       .mMMMMMMMMm`                      
                        :MMMMMMMMMy                      
                         sMMMMMMMMM+                     
                         `NMMMMMMMMN-                    
                        `yMMMMMMMMMMd`                   
                        yMMMMMMMMMMMMy                   
                       oMMMMMMMMMMMMMM/                  
                      oMMMMMMMMMMMMMMMN.                 
                     /MMMMMMMMdMMMMMMMMd`                
                    /MMMMMMMMd`dMMMMMMMMs                
                   :NMMMMMMMN. :MMMMMMMMM/               
                  -NMMMMMMMM/   yMMMMMMMMN.              
                 .NMMMMMMMM+    .NMMMMMMMMd              
                .mMMMMMMMMs      /MMMMMMMMMs             
               `dMMMMMMMMh`       yMMMMMMMMM:            
              `dMMMMMMMMd`        `mMMMMMMMMN.           
             `hMMMMMMMMm.          :MMMMMMMMMh           
             yMMMMMMMMN-            oMMMMMMMMMo          
            sMMMMMMMMN:             `dMMMMMMMMN:         
           oMMMMMMMMM/               -NMMMMMMMMm`        
          +MMMMMMMMMo                 +MMMMMMMMMh        
         /NMMMMMMMMs                   yMMMMMMMMM+       
        :NMMMMMMMMy`                   .mMMMMMMMMN-      
       -NMMMMMMMMd`                     :MMMMMMMMMm`     
      -mMMMMMMMMm.                       sMMMMMMMMMy     
     .mMMMMMMMMm-                        `dMMMMMMMMM+    
    .dMMMMMMMMN:                          -NMMMMMMMMN-   
   `dMMMMMMMMN/                            +MMMMMMMMMd`  
  `hMMMMMMMMM+                              hMMMMMMMMMy  
  yMMMMMMMMMs                               .NMMMMMMMMM/ 
 `//////////                                 -/////////: 
"
  "Banner picture for splash-screen"
  :type '(string)
  :group 'lambda-splash)

(defvar isep "   "
  "Separator for icon and title.")
(defvar ksep "   "
  "Separator for title and key.")

;;; Define Splash

(defun lem-splash--erase ()
  ;; check if splash exists and switch if so
  (when-let ((inhibit-read-only t)) 
    (get-buffer "*splash*")
    (switch-to-buffer "*splash*")
    (erase-buffer)))

(defun lem-splash-screen ()
  "A custom splash screen for Emacs"
  (interactive)
  ;; Hide modeline before window-body-height is computed
  (let* ((splash-buffer (get-buffer-create "*splash*")))
    (with-current-buffer splash-buffer
      (setq header-line-format nil)
      (setq mode-line-format nil)
      (erase-buffer))

    (let* ((buffer-read-only t)
           (midp  (/ (point-max) 2))
           (padding-top 3)
           (height (/ (window-height) 2))
	       (width (/ (window-width) 2))
           ;; scale icons
           (all-the-icons-scale-factor 1.10)
           (all-the-icons-default-adjust -0.02)
           ;; Original ascii image from here:
           ;; https://github.com/Triagle/emax/blob/master/boot.txt
           (image (lem-get-string-from-file (concat lem-library-dir "lambda-splash.txt"))))

      (with-current-buffer splash-buffer
        (erase-buffer)
        (when-let (windows (get-buffer-window-list "*splash*" nil t))
          (dolist (win windows)
            (set-window-start win 0)
            (set-window-fringes win 0 0)
            (set-window-margins
             win (max 0 (/ (- (window-total-width win) width))))))

        ;; Buffer local settings
        (if (one-window-p)
            (setq mode-line-format nil))
        (setq-local cursor-type nil)
        (setq vertical-scroll-bar nil)
        (setq horizontal-scroll-bar nil)
        (setq fill-column width)
        (face-remap-add-relative 'link :underline nil)
        (unless (and (display-graphic-p) sys-mac) (menu-bar-mode 0))

        ;; Set margin padding locally
        (setq-local left-margin-width (/ width 6)
                    right-margin-width (/ width 6))
        (set-window-buffer nil (current-buffer))

        ;; Add padding at top
        (insert-char ?\n padding-top)

        (when (and (> (window-width) 59)
                   (> (window-height) 44))
          (insert (propertize lem-splash-banner 'face 'lem-splash-image-face)))

        ;; Position point
        (goto-char (point-min))
        ;; subtract 10 to account for padding
        (forward-line (- (/ (window-height) 2) 10))
        (end-of-line)

        ;; Insert header text
        (insert-rectangle `(,(propertize "           Welcome to 𝛌-Emacs"  'face 'lem-splash-title-face)
                            ,(concat (propertize "         GNU Emacs version" 'face 'lem-splash-header-face)
                                     " "
                                     (propertize (format "%d.%d" emacs-major-version emacs-minor-version) 'face 'lem-splash-header-face))

                            ,(let ((init-info (funcall lem-splash-init-info)))
                               (concat "  " (propertize init-info 'face 'lem-splash-header-face)))))

        ;; Insert header buttons
        (next-line 2)
        (insert-char ?\s 11)
        (insert-text-button (concat (all-the-icons-faicon "calendar") isep "Agenda" ksep "(a)")
                            'action (lambda (_) (lem-open-agenda-in-workspace))
                            'help-echo "Open Agenda"
                            'face 'lem-splash-menu-face
                            'follow-link t)
        (next-line)
        (insert-char ?\s 11)
        (insert-text-button (concat (all-the-icons-faicon "code") isep "Config" ksep "(c)")
                            'action (lambda (_) (lem-open-emacsd-in-workspace))
                            'help-echo "Visit config directory"
                            'face 'lem-splash-menu-face
                            'follow-link t)

        (next-line)
        (insert-char ?\s 11)
        (insert-text-button (concat (all-the-icons-faicon "envelope-o") isep "Mail" ksep "(m)")
                            'action (lambda (_)  (lem-open-email-in-workspace))
                            'help-echo "Open Email in Mu4e"
                            'face 'lem-splash-menu-face
                            'follow-link t)

        (next-line)
        (insert-char ?\s 11)
        (insert-text-button (concat (all-the-icons-faicon "book") isep "Notes" ksep "(n)")
                            'action (lambda (_)  (lem-open-notes-in-workspace))
                            'help-echo "Open notes directory"
                            'face 'lem-splash-menu-face
                            'follow-link t)

        (next-line)
        (insert-char ?\s 11)
        (insert-text-button (concat (all-the-icons-octicon "git-branch") isep "Projects" ksep "(p)")
                            'action (lambda (_)  (tabspaces-open-existing-project-and-workspace))
                            'help-echo "Open project & workspace"
                            'face 'lem-splash-menu-face
                            'follow-link t)


        ;; see https://blog.lambda.cx/posts/emacs-align-columns/
        (align-regexp (point-min) (point-max) "\\(\\s(*\\)\\S(+" -1 1 t)

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
      (switch-to-buffer "*splash*")))
  (lem-splash-mode))

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

;;;;; Define Minor Mode
;; Custom splash screen
(defvar lem-splash-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") 'lem-open-agenda-in-workspace)
    (define-key map (kbd "c") 'lem-open-emacsd-in-workspace)
    (define-key map (kbd "m") 'lem-open-email-in-workspace)
    (define-key map (kbd "n") 'lem-open-notes-in-workspace)
    (define-key map (kbd "p") 'lem-open-existing-project-and-workspace)
    (define-key map (kbd "q") 'lem-splash-screen-bury)
    (define-key map (kbd "esc") 'lem-splash-screen-bury)
    (define-key map (kbd "k") 'lem-splash-screen-kill)
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
  (goto-char (point-min))
  (forward-button 1)
  (end-of-line))

;; Install hook after frame parameters have been applied and only if
;; no option on the command line
(if (and (not (member "--no-splash" command-line-args))
         (not (member "--file"      command-line-args))
         (not (member "--insert"    command-line-args))
         (not (member "--find-file" command-line-args)))
    (add-hook 'window-setup-hook #'lem-splash--setup-splash-hooks))

(defun lem-splash--setup-splash-hooks ()
  "Initialize splash and setup hooks."
  (progn
    (lem-splash-screen)
    (add-hook 'window-state-change-hook #'lem-splash-refresh)
    (add-hook 'window-configuration-change-hook  #'lem-splash-refresh)
    (add-hook 'lem-switch-buffer-hook #'lem-splash-refresh)))

(defun lem-splash-refresh ()
  "Refresh & recenter splash after window switch."
  (interactive)
  (when (string= (buffer-name) "*splash*")
    (progn
      (ignore-errors
        (lem-splash-screen-kill)
        (lem-splash-screen)))))

(provide 'lem-setup-splash)

;;; lem-setup-splash.el ends here

;; Local Variables:
;; eval: (ws-butler-mode -1)
;; End:
