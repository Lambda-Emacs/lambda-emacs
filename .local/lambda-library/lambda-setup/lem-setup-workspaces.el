;;; lem-setup-workspaces.el --- summary -*- lexical-binding: t -*-

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
;; Workspaces leveraging tab-bar and project.el. A "workspace" is just a tab
;; with an isolated set of buffers (see consult function). 𝛌-Emacs sets things
;; up so that these workspaces are nicely displayed out of the way in the echo
;; area rather than as visible tabs in the header tab-line at the top of the
;; frame.


;;; Code:

;;;; Workspace Functions

;;;;; Startup Workspaces
(defun lem--workspace-setup ()
  "Set up workspace at startup."
  ;; Add *Messages* and *splash* to Tab \`Home\'
  (progn
    (tab-bar-rename-tab "Home")
    (when (get-buffer "*Messages*")
      (set-frame-parameter nil
                           'buffer-list
                           (cons (get-buffer "*Messages*")
                                 (frame-parameter nil 'buffer-list))))
    (when (get-buffer "*splash*")
      (set-frame-parameter nil
                           'buffer-list
                           (cons (get-buffer "*splash*")
                                 (frame-parameter nil 'buffer-list))))))

(add-hook 'after-init-hook #'lem--workspace-setup)
(defun lem-go-home ()
  (interactive)
  (tab-bar-switch-to-tab "Home"))

;;;;; Open Project in New Workspace
(defun lem-open-existing-project-and-workspace ()
  "Open a project as its own workspace"
  (interactive)
  (progn
    (tab-bar-new-tab)
    (call-interactively 'project-switch-project-open-file)
    (tab-bar-rename-tab (tabspaces--name-tab-by-project-or-default))
    (project-magit-dir)))

;;;;; Open Agenda as Workspace
(defun lem-open-agenda-in-workspace ()
  "Open agenda in its own workspace"
  (interactive)
  (if (member "Agenda" (tabspaces--list-tabspaces))
      (progn
        (tab-bar-switch-to-tab "Agenda")
        (switch-to-buffer "*Org Agenda*")
        (org-agenda-redo)
        (delete-other-windows))
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Agenda")
      (require 'org)
      (org-agenda-list))))

;;;;; Open emacs.d in Workspace
(defun lem-open-emacsd-in-workspace ()
  "Open emacs.d in its own workspace"
  (interactive)
  (require 'tabspaces)
  (if (member "emacs.d" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "emacs.d")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "emacs.d")
      (find-file lem-config-file)
      (split-window-right)
      (other-window 1)
      (project-magit-dir))))

;;;;; Open Notes in Workspace

(defun lem-open-notes-in-workspace ()
  "Open notes dir in its own workspace"
  (interactive)
  (require 'tabspaces)
  (if (member "Notes" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "Notes")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Notes")
      (lem-notebook))))

;;;;; Terminal Workspace
(defun lem-vterm-workspace ()
  "Open vterm in home dir in its own workspace"
  (interactive)
  (let ((default-directory "~/"))
    (require 'multi-vterm)
    (multi-vterm-next)))

(defun lem-open-new-terminal-and-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (require 'tabspaces)
  (if (member "Terminal" (tabspaces--list-tabspaces))
      (tab-bar-switch-to-tab "Terminal")
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Terminal")
      (lem-vterm-workspace)
      (delete-other-windows))))

;;;;; Open Mu4e Email in Workspace
(defun lem-open-email-in-workspace ()
  "Open mu4e email in its own workspace"
  (interactive)
  (require 'tabspaces)
  (if (member "Email" (tabspaces--list-tabspaces))
      (progn
        (tab-bar-switch-to-tab "Email")
        (cond ((get-buffer "*mu4e-headers*")
               (switch-to-buffer "*mu4e-headers*"))
              ((get-buffer " *mu4e-main*")
               (progn
                 (switch-to-buffer " *mu4e-main*")
                 (delete-other-windows)))
              (t (mu4e))))
    (progn
      (tab-bar-new-tab)
      (tab-bar-rename-tab "Email")
      (require 'org) ; need this for loading?
      (find-file (concat org-directory "mail.org"))
      (mu4e)
      (switch-to-buffer " *mu4e-main*"))))

;;;;; Open New Buffer & Workspace
;; This function is a bit weird; It creates a new buffer in a new workspace with a
;; dummy git project to give the isolation of buffers typical with a git project
;; I'm sure there is a more elegant way to do this but I don't know how :)
(defun lem-open-new-buffer-and-workspace ()
  "Open an empty buffer in its own workspace"
  (interactive)
  (tab-bar-new-tab)
  (tab-bar-rename-tab-tab "New project")
  (let ((lem-project-temp-dir "/tmp/temp-projects/"))
    (progn
      (when (not (file-exists-p lem-project-temp-dir))
        (make-directory lem-project-temp-dir t))
      (when (not (file-exists-p (concat lem-project-temp-dir ".git/")))
        (magit-init lem-project-temp-dir))
      (when (not (file-exists-p (concat lem-project-temp-dir "temp")))
        (with-temp-buffer (write-file (concat lem-project-temp-dir "temp")))))
    (setq default-directory lem-project-temp-dir)
    (find-file (concat lem-project-temp-dir "temp"))))

;;;; Workspace Keybindings

(bind-keys :map lem+leader-map
  ("1" . lem-go-home)
  ("2" . lem-open-emacsd-in-workspace)
  ("3" . lem-open-agenda-in-workspace)
  ("4" . lem-open-notes-in-workspace)
  ("6" . lem-open-new-terminal-and-workspace)
  ("5" . lem-open-email-in-workspace))
(bind-key "N" #'lem-open-new-buffer-and-workspace 'project-prefix-map)

;;; Provide
(provide 'lem-setup-workspaces)
