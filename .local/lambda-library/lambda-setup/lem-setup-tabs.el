;;; lem-setup-tabs.el --- summary -*- lexical-binding: t -*-

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

;; Tab-bar -- use tab-bar for window & buffer management -*- lexical-binding: t; -*-

;;; Code:

;;;; Tab Bar
;; Use tab-bar for window grouping and configuration within a project (replaces eyebrowse)
(use-package tab-bar
  :straight (:type built-in)
  :after (project)
  :commands (tab-bar-new-tab
             tab-bar-switch-to-tab
             tab-bar-switch-to-next-tab
             tab-bar-switch-to-prev-tab)
  :custom
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-close-tab-select 'recent)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-show nil)
  :config
  ;; https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/
  (defun lem/tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t))))))
  (setq tab-bar-mode t))

;;;;; Tab Bar Echo
;; Use echo area for tab name display
(use-package tab-bar-echo-area
  :straight (:type git :host github :repo "fritzgrabo/tab-bar-echo-area")
  :bind (:map tab-prefix-map
         ("c" . tab-bar-echo-area-display-tab-name)
         ("a" . tab-bar-echo-area-display-tab-names))
  :config
  (setq tab-bar-echo-area-trigger-display-functions nil)
  (tab-bar-echo-area-mode 1))

;; display all tabs when idle
;; (run-with-idle-timer 5 t (lambda () (message nil) (tab-bar-echo-area-display-tab-names)))

;;;;; Echo-Bar
;; Display info in the echo area -- using just for tabs/workspaces right now
(use-package echo-bar
  :straight (echo-bar :type git :host github :repo "qaiviq/echo-bar.el")
  :config
  (setq echo-bar-function #'cpm--echo-bar
        echo-bar-minibuffer t)
  (echo-bar-enable))

(defun cpm--echo-bar ()
  "Function for echo bar"
  ;; Show tabs
  (cpm-echo-bar/tabs))

(defun cpm-echo-bar/tabs ()
  "Display tab names in the echo area. Depends on `tab-bar-echo-area'"
  (interactive)
  (require 'tab-bar-echo-area)
  (let* ((tab-bar-format (or tab-bar-echo-area-format (and (boundp 'tab-bar-format) tab-bar-format)))
         (keymap (funcall tab-bar-echo-area-make-keymap-function))
         (keymap-elements (seq-filter #'tab-bar-echo-area--keymap-element-type (cdr keymap))))
    (if-let ((tab-names (tab-bar-echo-area--processed-tab-names keymap-elements))
             (format-string (cond ((functionp tab-bar-echo-area-display-tab-names-format-string)
                                   (funcall tab-bar-echo-area-display-tab-names-format-string keymap-elements))
                                  ((stringp tab-bar-echo-area-display-tab-names-format-string)
                                   tab-bar-echo-area-display-tab-names-format-string)
                                  (t "%s"))))
        (format "%s" (string-join tab-names)))))

(defun echo-bar--list-workspaces ()
  "Return a list of `tab-bar' tabs/workspaces."
  (require 'tab-bar)
  (let ((tab-names (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs))))
    (mapconcat 'identity tab-names " ")))

;;;;; Tab Bookmark
;; Bookmark window configurations in a tab
;; NOTE: would be good to get this working with emacs-workspaces
(use-package tab-bookmark
  :straight (:type git :host github :repo "minad/tab-bookmark")
  :commands (tab-bookmark tab-bookmark-open)
  :bind (:map project-prefix-map
         ("m" . tab-bookmark)))


;;;; Workspaces
;; Workspaces leveraging tab-bar and project.el
(use-package emacs-workspaces
  :straight (:type git :host github :repo "mclear-tools/emacs-workspaces")
  ;; Add some functions to the project map
  :bind (:map project-prefix-map
         ("p" . emacs-workspaces/open-existing-project-and-workspace)
         ("n" . emacs-workspaces/create-new-project-and-workspace))
  :commands (emacs-workspaces/create-workspace
             emacs-workspaces/create-new-project-and-workspace
             emacs-workspaces/open-existing-project-and-workspace)
  :config
  (setq emacs-workspaces-use-consult-project t))

;;;;; Per Workspace Buffers with Consult
;; Filter Buffers for Consult-Buffer

(with-eval-after-load 'consult
  ;; hide full buffer list (still available with "b")
  (consult-customize consult--source-buffer :hidden t :default nil)
  ;; set consult-workspace buffer list
  (defvar consult--source-workspace
    (list :name     "Workspace Buffers"
          :narrow   ?w
          :category 'buffer
          :state    #'consult--buffer-state
          :default  t
          :items    (lambda ()
                      (emacs-workspaces--tab-bar-buffer-name-filter ((lambda () (consult--buffer-query :sort 'visibility
                                                                                                  :as #'buffer-name))))))

    "Set workspace buffer list for consult-buffer.")
  (push consult--source-workspace consult-buffer-sources))

;;;; Set Workspace  Variables

(setq lem-open-agenda-in-workspace #'lem/open-agenda-in-workspace)

(provide 'lem-setup-tabs)
;;; lem-setup-tabs.el ends here
