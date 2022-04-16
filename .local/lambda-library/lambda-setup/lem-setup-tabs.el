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

;; Tabs -- in some ways tabs are not very Emacsy, but tab-bar is great for
;; window & buffer management. The setup for tabs here is focused on tabs for
;; window configurations related to a "workspace" -- i.e. a project or other set
;; of windows. A workspace or "tabspace" (see the package below) is just a tab
;; with an isolated set of buffers (see also the consult function). 𝛌-Emacs (and
;; 𝛌-Themes) sets things up so that these tabs/workspaces are nicely displayed.

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
  (tab-bar-show 1)
  ;; Unless another file/buffer is designated, start from scratch buffer
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-select-tab-modifiers '(super))
  (tab-bar-close-tab-select 'recent)
  (tab-bar-new-tab-to 'rightmost)
  (tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (tab-bar-separator " ")
  (tab-bar-new-button nil)
  (tab-bar-close-button nil)
  (tab-bar-format '(tab-bar-format-history
                    tab-bar-format-tabs
                    tab-bar-separator
                    tab-bar-format-add-tab))
  :config
  ;; https://protesilaos.com/codelog/2020-08-03-emacs-custom-functions-galore/
  (defun lem/tab-bar-select-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Otherwise use completion to select the tab."
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

;;;; Tab Bookmark
;; Bookmark window configurations in a tab
;; NOTE: would be good to get this working with tabspaces
(use-package tab-bookmark
  :straight (:type git :host github :repo "minad/tab-bookmark")
  :commands (tab-bookmark tab-bookmark-open)
  :bind (:map project-prefix-map
         ("m" . tab-bookmark)))

;;;; Workspaces

(use-package tabspaces
  ;; :straight (:type git :host github :repo "mclear-tools/emacs-workspaces")
  :straight (:local-repo "/Users/roambot/bin/lisp-projects/tabspaces")
  :hook (after-init . tabspaces-mode)
  ;; Add some functions to the project map
  :bind (:map project-prefix-map
         ("p" . tabspaces-open-existing-project-and-workspace)
         ("n" . tabspaces-create-new-project-and-workspace))
  :custom
  (tabspaces-use-filtered-buffers-as-default t))

;;;; Per Workspace Buffers with Consult
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
                      (tabspaces--tab-bar-buffer-name-filter ((lambda () (consult--buffer-query :sort 'visibility
                                                                                           :as #'buffer-name))))))

    "Set workspace buffer list for consult-buffer.")
  (push consult--source-workspace consult-buffer-sources))

;;; Provide
(provide 'lem-setup-tabs)
;;; lem-setup-tabs.el ends here
