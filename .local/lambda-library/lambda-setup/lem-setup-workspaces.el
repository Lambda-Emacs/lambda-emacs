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
(provide 'lem-setup-workspaces)
