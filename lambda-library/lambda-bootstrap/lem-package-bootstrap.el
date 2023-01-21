;;;; lem-package-bootstrap.el --- Configuration to use `package.el'.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
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

;; Bootstrap `package.el' configuration.  This is the default package
;; manager for Lambda-Emacs.  Code provided herein is intended for
;; internal use, the user is not expected to use the interface
;; provided here to manage their packages.

;;; Code:
;; Package configuration
(require 'package)

;; See https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/ for discussion
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned gets priority 0)
(customize-set-variable 'package-archive-priorities
                        '( ;; Prefer GNU packages
                          ("elpa-devel" . 99)
                          ("melpa" . 90)))

;; Set location of package directory
(customize-set-variable 'package-user-dir
                        (expand-file-name "elpa/" (concat lem-var-dir)))
(customize-set-variable 'package-gnupghome-dir (concat package-user-dir "gnupg"))

;; Make sure the elpa/ folder exists after setting it above.
(unless (file-exists-p package-user-dir)
  (mkdir package-user-dir t))
(setq package-quickstart-file (expand-file-name "package-quickstart.el" (concat lem-cache-dir)))

;; TODO: add auto refresh option
;; See https://andreyorst.gitlab.io/posts/2022-07-15-refresh-package-contents-automatically/

;; Ensure packages?
(defcustom lem-package-ensure-packages t
  "Whether to ensure packages with use-package, or install manually using the list in `package-selected-packages'."
  :group 'lambda-emacs
  :type 'boolean)

(provide 'lem-package-bootstrap)
;;; lem-package-bootstrap.el ends here
