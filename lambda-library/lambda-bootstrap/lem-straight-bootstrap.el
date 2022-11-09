;;;; lem-package/straight.el --- Configuration to use `straigt.el`.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT

;; Author: System Crafters Community

;;; Commentary:

;; This code loads the bootstrap code for `straight.el', according as
;; described in <https://github.com/radian-software/straight.el>.

;; Bootstrap `straight.el' configuration, as described in
;; <https://github.com/radian-software/straight.el>.  Code provided
;; herein is intended for internal use, the user is not expected to
;; use the interface provided here to manage their packages.

;;; Code:

(defvar bootstrap-version)

;; We use straight not package.el for all package loading.
;; So we don't need package.el loaded at startup (or at all).
(customize-set-variable 'package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Do not allow loading from the package cache (same reason).
(customize-set-variable 'package-quickstart nil)

;;;;; Straight
;;;;;; Straight settings
;; Use straight.el to install all packages
;; https://github.com/raxod502/straight.el
;; Don't check packages on startup
(customize-set-variable 'straight-check-for-modifications '(check-on-save find-when-checking))
;; Set branch
(customize-set-variable 'straight-repository-branch "develop")
;; Set dir
(customize-set-variable 'straight-base-dir lem-var-dir)
;; Use use-package
(customize-set-variable 'straight-use-package-by-default t)
;; Check updates manually
(customize-set-variable 'straight-vc-git-auto-fast-forward nil)
;; Avoid problems with straight building with native-comp
;; See https://github.com/raxod502/straight.el/issues/757
(customize-set-variable 'native-comp-deferred-compilation-deny-list nil)
;; Tell straight.el about the profiles we are going to be using.
(customize-set-variable 'straight-profiles
                        '((nil . "default.el")
                          ;; Packages which are pinned to a specific commit.
                          (pinned . "pinned.el")))

;;;;;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" straight-base-dir))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defmacro lem-package-install-package (package)
  "Lambda-Emacs interface to install packages.

Only install the package if it is not already installed using
straight.el."
  `(straight-use-package ,package))

(defmacro lem-package-installed-p (package)
  "Lambda-Emacs interface to check if a package is installed."
  `(straight--installed-p ,package))

;;;;;; Straight-X
;; Use experimental straight commands
(require 'straight-x)
;; https://github.com/raxod502/straight.el#how-do-i-pin-package-versions-or-use-only-tagged-releases
(autoload #'straight-x-freeze-versions "straight-x")
;; package updates
;; use this workflow?
;; https://github.com/raxod502/straight.el/issues/354#issuecomment-465305063
(autoload #'straight-x-pull-all "straight-x")
;; async fetch
(autoload #'straight-x-fetch-all "straight-x")



(provide 'lem-package/straight)
;;; lem-package/straight.el ends here
