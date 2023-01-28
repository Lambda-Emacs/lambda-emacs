;;; init.el  -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;; Author: Colin McLear
;; Maintainer: Colin McLear
;; Version: 0.3

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
;; This is the base init file to load the entire emacs config. For ease of
;; navigation use outline-mode to cycle headlines open and closed (<Tab> and
;; <S-Tab>) to navigate through sections, and "imenu" to locate individual
;; use-package definitions.

;;; Code:

;; Define customization group for Crafted Emacs.
(defgroup lambda-emacs '()
  "An Emacs distribution with sane defaults, pre-configured packages, and useful functions, aimed at writing and academic work in the humanities."
  :tag "Lambda-Emacs"
  :link '(url-link "https://github.com/Lambda-Emacs/lambda-emacs")
  :group 'emacs)

;;;; Startup
;;;;; Load Path
;; Add all configuration files to load-path
(eval-and-compile
  (progn
    (push lem-setup-dir load-path)
    (push lem-user-dir load-path)))

;;;;; Exec Path
;; Use exec-path-from-shell to fix path issues when starting from GUI
(when (memq window-system '(mac ns x))
  (add-hook 'emacs-startup-hook
            #'exec-path-from-shell-initialize))

;;;;; Package Settings
;; Check if package system for Lambda-Emacs is using package.el. Initialize
;; system if not already initialized.
(when (and (eq lem-package-system 'package)
           (not package-enable-at-startup))
  (package-initialize))

;;;;; Use-Package
;; Install use-package to manage package setup. If using Emacs 29 or later
;; use-package is built-in.
(cond ((version= (format "%s" emacs-major-version) "29")
       (require 'use-package))
      ((eq lem-package-system 'package)
       (unless (package-installed-p 'use-package)
         (package-refresh-contents)
         (package-install 'use-package)))
      ((eq lem-package-system 'straight)
       (straight-use-package 'use-package)))

;; Use-Package Settings
(use-package use-package
  :custom
  ;; Don't automatically defer
  (use-package-always-defer nil)
  ;; Report loading details
  (use-package-verbose t)
  ;; This is really helpful for profiling
  (use-package-minimum-reported-time 0)
  ;; Expand normally
  (use-package-expand-minimally nil)
  ;; Unless otherwise set, manually handle package install -- see early-config.el
  (use-package-always-ensure (if lem-package-ensure-packages t nil))
  ;; Navigate use-package declarations w/imenu
  (use-package-enable-imenu-support t))

;;;;; El-Patch
;; Package for helping advise/modify features of other packages
(use-package el-patch
  :custom
  (el-patch-enable-use-package-integration t))

;;;;; Security
;; Properly verify outgoing ssl connections.
;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(use-package gnutls
  :defer 1
  :custom
  (gnutls-verify-error t)
  (gnutls-min-prime-bits 3072))

;;;;; Auto-compile
;; Automatically byte-recompile changed elisp libraries
(use-package auto-compile
  :defer 1
  :custom
  (auto-compile-display-buffer nil)
  (auto-compile-mode-line-counter nil)
  (auto-compile-use-mode-line nil)
  (auto-compile-update-autoloads t)
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

;;;;; Command Line Switches

;; Conditionally load parts of config depending on command line switches.
;; This allows startup with a clean emacs that still recognizes straight.
;; Helpful for testing packages.
;; Use (straight-use-package) command to selectively load packages.
;; See https://emacs.stackexchange.com/a/34909/11934
;; For function switch see https://stackoverflow.com/a/4065412/6277148

;; NOTE: The variable here doesn't really do anything. It is just useful to keep
;; for a record of switches.
(defvar lem-config-switches
  '("basic"
    "clean"
    "core"
    "test")
  "Custom switches for conditional loading from command line.
  `clean' loads only the `init.el' file w/no personal config; `core'
  loads the set of modules set in `lem-core-modules'; `test' loads
  only a `lem-setup-test.el' file for easy testing.")

(defun lem--emacs-switches (switch)
  "Depending on command line argument SWITCH, load Emacs with minimal settings & no modules; useful for testing."
  (let ((found-switch (member switch command-line-args)))
    (setq command-line-args (delete switch command-line-args))
    found-switch))

;;;;; Emacs Build Version
;; When built with https://github.com/mclear-tools/build-emacs-macos, Emacs has
;; git-version patch to include git sha1 in emacs-version string.
(setq site-lisp "/Applications/Emacs.app/Contents/Resources/site-lisp/")
(when (file-exists-p (concat site-lisp "emacs-git-version.el"))
  (require 'emacs-git-version))

(defun lem-emacs-version ()
  "A convenience function to print the emacs-version in the echo-area/*messages* buffer and put
emacs-version string on the kill ring."
  (interactive)
  (let ((emacs (emacs-version)))
    (message (emacs-version))
    (kill-new emacs)))

;;;;; Outline Navigation
;; Navigate elisp files easily. Outline is a built-in library and we can easily
;; configure it to treat elisp comments as headings.
(use-package outline
  :hook (prog-mode . outline-minor-mode)
  :bind (:map outline-minor-mode-map
         ("<tab>"   . outline-cycle)
         ("S-<tab>" . outline-cycle-buffer)
         ("M-j"     . outline-move-subtree-down)
         ("M-k"     . outline-move-subtree-up)
         ("M-h"     . outline-promote)
         ("M-l"     . outline-demote))
  :config
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              ;; prevent `outline-level' from being overwritten by `lispy'
              (setq-local outline-level #'outline-level)
              ;; setup heading regexp specific to `emacs-lisp-mode'
              (setq-local outline-regexp ";;;\\(;* \\)")
              ;; heading alist allows for subtree-like folding
              (setq-local outline-heading-alist
                          '((";;; " . 1)
                            (";;;; " . 2)
                            (";;;;; " . 3)
                            (";;;;;; " . 4)
                            (";;;;;;; " . 5))))))

;;;;; Load Configuration Modules
;; Lambda-Emacs loads a series of lisp-libraries or 'modules'. Which modules are
;; loaded is left to the user to set in `config.el', though if there is no
;; `config.el' file a default set of modules will be loaded.

(defun lem--default-modules ()
  "Load a default configuration for 𝛌-Emacs."
  (message "
;; ======================================================
;; *Loading default setup of 𝛌-Emacs Modules*
;; ======================================================")
  (measure-time
   (cl-dolist (mod (list
                    ;; Core modules
                    'lem-setup-libraries
                    'lem-setup-settings
                    'lem-setup-functions
                    'lem-setup-server
                    'lem-setup-scratch

                    ;; UI modules
                    'lem-setup-frames
                    'lem-setup-windows
                    'lem-setup-buffers
                    'lem-setup-fonts
                    'lem-setup-faces
                    'lem-setup-colors
                    'lem-setup-completion
                    'lem-setup-keybindings
                    'lem-setup-help
                    'lem-setup-modeline
                    'lem-setup-theme
                    'lem-setup-splash

                    ;; Navigation & Search modules
                    'lem-setup-navigation
                    'lem-setup-dired
                    'lem-setup-search

                    ;; Project & Tab/Workspace modules
                    'lem-setup-vc
                    'lem-setup-projects
                    'lem-setup-tabs

                    ;; Org modules
                    'lem-setup-org-base
                    'lem-setup-org-settings

                    ;; Writing modules
                    'lem-setup-writing
                    'lem-setup-notes
                    'lem-setup-citation

                    ;; Shell & Terminal
                    'lem-setup-shell
                    'lem-setup-eshell

                    ;; Programming modules
                    'lem-setup-programming

                    ;; Productivity
                    'lem-setup-pdf))
     (require mod))))

(defun lem--minimal-modules ()
  "Load 𝛌-Emacs with a minimal set of modules."
  (message "
;; ======================================================
;; *Loading 𝛌-Emacs, with minimal modules*
;; ======================================================")
  (measure-time
   (cl-dolist (mod (list
                    ;; Core modules
                    'lem-setup-libraries
                    'lem-setup-settings
                    'lem-setup-functions
                    'lem-setup-server
                    'lem-setup-scratch

                    ;; UI modules
                    'lem-setup-frames
                    'lem-setup-windows
                    'lem-setup-buffers
                    'lem-setup-completion
                    'lem-setup-keybindings
                    'lem-setup-help
                    'lem-setup-modeline
                    'lem-setup-splash

                    ;; Navigation & Search modules
                    'lem-setup-navigation
                    'lem-setup-dired
                    'lem-setup-search))
     (require mod))))

;; Conditionally load configuration files based on command-line switches,
;; presence of user-config file, or the default set of modules.
(cond
 ;; Load a subset of modules only, ignore other configuration.
 ((lem--emacs-switches "-minimal")
  (message "*Loading a subset of 𝛌-Emacs modules *only*, ignoring personal user configuration.*")
  (lem--minimal-modules))

 ;; Load test module only. This is useful for testing a specific package
 ;; against vanilla/default emacs settings.
 ((lem--emacs-switches "-test")
  (message "*Loading test module only*")
  (require 'lem-setup-test))

 ;; Load no libraries other than the built-in icomplete, for better completion.
 ((lem--emacs-switches "-vanilla")
  (message "*Do not load 𝛌-Emacs setup files. Loading a clean vanilla setup plus icomplete*")
  (require 'lem-setup-icomplete))

 ;; Load user's personal config file (if it exists) and hasn't been bypassed
 ;; by a command-line switch to load the default libraries.
 ((and (not (lem--emacs-switches "-default"))
       (file-exists-p lem-config-file))
  (message "*Loading 𝛌-Emacs & user config*")
  (load lem-config-file 'noerror))

 ;; Load default config
 ((lem--emacs-switches "-default")
  (message "*Loading 𝛌-Emacs default modules")
  (lem--default-modules)
  ;; MacOS settings - defer load until after init.
  (when sys-mac
    (message "*Load MacOS settings...*")
    (measure-time
     (run-with-idle-timer 1 nil
                          (function require)
                          'lem-setup-macos nil t))))

 ;; Ask if user would like to create a config file.
 ((when (not (file-exists-p lem-config-file))
    (yes-or-no-p "Would you like to create a user configuration file? ")
    (progn
      (with-temp-file lem-config-file
        (insert-file lem-default-config-file))
      (load-file lem-config-file))))
 ;; Load default modules
 (t
  (message "*Loading 𝛌-Emacs default configuration files.*")
  (lem--default-modules)
  ;; MacOS settings - defer load until after init.
  (when sys-mac
    (message "*Load MacOS settings...*")
    (measure-time
     (run-with-idle-timer 1 nil
                          (function require)
                          'lem-setup-macos nil t)))))


;;;; After Startup
;; reset file-name-handler-alist
(add-hook 'emacs-startup-hook (lambda ()
                                (setq file-name-handler-alist lem-file-name-handler-alist)
                                ;; reset garbage collection
                                (setq gc-cons-threshold 800000)
                                ;; Startup time
                                (message (format ";; ======================================================\n;; Emacs ready in %.2f seconds with %d garbage collections.\n;; ======================================================"
                                                 (float-time
                                                  (time-subtract after-init-time before-init-time)) gcs-done)
                                         (put 'narrow-to-page 'disabled nil))))
