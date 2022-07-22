;;; init.el  -*- lexical-binding: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;;; Commentary:
;; This is the base init file to load the entire emacs config. For ease of
;; navigation use outline-mode to cycle headlines open and closed (<Tab> and
;; <S-Tab>) to navigate through sections, and "imenu" to locate individual
;; use-package definitions.

;;; Code:

;;;; Startup
;;;;; System Variables
;; Check the system used
(defconst sys-linux   (eq system-type 'gnu/linux))
(defconst sys-mac     (eq system-type 'darwin))
(defconst sys-bsd     (or sys-mac (eq system-type 'berkeley-unix)))
(defconst sys-win     (memq system-type '(cygwin windows-nt ms-dos)))

;;;;; Directory Variables
;;  We're going to define a number of directories that are used throughout this
;;  configuration to store different types of files. This is a bit like the
;;  `no-littering' package, and allows us to keep `user-emacs-directory' tidy.

(defconst lem-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

(defconst lem-local-dir (concat lem-emacs-dir ".local/")
  "The root directory for local Emacs files.
This contains all elisp libraries as well as non-essential and/or
ephemeral files. There are two main directories --
lem-library-dir, which contains all libraries and packages, and
lem-temp-dir, which contains non-essential files and emphemera.")

(defconst lem-library-dir (concat lem-local-dir "lambda-library/")
  "The directory for 𝛌-Emacs Lisp libraries.
This will house all setup libraries and external libraries or packages.")

(defconst lem-user-dir (concat lem-library-dir "lambda-user/")
  "Storage for personal elisp, scripts, and any other private files.")

(defconst lem-setup-dir (concat lem-library-dir "lambda-setup/")
  "The storage location of the setup-init files.")

(defconst lem-temp-dir (concat lem-local-dir "temp/")
  "The directory for non-essential file storage.
Used by `lem-etc-dir' and `lem-cache-dir'.")

(defconst lem-etc-dir (concat lem-temp-dir "etc/")
  "The directory for non-volatile storage.
These are not deleted or tampered with by emacs functions. Use
this for dependencies like servers or config files that are
stable (i.e. it should be unlikely that you need to delete them
if something goes wrong).")


(defconst lem-cache-dir (concat lem-temp-dir "cache/")
  "The directory for volatile storage.
Use this for transient files that are generated on the fly like
caches and ephemeral/temporary files. Anything that may need to
be cleared if there are problems.")

;;;;; User Configuration Variables
;; Find the user configuration file
(defconst lem-config-file (expand-file-name "config.el" lem-user-dir)
  "The user's configuration file.")

;; These next two variables are both optional, but maybe convenient.
;; They are used with the functions `lem-goto-projects' and `lem-goto-elisp-library'.

;; Set user project directory
(defcustom lem-project-dir nil "Set the directory for user projects."
  :group 'lambda-emacs
  :type 'string)

;; Set user elisp project dir
(defcustom lem-user-elisp-dir nil
  "Directory for personal elisp projects."
  :group 'lambda-emacs
  :type 'string)

;;;;; Path Settings
;; Directory paths
(dolist (dir (list lem-local-dir lem-library-dir lem-etc-dir lem-cache-dir lem-user-dir lem-setup-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;;;; Load Path
;; Add all configuration files to load-path
(eval-and-compile
  (progn
    (push lem-setup-dir load-path)
    (push lem-user-dir load-path)))

;;;;; Exec Path
;; Set PATH properly for emacs. This should make a package like
;; `exec-path-from-shell' unnecessary

;; Set local (i.e. current user) bin path, if it exists
(when (file-directory-p (concat (getenv "HOME") "/bin"))
  (defconst lem-local-bin (concat (getenv "HOME") "/bin") "Local execs."))

;; If on a mac using homebrew set path correctly
;; NOTE: the location of homebrew depends on whether we're on mac silicon
(when (shell-command-to-string "command -v brew")
  (defconst homebrew (if (string= (shell-command-to-string "arch") "arm64") "/opt/homebrew/bin/" "/usr/local/bin/") "Path to homebrew packages."))

;; Define the system local bins
(defconst usr-local-bin "/usr/local/bin" "System bin.")
(defconst usr-local-sbin "/usr/local/sbin" "System sbin.")

;; Set paths
(setenv "PATH" (concat homebrew ":" (getenv "PATH") ":" lem-local-bin ":" usr-local-bin ":" usr-local-sbin))
(setq exec-path (append exec-path (list homebrew lem-local-bin usr-local-bin usr-local-sbin)))

;;;;; Package Settings
;; Use straight to manage package installation and use-package to manage
;; settings. Defer package loading as much as possible to either the
;; after-init-hook or after some number of seconds of idle. This should produce
;; shorter startup times, which helps especially when doing, e.g., a quick
;; restart-and-check of something in emacs.

;;;;; Straight
;;;;;; Straight settings
;; Use straight.el to install all packages
;; https://github.com/raxod502/straight.el
;; Don't check packages on startup
(customize-set-variable 'straight-check-for-modifications '(check-on-save find-when-checking))
;; Set branch
(customize-set-variable 'straight-repository-branch "develop")
;; Set dir
(customize-set-variable 'straight-base-dir lem-library-dir)
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

;;;;; Use-Package
;; Install use package
(straight-use-package 'use-package)
;; Settings
(use-package use-package
  :custom
  (use-package-always-defer nil)
  (use-package-verbose t)
  ;; This is really helpful for profiling
  (use-package-minimum-reported-time 0)
  (use-package-enable-imenu-support t)
  (use-package-expand-minimally nil)
  ;; Let straight handle package install
  (use-package-always-ensure nil))

;;;;; El-Patch
;; Package for helping advise/modify features of other packages
(use-package el-patch
  :straight (:type git :host github :repo "radian-software/el-patch")
  :custom
  (el-patch-enable-use-package-integration t))

;;;;; Security
;; Properly verify outgoing ssl connections.
;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html
(use-package gnutls
  :straight nil
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
  :straight (:type built-in)
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

;;;;; Measure Time Macro
;; Useful macro to wrap functions in for testing
;; See https://stackoverflow.com/q/23622296
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "*Elapsed time: %.06f*" (float-time (time-since time)))))

;;;;; Load Configuration Modules
;; Lambda-Emacs loads a series of lisp-libraries or 'modules'. Which modules are
;; loaded is left to the user to set in `config.el', though if there is no
;; `config.el' file a default set of modules will be loaded.

(defun lem--default-modules ()
  "Load a default configuration for 𝛌-Emacs."
  (message "*Loading default setup of 𝛌-Emacs Modules*")
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

                    ;; Writing modules
                    'lem-setup-writing
                    'lem-setup-notes
                    'lem-setup-citation

                    ;; Programming modules
                    'lem-setup-programming
                    'lem-setup-debug
                    'lem-setup-shell

                    ;; Org modules
                    ;; NOTE: other modules are enabled in org-base
                    'lem-setup-org-base

                    ;; Productivity
                    'lem-setup-pdf))
     (require mod))))

(defun lem--default-config-modules ()
  "Load 𝛌-Emacs with a minimal set of modules."
  (message "*Loading 𝛌-Emacs, with minimal modules*")
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
                    'lem-setup-theme
                    'lem-setup-splash

                    ;; Project & Tab/Workspace modules
                    'lem-setup-vc
                    'lem-setup-projects
                    'lem-setup-tabs
                    'lem-setup-workspaces

                    ;; Navigation & Search modules
                    'lem-setup-navigation
                    'lem-setup-dired
                    'lem-setup-search

                    ;; Programming modules
                    'lem-setup-programming
                    ))
     (require mod))))

(defun lem--minimal-modules ()
  "Load 𝛌-Emacs with a minimal set of modules."
  (message "*Loading 𝛌-Emacs, with minimal modules*")
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
