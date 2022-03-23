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
  "The root directory for local Emacs files. This contains all
elisp libraries as well as non-essential and/or ephemeral files.
There are two main directories -- lem-library-dir, which
contains all libraries and packages, and lem-temp-dir, which
contains non-essential files and emphemera.")

(defconst lem-library-dir (concat lem-local-dir "lambda-library/")
  "The directory for Lisp libraries. This will house any personal
Lisp libraries as well as all setup libraries and external packages.")

(defconst lem-user-dir (concat lem-library-dir "lambda-user/")
  "Storage for personal elisp, scripts, and any other private files.")

(defconst lem-setup-dir (concat lem-library-dir "lambda-setup/")
  "The storage location of the setup-init files.")

(defconst lem-temp-dir (concat lem-local-dir "temp/")
  "The directory for non-essential file storage. Used by
  `lem-etc-dir' and `lem-cache-dir'.")

(defconst lem-etc-dir (concat lem-temp-dir "etc/")
  "The directory for non-volatile storage. These are not deleted or
tampered with by emacs functions. Use this for dependencies like
servers or config files that are stable (i.e. it should be
unlikely that you need to delete them if something goes wrong).")

(defconst lem-cache-dir (concat lem-temp-dir "cache/")
  "The directory for volatile storage. Use this for transient files
that are generated on the fly like caches and ephemeral/temporary
files. Anything that may need to be cleared if there are
problems.")

;;;;; User Configuration Variables
;; Find the user configuration file
(defvar lem-config-file (expand-file-name "config.el" lem-user-dir)
  "The user's configuration file.")

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

;; Set PATH properly for emacs. This should make a package like
;; `exec-path-from-shell' unnecessary

;; Set local (i.e. current user) bin path
(when (file-directory-p (concat (getenv "HOME") "/bin"))
  (defconst lem-local-bin (concat (getenv "HOME") "/bin") "Local execs."))

;; If on a mac using homebrew set path correctly
;; NOTE the location of homebrew depends on whether we're on mac silicon
(when (shell-command-to-string "command -v brew")
  (defconst homebrew (if (string= (shell-command-to-string "arch") "arm64") "/opt/homebrew/bin/" "/usr/local/bin/") "Path to homebrew packages."))

;; Define the system local bins
(defconst usr-local-bin "/usr/local/bin" "System bin.")
(defconst usr-local-sbin "/usr/local/sbin" "System sbin.")

;; Set paths
(setenv "PATH" (concat homebrew ":" (getenv "PATH") ":" lem-local-bin ":" usr-local-bin ":" usr-local-sbin))
(setq exec-path (append exec-path (list homebrew lem-local-bin usr-local-bin usr-local-sbin)))

;;;; Package Settings
;; Use straight and use-package to manage settings. Defer package loading as
;; much as possible to either the after-init-hook or after some number of
;; seconds of idle. This helps especially when doing, e.g., a quick
;; restart-and-check of something in emacs.

;;;;; Straight
;;;;;; Straight settings
;; Use straight.el to install all packages
;; https://github.com/raxod502/straight.el
;; Don't check packages on startup
(setq straight-check-for-modifications '(check-on-save find-when-checking))
;; Set branch
(setq straight-repository-branch "develop")
;; Set dir
(setq straight-base-dir lem-library-dir)
;; Use use-package
(setq straight-use-package-by-default t)
;; Check updates manually
(setq straight-vc-git-auto-fast-forward nil)
;; Avoid problems with straight building with native-comp
;; See https://github.com/raxod502/straight.el/issues/757
(setq native-comp-deferred-compilation-deny-list nil)
;; Tell straight.el about the profiles we are going to be using.
(setq straight-profiles
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

;;;;;; Straight Helper Functions
;; delete .DS_Store before prune
(advice-add 'straight-prune-build :before #'(lambda () (move-file-to-trash "/Users/roambot/.emacs.d/.local/straight/build/.DS_Store")))

;;;;; Use-Package
;; install use package
(straight-use-package 'use-package)
;; settings
(setq use-package-always-defer nil
      use-package-verbose t
      use-package-minimum-reported-time 0
      use-package-enable-imenu-support t
      use-package-expand-minimally nil
      use-package-always-ensure nil)

;;;;; El-Patch
;; Package for helping advise/modify features of other packages

(use-package el-patch
  :straight t
  :config
  (setq el-patch-enable-use-package-integration t))

;;;; Security
;; Properly verify outgoing ssl connections.
;; See https://glyph.twistedmatrix.com/2015/11/editor-malware.html

(use-package gnutls
  :straight nil
  :defer 1
  :init
  (setq gnutls-verify-error t
        gnutls-min-prime-bits 3072))

;;;; Benchmark Init
(use-package benchmark-init
  ;; demand when using
  ;; :demand t
  :defer t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'emacs-startup-hook 'benchmark-init/deactivate))

;;;; Auto-compile
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

;;;; Command Line Switches

;; Conditionally load parts of config depending on command line switches.
;; This allows startup with a clean emacs that still recognizes straight.
;; Helpful for testing packages.
;; Use (straight-use-package) command to selectively load packages.
;; See https://emacs.stackexchange.com/a/34909/11934
;; For function switch see https://stackoverflow.com/a/4065412/6277148

(defvar lem-config-switches
  '("clean"
    "core"
    "test")
  "Custom switches for conditional loading from command line.
`clean' loads only the init.el file w/no personal config; `core'
loads the set of modules set in `lem-core-modules'; `test' loads
only a lem-setup-test.el' file for easy testing.")

(defun lem--emacs-switches (switch)
  "Depending on command line argument SWITCH, load Emacs with minimal settings & no modules; useful for testing."
  (let ((found-switch (member switch command-line-args)))
    (setq command-line-args (delete switch command-line-args))
    found-switch))

;;;; Emacs Build Version
;; When built emacs has git-version patch
;; to include git sha1 in emacs-version string
(setq site-lisp "/Applications/Emacs.app/Contents/Resources/site-lisp/")
(when (file-exists-p (concat site-lisp "emacs-git-version.el"))
  (require 'emacs-git-version))

(defun lem/emacs-version ()
  "Print emacs-version and put emacs-version string on the kill ring"
  (interactive)
  (let ((emacs (emacs-version)))
    (message (emacs-version))
    (kill-new emacs)))

;;;; Outline Navigation
;; Packages to help with navigating
;; I used to use outshine.el but it was overkill -- these packages are much smaller/simpler

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

;; Make outline faces look better
(use-package outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    'outline-minor-faces-add-font-lock-keywords))

;;;; Load Configuration
;; What counts as a `core' module is open to debate, but the below are recommended
(defun lem--core-modules ()
  "Load 𝛌-Emacs's core files for an interactive session."
  (require 'lem-setup-libraries)
  (require 'lem-setup-settings)
  (require 'lem-setup-functions)
  (require 'lem-setup-macros)
  (require 'lem-setup-server))

(defun lem--ui-modules ()
  (require 'lem-setup-frames)
  (require 'lem-setup-windows)
  (require 'lem-setup-buffers)
  (require 'lem-setup-splash)
  (require 'lem-setup-colors)
  (require 'lem-setup-completion)
  (require 'lem-setup-fonts)
  (require 'lem-setup-faces)
  (require 'lem-setup-keybindings)
  (require 'lem-setup-navigation)
  (require 'lem-setup-dired)
  (require 'lem-setup-help)
  (require 'lem-setup-tabs)
  (require 'lem-setup-theme)
  (require 'lem-setup-modeline)
  (require 'lem-setup-search)
  (when sys-mac
    (require 'lem-setup-macos))
  )

(defun lem--project-modules ()
  (require 'lem-setup-projects)
  (require 'lem-setup-vc)
  )

(defun lem--editing-modules ()
  (require 'lem-setup-citation)
  (require 'lem-setup-writing)
  (require 'lem-setup-programming) ;; some of this should go in language module
  (require 'lem-setup-debug)
  (require 'lem-setup-notes)
  )
(defun lem--language-modules ()
  )
(defun lem--shell-modules ()
  (require 'lem-setup-shell)
  )
(defun lem--org-modules ()
  (require 'lem-setup-org)
  (require 'lem-setup-org-extensions)
  )
(defun lem--misc-modules ()
  (require 'lem-setup-pdf)
  (require 'lem-setup-email)
  (require 'lem-setup-scratch)
  )

;; Conditionally load configuration files based on switches.
(cond
 ;; Load core modules only, ignore other configuration.
 ((lem--emacs-switches "-core")
  (progn
    (message "*Loading 𝛌-Emacs core configuration files *only*, ignoring personal user configuration.*")
    (set-window-margins (selected-window) 30 15)
    (set-window-fringes (selected-window) 0 0 nil)
    ;; FIXME: Would be good to have a real splash page here but the screen flashes when frame changes size.
    (setq initial-scratch-message "Welcome to 𝛌-Emacs")
    (lem--core-modules)))
 ;; Load the just essential modules
 ((lem--emacs-switches "-basic")
  (progn
    (message "*Loading basic 𝛌-Emacs configuration*")
    (lem--core-modules)
    (lem--ui-modules)))
 ;; Load test module only
 ((lem--emacs-switches "-test")
  (progn
    (message "*Loading test file*")
    (require 'lem-setup-test)))
 ;; Load no modules other than the internal icomplete, for better completion.
 ((lem--emacs-switches "-clean")
  (message "*Do not load 𝛌-Emacs setup files. Loading a clean setup plus icomplete*")
  (require 'lem-setup-icomplete))
 ;; Load user's personal config file (if it exists).
 ((when (file-exists-p lem-config-file)
    (progn
      (message "*Loading 𝛌-Emacs & user config*")
      (load lem-config-file 'noerror))))
 ;; Otherwise load core modules
 ((message "*Loading 𝛌-Emacs*")
  (lem--core-modules)
  (lem--ui-modules)
  (lem--project-modules)
  (lem--editing-modules)
  (lem--language-modules)
  (lem--shell-modules)
  (lem--org-modules)
  (lem--misc-modules)))

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
