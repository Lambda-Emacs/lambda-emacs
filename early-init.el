;;; early-init.el --- summary -*- lexical-binding: t; no-byte-compile: t; mode: emacs-lisp; coding:utf-8; fill-column: 80 -*-
;; Author: Colin McLear
;; Maintainer: Colin McLear

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

;; This is the early-init file. Only for use with emacs 27 or higher. See
;; https://www.reddit.com/r/emacs/comments/7yns85/emacs_adds_support_for_a_second_read_earlier_init/
;; and https://lists.gnu.org/archive/html/emacs-devel/2017-10/msg00372.html for
;; more information.

;;; Code:

;;;; Speed up startup
;; Help speed up emacs initialization See
;; https://blog.d46.us/advanced-emacs-startup/ and
;; http://tvraman.github.io/emacspeak/blog/emacs-start-speed-up.html and
;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
;; This will be set back to normal at the end of the init file

(defvar lem-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;;; Measure Time Macro
;; Useful macro to wrap functions in for testing
;; See https://stackoverflow.com/q/23622296
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "
;; ======================================================
;; %s *Elapsed time: %.06f*
;; ======================================================
" (if load-file-name
      (file-name-nondirectory (format "%s |" load-file-name))
    "")
(float-time (time-since time)))))

;;;; Native Comp

;; See https://github.com/jimeh/build-emacs-for-macos#native-comp
;; https://akrl.sdf.org/gccemacs.html#org335c0de
;; https://github.com/emacscollective/no-littering/wiki/Setting-gccemacs'-eln-cache
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=53891
;; https://emacs.stackexchange.com/a/70478/11934

;; See if native-comp is available
(cond ((not (and (fboundp 'native-comp-available-p)
                 (native-comp-available-p)))
       (message "Native complation is *not* available"))
      ;; Put eln-cache dir in cache directory
      ;; NOTE the method for setting the eln-cache dir depends on the emacs version
      ((version< emacs-version "29")
       (setcar native-comp-eln-load-path
               (expand-file-name (convert-standard-filename "var/cache/eln-cache/") user-emacs-directory)))
      (t
       (startup-redirect-eln-cache (convert-standard-filename (expand-file-name "var/cache/eln-cache/" user-emacs-directory)))))

;; Silence nativecomp warnings popping up
(setopt native-comp-async-report-warnings-errors nil)

;; Native-comp settings
(setopt native-comp-speed 2)
(setopt native-comp-deferred-compilation t)

;;;; Garbage collection
;; Defer garbage collection further back in the startup process. We'll lower
;; this to a more reasonable number at the end of the init process (i.e. at end of
;; init.el)

(setq gc-cons-threshold most-positive-fixnum)

;; Adjust garbage collection thresholds during startup, and thereafter
;; See http://akrl.sdf.org https://gitlab.com/koral/gcmh

(defmacro k-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((time (current-time)))
     ,@body
     (float-time (time-since time))))

;; When idle for 15sec run the GC no matter what.
(defvar k-gc-timer
  (run-with-idle-timer 15 t
                       (lambda ()
                         (let ((inhibit-message t))
                           (message "Garbage Collector has run for %.06fsec"
                                    (k-time (garbage-collect)))))))

;;;; Clean View
;; UI - Disable visual cruft

;; Resizing the Emacs frame can be an expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setopt frame-inhibit-implied-resize t
        ;; HACK: Don't show size info (or anything else) in frame title
        frame-title-format "\n"
        ;; Disable start-up screen
        inhibit-startup-screen t
        inhibit-startup-message t
        ;; We'll provide our own splash screen, thanks
        inhibit-splash-screen t
        ;; No message in initial scratch buffer
        initial-scratch-message nil)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(setopt tool-bar-mode nil
        scroll-bar-mode nil)

;; Fundamental mode at startup.
;; This helps with load-time since no extra libraries are loaded.
(setopt initial-major-mode 'fundamental-mode)

;; Echo buffer -- don't display any message
;; https://emacs.stackexchange.com/a/437/11934
(defun display-startup-echo-area-message ()
  (message ""))

;;;; Set C Directory
;; NOTE this assumes that the C source files are included with emacs.
;; This depends on the build process used.
;; For one example see https://github.com/mclear-tools/build-emacs-macos
(setq find-function-C-source-directory "/Applications/Emacs.app/Contents/Resources/src")

;;;; System Variables
;; Check the system used
(defconst sys-linux   (eq system-type 'gnu/linux))
(defconst sys-mac     (eq system-type 'darwin))
(defconst sys-bsd     (or sys-mac (eq system-type 'berkeley-unix)))
(defconst sys-win     (memq system-type '(cygwin windows-nt ms-dos)))

;;;; Directory Variables
;;  We're going to define a number of directories that are used throughout this
;;  configuration to store different types of files. This is a bit like the
;;  `no-littering' package, and allows us to keep `user-emacs-directory' tidy.

(defconst lem-emacs-dir (expand-file-name user-emacs-directory)
  "The path to the emacs.d directory.")

(defconst lem-library-dir (concat lem-emacs-dir "lambda-library/")
  "The directory for 𝛌-Emacs Lisp libraries.
This will house all setup libraries and external libraries or packages.")

(defconst lem-user-dir (concat lem-library-dir "lambda-user/")
  "Storage for personal elisp, scripts, and any other private files.")

(defconst lem-setup-dir (concat lem-library-dir "lambda-setup/")
  "The storage location of the setup-init files.")

(defconst lem-var-dir (concat lem-emacs-dir "var/")
  "The directory for non-essential file storage.
Contents are subject to change. Used for package storage (elpa or
straight) and by `lem-etc-dir' and `lem-cache-dir'.")

(defconst lem-etc-dir (concat lem-var-dir "etc/")
  "The directory for non-volatile storage.
  These are not deleted or tampered with by emacs functions. Use
  this for dependencies like servers or config files that are
  stable (i.e. it should be unlikely that you need to delete them
               if something goes wrong).")

(defconst lem-cache-dir (concat lem-var-dir "cache/")
  "The directory for volatile storage.
  Use this for transient files that are generated on the fly like
  caches and ephemeral/temporary files. Anything that may need to
  be cleared if there are problems.")

(defconst lem-default-config-file (concat lem-library-dir "lem-default-config.el")
  "A sample default configuration of the personal config file to get the user started.")

;;;; User Configuration Variables

;; Define customization group for Lambda Emacs.
(defgroup lambda-emacs '()
  "An Emacs distribution with sane defaults, pre-configured packages, and useful functions, aimed at writing and academic work in the humanities."
  :tag "Lambda-Emacs"
  :link '(url-link "https://github.com/Lambda-Emacs/lambda-emacs")
  :group 'emacs)

;; Find the user configuration file
(defconst lem-config-file (expand-file-name "config.el" lem-user-dir)
  "The user's configuration file.")

;; These next two variables are both optional, but may be convenient.
;; They are used with the functions `lem-goto-projects' and `lem-goto-elisp-library'.

;; Set user project directory
(defcustom lem-project-dir nil "Set the directory for user projects."
  :group 'lambda-emacs
  :type 'string)

;; Set user elisp project dir
(defcustom lem-user-elisp-dir nil
  "Directory for personal elisp projects.
Any customized libraries not available via standard package repos like elpa or melpa should go here."
  :group 'lambda-emacs
  :type 'string)

;; Ensure packages?
(defcustom lem-package-ensure-packages t
  "Whether to ensure packages with use-package, or install manually using the list in `package-selected-packages'."
  :group 'lambda-emacs
  :type 'boolean)

;;;; Make System Directories
;; Directory paths
(dolist (dir (list lem-library-dir lem-var-dir lem-etc-dir lem-cache-dir lem-user-dir lem-setup-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;;;; Load Path
;; Add all configuration files to load-path
(eval-and-compile
  (progn
    (push lem-setup-dir load-path)
    (push lem-user-dir load-path)))

;;;; Prefer Newer files
;; Prefer newer versions of files
(setopt load-prefer-newer t)

;;;; Byte Compile Warnings
;; Disable certain byte compiler warnings to cut down on the noise. This is a
;; personal choice and can be removed if you would like to see any and all byte
;; compiler warnings.
;; NOTE: Setopt won't work here
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local obsolete))

;;;; Check Errors
;; Don't produce backtraces when errors occur.
;; This can be set to `t' interactively when debugging.
(setopt debug-on-error nil)

;;;; When-let errors
;; https://github.com/alphapapa/frame-purpose.el/issues/3
(eval-and-compile
  (when (version< emacs-version "26")
    (with-no-warnings
      (defalias 'when-let* #'when-let)
      (function-put #'when-let* 'lisp-indent-function 1)
      (defalias 'if-let* #'if-let)
      (function-put #'if-let* 'lisp-indent-function 2))))

;;;; Variable Binding Depth
;; This variable controls the number of lisp bindings that can exists at a time.
;; We should make it fairly large for modern machines.
;; NOTE: Obsolete on Emacs 29+
;; https://www.reddit.com/r/emacs/comments/9jp9zt/anyone_know_what_variable_binding_depth_exceeds/
(when (version< emacs-version "29")
  (setq max-specpdl-size 13000))

;;;; Custom Settings & Default Theme
;; Ordinarily we might leave theme loading until later in the init process, but
;; this leads to the initial frame flashing either light or dark color,
;; depending on the system settings. Let's avoid that by loading a default theme
;; before initial frame creation. The modus themes are built in and excellent.
;; NOTE: 1. The default theme is set only if there are no user configuration
;; files, otherwise it is left to the user to do; 2. This system check only
;; works for MacOS, with an emacs build with the ns-system-appearance patch. For
;; examples of such builds see https://github.com/mclear-tools/build-emacs-macos
;; or https://github.com/d12frosted/homebrew-emacs-plus

;; Use this variable for checking what the active them setting is vis-a-vis the
;; system light or dark mode.
(defvar active-theme nil "Variable for holding light/dark value of theme appearance.")
(defvar light-theme nil "Variable for holding light value of theme appearance.")
(defvar dark-theme nil "Variable for holding dark value of theme appearance.")

;;;; Bootstrap Package System
;; Load the package-system.
(require 'package)

;;;; Package Archives
;; See https://protesilaos.com/codelog/2022-05-13-emacs-elpa-devel/ for discussion
(setopt package-archives
        '(("elpa" . "https://elpa.gnu.org/packages/")
          ("elpa-devel" . "https://elpa.gnu.org/devel/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")
          ("melpa" . "https://melpa.org/packages/"))

        ;; Highest number gets priority (what is not mentioned gets priority 0)
        package-archive-priorities
        '(;; Prefer development packages
          ("elpa-devel" . 99)
          ("melpa" . 90))

        ;; Set location of package directory
        package-user-dir (expand-file-name "elpa/" lem-var-dir)
        package-gnupghome-dir (concat package-user-dir "gnupg"))

;; Make sure the elpa/ folder exists after setting it above.
(unless (file-exists-p package-user-dir)
  (mkdir package-user-dir t))
(setopt package-quickstart-file (expand-file-name "package-quickstart.el" lem-cache-dir))

;;;; Early Config
;; Check if there is a user early-config file & load. If it doesn't exist, print
;; a message saying so.
(let ((early-config-file (expand-file-name "early-config.el" lem-user-dir)))
  (cond ((file-exists-p early-config-file)
         (measure-time
          (load early-config-file nil 'nomessage))
         (message "early-config.el loaded!"))
        (t
         (message "No user early-config file exists.")
         (message "Loading default settings."))))

;;; early-init.el ends here
