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

;;;; Native Comp

;; See https://github.com/jimeh/build-emacs-for-macos#native-comp
;; https://akrl.sdf.org/gccemacs.html#org335c0de
;; https://github.com/emacscollective/no-littering/wiki/Setting-gccemacs'-eln-cache
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=53891
;; https://emacs.stackexchange.com/a/70478/11934

;; See if native-comp is available
(when (not (and (fboundp 'native-comp-available-p)
                (native-comp-available-p)))
  (message "Native complation is *not* available"))

;; Put eln-cache dir in cache directory
;; NOTE the method for setting the eln-cache dir depends on the emacs version
(when (fboundp 'startup-redirect-eln-cache)
  (if (version< emacs-version "29")
      (add-to-list 'native-comp-eln-load-path (convert-standard-filename (expand-file-name ".local/temp/cache/eln-cache/" user-emacs-directory)))
    (startup-redirect-eln-cache (convert-standard-filename (expand-file-name ".local/temp/cache/eln-cache/" user-emacs-directory)))))

;; Silence nativecomp warnings popping up
(customize-set-variable 'native-comp-async-report-warnings-errors nil)

;; Native-comp settings
(customize-set-variable 'native-comp-speed 2)
(customize-set-variable 'native-comp-deferred-compilation t)

;;;; Set C Directory
;; NOTE this assumes that the C source files are included with emacs.
;; This depends on the build process used.
;; For one example see https://github.com/mclear-tools/build-emacs-macos
(setq find-function-C-source-directory "/Applications/Emacs.app/Contents/Resources/src")

;;;; Prefer Newer files
;; Prefer newer versions of files
(setq load-prefer-newer t)

;;;; Byte Compile Warnings
;; Disable certain byte compiler warnings to cut down on the noise. This is a
;; personal choice and can be removed if you would like to see any and all byte
;; compiler warnings.
(customize-set-variable 'byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local obsolete))
;;;; Check Errors
;; Don't produce backtraces when errors occur.
;; This can be set to `t' interactively when debugging.
(customize-set-variable 'debug-on-error nil)

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
;; https://www.reddit.com/r/emacs/comments/9jp9zt/anyone_know_what_variable_binding_depth_exceeds/
(customize-set-variable 'max-specpdl-size 13000)

;;;; Package settings
;; We use straight not package.el for all package loading.
;; So we don't need package.el loaded at startup (or at all).
(customize-set-variable 'package-enable-at-startup nil)
(advice-add #'package--ensure-init-file :override #'ignore)

;; Do not allow loading from the package cache (same reason).
(customize-set-variable 'package-quickstart nil)

;;;; Clean View
;; UI - Disable visual cruft

;; Resizing the Emacs frame can be an expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq-default frame-inhibit-implied-resize t)
;; HACK: Don't show size info (or anything else) in frame title
(setq-default frame-title-format "\n")
;; Disable start-up screen
(setq-default inhibit-startup-screen t)
(setq-default inhibit-startup-message t)
;; We'll provide our own splash screen, thanks
(setq-default inhibit-splash-screen t)
;; No message in initial scratch buffer
(setq-default initial-scratch-message nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
;; Disable tool and scrollbars. These are just clutter (the scrollbar
;; also impacts performance).
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; no titlebar (disable if not using a WM)
(push '(undecorated . t) default-frame-alist)

;; And set these to nil so users don't have to toggle the modes twice to
;; reactivate them.
(customize-set-variable 'tool-bar-mode nil)
(customize-set-variable 'scroll-bar-mode nil)

;; Fundamental mode at startup.
;; This helps with load-time since no extra libraries are loaded.
(setq initial-major-mode 'fundamental-mode)

;; Echo buffer -- don't display any message
;; https://emacs.stackexchange.com/a/437/11934
(defun display-startup-echo-area-message ()
  (message ""))

;;;; Default Theme & Custom Settings
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
(defconst active-theme nil "Variable for holding light/dark value of theme appearance.")

(defun lem--apply-default-background (appearance)
  "If no other theme is set, load default background color.
This takes current system APPEARANCE into consideration and
avoids the frame flashing on startup. Automatically remove this
hook after running."
  (progn
    (remove-hook 'ns-system-appearance-change-functions #'lem--apply-default-background)
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (progn
                (setq active-theme 'light-theme)
                (set-foreground-color "#282b35")
                (set-background-color "#fffef9")))
      ('dark  (progn
                (setq active-theme 'dark-theme)
                (set-foreground-color "#eceff1")
                (set-background-color "#282b35"))))))

(add-hook 'ns-system-appearance-change-functions #'lem--apply-default-background)

;; Check if there is a user early-config file. If not then load modus-themes as default.
(let ((early-config-file (expand-file-name "early-config.el" "~/.emacs.d/.local/lambda-library/lambda-user/")))
  (when (file-exists-p early-config-file)
    (load early-config-file nil 'nomessage)))

;;; early-init.el ends here
