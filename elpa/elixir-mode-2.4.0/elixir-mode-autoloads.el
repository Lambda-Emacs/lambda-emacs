;;; elixir-mode-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "elixir-format" "elixir-format.el" (0 0 0 0))
;;; Generated autoloads from elixir-format.el

(autoload 'elixir-format "elixir-format" "\


\(fn &optional CALLED-INTERACTIVELY-P)" t nil)

(register-definition-prefixes "elixir-format" '("elixir-format-"))

;;;***

;;;### (autoloads nil "elixir-mode" "elixir-mode.el" (0 0 0 0))
;;; Generated autoloads from elixir-mode.el

(autoload 'elixir-mode-open-github "elixir-mode" "\
Elixir mode open GitHub page." t nil)

(autoload 'elixir-mode-open-elixir-home "elixir-mode" "\
Elixir mode go to language home." t nil)

(autoload 'elixir-mode-open-docs-master "elixir-mode" "\
Elixir mode go to master documentation." t nil)

(autoload 'elixir-mode-open-docs-stable "elixir-mode" "\
Elixir mode go to stable documentation." t nil)

(autoload 'elixir-mode-version "elixir-mode" "\
Get the Elixir-Mode version as string.

If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.

The returned string includes both, the version from package.el
and the library version, if both a present and different.

If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil.

\(fn &optional SHOW-VERSION)" t nil)

(autoload 'elixir-mode "elixir-mode" "\
Major mode for editing Elixir code.

\\{elixir-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.elixir\\'" . elixir-mode))

(add-to-list 'auto-mode-alist '("\\.ex\\'" . elixir-mode))

(add-to-list 'auto-mode-alist '("\\.exs\\'" . elixir-mode))

(register-definition-prefixes "elixir-mode" '("elixir-"))

;;;***

;;;### (autoloads nil "elixir-smie" "elixir-smie.el" (0 0 0 0))
;;; Generated autoloads from elixir-smie.el

(register-definition-prefixes "elixir-smie" '("elixir-" "verbose-elixir-smie-rules"))

;;;***

;;;### (autoloads nil nil ("elixir-mode-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; elixir-mode-autoloads.el ends here
