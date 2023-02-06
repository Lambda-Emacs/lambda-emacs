;;;; lem-package.el --- Configuration to use `package.el'.  -*- lexical-binding: t; -*-

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
;; The code here is largely taken from the `Crafted Emacs' distro.
;; See https://github.com/SystemCrafters/crafted-emacs for original code.

;; This library provides a package related interface for `Lambda-Emacs'.

;; So far, it has three backends:
;; - `package.el' -- The default; built-in.
;; - `straight.el' -- A popular option.
;; - `elpaca.el' -- the next generation replacement for straight (sill in development)

;; Other backends could be added.  To add a backend, add the name to
;; the list above, provide a bootstrap file (the name must match
;; `lem-%s-bootstrap.el' or it will fail to be loaded with this
;; code), and make sure to implement the following macros (at a
;; minimum):
;;
;; See the bootstrap files in this directory for examples.

;;; Code:

(defvar lem-package-system 'package
  "What package system to use.

By default, it uses 'package for `package.el'.  Another option is
'straight for `straight.el' or 'elpaca fr `elpaca.el'.")

(defun lem-package-bootstrap (&optional system)
  "Load the configuration and defaults to the selected package.

This will check for the value of the variable
`lem-package-system', but could be overriden with the
optional parameter SYSTEM.

This is called when `early-init.el' runs."
  (let* ((module (make-symbol (format "lem-%s-bootstrap.el"
                                      (symbol-name (or system
                                                       lem-package-system
                                                       ;; In case both above are nil
                                                       'package)))))
         (module-path (expand-file-name (symbol-name module) lem-bootstrap-directory)))
    (if (file-exists-p module-path)
        (load module-path)
      (error "Could not find module %s" module))))

(provide 'lem-package)

;;; lem-package.el ends here
