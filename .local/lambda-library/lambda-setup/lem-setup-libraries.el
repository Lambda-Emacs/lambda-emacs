;;; lem-setup-libraries.el --- summary -*- lexical-binding: t -*-

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

;; https://github.com/jwiegley/emacs-async, https://github.com/magnars/s.el,
;; https://github.com/magnars/dash.el http://elpa.gnu.org/packages/cl-lib.html
;; Included are built-in libraries and those for asynchronous processing string
;; manipulation, list manipulation and backward compatibility respectively.

;;; Code:

;; Built in libraries; lots of packages depend on these
(use-package subr-x   :straight (:type built-in) :defer 1)
(use-package cl-lib   :straight (:type built-in) :defer t)
;; Include this only for compatibility purposes
(use-package cl       :straight (:type built-in) :defer t)

;; Asynchronous commands. Adds the ability to call asynchronous functions and
;; process with ease. See the documentation for `async-start' and
;; `async-start-process'.
(use-package async
  :straight t
  :defer
  :config
  (dired-async-mode 1)
  (setq dired-async--modeline-mode nil))

;; A modern list API for Emacs. No 'cl required.
(use-package dash     :straight t :defer 2)
;; String manipulation
(use-package s        :straight t :defer 2)
;; APIs for files & directories
(use-package f        :straight t :defer 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lem-setup-libraries)
;;; lem-setup-libraries.el ends here
