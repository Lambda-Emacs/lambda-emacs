;;; setup-debug.el --- summary -*- lexical-binding: t -*-

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

;; Setup debugging packages

;;; Code:

;;;; Bug Hunter
(use-package bug-hunter
  :defer 2)

;;;; Emacs Profiling
;; might be worth checking this out more closely
;; https://github.com/raxod502/radian/blob/c4246176155873d3937ff997965279048dabbc01/emacs/radian.el#L4423-L4476
(use-package esup
  :commands esup
  :config
  (setq esup-depth 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'lem-setup-debug)
;;; setup-debug.el ends here
