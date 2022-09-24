;;; lem-setup-colors.el --- summary -*- lexical-binding: t -*-

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

;; Setup how emacs handles colors

;;; Code:

;;;; Color
(setq-default ns-use-srgb-colorspace t)

;;;; Show Colors
;; https://github.com/emacsmirror/rainbow-mode
;; Colorize color names in buffers
(use-package rainbow-mode
  :commands rainbow-mode)

(provide 'lem-setup-colors)
;;; lem-setup-colors.el ends here
