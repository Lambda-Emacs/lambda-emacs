;;; lem-setup-elfeed.el ---Setup for elfeed         -*- lexical-binding: t; -*-
;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT
;; Author: Colin McLear

;;; Commentary:
;; Elfeed is an extensible web feed reader for Emacs, supporting both Atom and RSS. It requires Emacs 24.3 or higher.

;; NOTE: feeds must be set by the user in their config.el file. See the
;; docstring for the `elfeed-feeds` variable for further info.

;;; Code:

;;;; Elfeed
(use-package elfeed
  ;; :straight (:type git :host github :repo "skeeto/elfeed")
  :commands (elfeed-load-db-and-open)
  :bind (:map elfeed-search-mode-map
         ("a" . elfeed-show-all)
         ("e" . elfeed-show-emacs)
         ("d" . elfeed-show-daily)
         ("q" . elfeed-save-db-and-bury)
         ("R" . elfeed-mark-all-as-read)
         ("U" . elfeed-update)
         ("p" . elfeed-play-in-external-player))
  :custom
  (elfeed-db-directory (concat lem-cache-dir ".elfeed"))
  :config
  ;; Make sure the database is created
  (when (not (file-exists-p (concat elfeed-db-directory "/data")))
    (elfeed-update))
  ;; Useful functions for elfeed
  (defun elfeed-show-all ()
    "Show all feeds within the last two months."
    (interactive)
    (elfeed-search-set-filter "@2-months-ago"))

  (defun elfeed-mark-all-as-read ()
    "Mark all feeds in buffer as read."
    (interactive)
    (mark-whole-buffer)
    (elfeed-search-untag-all-unread))

  (defun elfeed-play-in-external-player ()
    "Play with mpv."
    (interactive)
    (elfeed-search-yank)
    (interactive)
    (play-with-mpv (substring-no-properties (car kill-ring))))

  ;; Functions to support syncing .elfeed between machines makes sure elfeed
  ;; reads index from disk before launching
  (defun elfeed-load-db-and-open ()
    "Wrapper to load the elfeed db from disk before opening."
    (interactive)
    (elfeed-db-load)
    (elfeed)
    (elfeed-search-update--force))

  ;; Write to disk when quitting
  (defun elfeed-save-db-and-bury ()
    "Wrapper to save the elfeed db to disk before burying buffer."
    (interactive)
    (elfeed-db-save)
    (quit-window)))

;;;; Elfeed Tube
;; Elfeed Tube is an Emacs package for a richer, interactive, noise-free and
;; fully text-capable interface to your Youtube subscriptions and playlists
;; using Elfeed, the RSS feed reader for Emacs.

(use-package elfeed-tube
  ;; :straight (:type git :host github :repo "karthink/elfeed-tube")
  :after elfeed
  :demand t
  :bind (:map elfeed-show-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save)
         :map elfeed-search-mode-map
         ("F" . elfeed-tube-fetch)
         ([remap save-buffer] . elfeed-tube-save))
  :custom
  (elfeed-tube-auto-save-p nil) ; default value
  (elfeed-tube-auto-fetch-p t)  ; default value
  :config
  (elfeed-tube-setup))

(provide 'lem-setup-elfeed)
;;; lem-setup-elfeed.el ends here
