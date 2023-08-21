;; -*- lexical-binding: t -*-
;; Project Management -- using project.el, bookmarks, and git

;;; Project
;; Use project to switch to, and search in, projects (replaces projectile)
(use-package project
  :ensure nil
  :commands (project-find-file
             project-switch-to-buffer
             project-switch-project
             project-switch-project-open-file)
  :bind (:map project-prefix-map
         ("P" .  project-switch-project)
         ("t" .  lem-goto-projects)
         ("R" .  project-remember-projects-under))
  :custom
  (project-list-file (concat lem-cache-dir "projects"))
  (project-switch-commands '((project-find-file "Find file")
                             (project-find-regexp "Find regexp")
                             (project-find-dir "Find directory")
                             (project-vc-dir "VC-Dir")
                             (project-magit-dir "Magit status")))
  (project-vc-extra-root-markers '(".dir-locals.el" ".project.el" "package.json" "requirements.txt" "autogen.sh"))
  :config
  ;; Use Ripgrep if installed
  (when (shell-command-to-string "command rg --version")
    (setq xref-search-program 'ripgrep))
  (setq lem-project-dir "~/Dropbox/Work/projects")
  ;; remove deleted projects from list
  (project-forget-zombie-projects))

(defun lem--project-name ()
  "Return name of project without path"
  (file-name-nondirectory (directory-file-name (if (vc-root-dir) (vc-root-dir) "-"))))

;; magit function for project
(defun project-magit-dir ()
  "Run magit in the current project's root"
  (interactive)
  (magit-status))
;; Add to keymap
(define-key (current-global-map) (kbd "C-x p G") #'project-magit-dir)

;;;; Open project & file
(with-eval-after-load 'project
  (defun project-switch-project-open-file (dir)
    "Switch to another project by running an Emacs command.
Open file using project-find-file

When called in a program, it will use the project corresponding
to directory DIR."
    (interactive (list (project-prompt-project-dir)))
    (let ((default-directory dir)
          (project-current-inhibit-prompt t))
      (call-interactively 'project-find-file))))

;;; Bookmarks
(use-package bookmark
  :ensure nil
  :defer 2
  :config
  (setq bookmark-default-file (concat lem-cache-dir "bookmarks")))

;;; New Git Project
(defun lem-git-new-project ()
  "Initializes a new git repo and adds it to project.el's known projects."
  (interactive)
  (let ((project-dir (expand-file-name
                      (read-directory-name "New project root:"))))
    (magit-init project-dir)
    (setq default-directory project-dir)
    ;; make sure project.el remembers new project
    (let ((pr (project--find-in-directory default-directory)))
      (project-remember-project pr))))

;;; Clone a Git Repo from Clipboard
;; http://xenodium.com/emacs-clone-git-repo-from-clipboard/
(defun lem-git-clone-clipboard-url ()
  "Clone git URL in clipboard asynchronously and open in dired when finishe.
Git repo is cloned to directory set by `lem-user-elisp-dir'."
  (interactive)
  (cl-assert (string-match-p "^\\(http\\|https\\|ssh\\)://" (current-kill 0)) nil "No URL in clipboard")
  (let* ((url (current-kill 0))
         (download-dir lem-user-elisp-dir)
         (project-dir (concat (file-name-as-directory download-dir)
                              (file-name-base url)))
         (default-directory download-dir)
         (command (format "git clone %s" url))
         (buffer (generate-new-buffer (format "*%s*" command)))
         (proc))
    (when (file-exists-p project-dir)
      (if (y-or-n-p (format "%s exists. delete?" (file-name-base url)))
          (delete-directory project-dir t)
        (user-error "Bailed")))
    (switch-to-buffer buffer)
    (setq proc (start-process-shell-command (nth 0 (split-string command)) buffer command))
    (with-current-buffer buffer
      (setq default-directory download-dir)
      (shell-command-save-pos-or-erase)
      (require 'shell)
      (shell-mode)
      (view-mode +1))
    (set-process-sentinel proc (lambda (process state)
                                 (let ((output (with-current-buffer (process-buffer process)
                                                 (buffer-string))))
                                   (kill-buffer (process-buffer process))
                                   (if (= (process-exit-status process) 0)
                                       (progn
                                         (message "finished: %s" command)
                                         (dired project-dir))
                                     (user-error (format "%s\n%s" command output))))))
    (set-process-filter proc #'comint-output-filter)))

;;; End Projects.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'lem-setup-projects)
