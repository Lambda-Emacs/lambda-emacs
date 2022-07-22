;; Additions & modifications to org functionality
;; NOTE: some org-extensions are set in other setup files:
;; - org roam setup is in setup-notes
;; - org citations setup is in setup-citation
;; - some org export functions are in setup-teaching

;;; Org Appearance
;;;; Org-Appear (Show Markup/Pretty Entities)
;; show markup at point -- this should be part of org!
(use-package org-appear
  :straight (:type git :host github :repo "awth13/org-appear"
             :branch "master")
  :commands (org-appear-mode)
  :custom
  (org-appear-autoemphasis  t)
  (org-appear-autolinks nil)
  (org-appear-autosubmarkers t)
  :hook (org-mode . org-appear-mode))

;;;; Org Modern (Display properties, bullets, etc)
;; A nicer set of default display options
(use-package org-modern
  :straight (:type git :host github :repo "minad/org-modern")
  :hook (org-mode . org-modern-mode)
  :custom-face
  (org-modern-label ((t (:height 1.0 :inherit ,(if lambda-themes-set-variable-pitch 'variable-pitch 'default)))))
  :custom
  (org-modern-hide-stars nil) ;; compatibility w/org-indent
  ;; don't use other faces
  (org-modern-priority nil)
  (org-modern-todo nil)
  (org-modern-tag t)
  ;; Customize this per your font
  (org-modern-label-border .25)
  ;; Note that these stars allow differentiation of levels
  ;; "①" "②" "③" "④" "⑤" "⑥" "⑦"
  (org-modern-star ["⦶" "⦷" "⦹" "⊕" "⍟" "⊛" "⏣" "❂"]))

;;;; Org Modern Indent
;; Make org-modern work better with org-indent
(use-package org-modern-indent
  :straight (:type git :host github :repo "jdtsmith/org-modern-indent")
  :hook (org-indent-mode . org-modern-indent-mode))

;;; Org Autolist (Smart Lists)
;; Better list behavior
(use-package org-auto-list
  :straight (:type git :host github :repo "calvinwyoung/org-autolist")
  :hook (org-mode . org-autolist-mode))

;;; Org Babel
;; Avoid `org-babel-do-load-languages' since it does an eager require.
(use-package ob-python
  :straight nil
  :defer t
  :commands (org-babel-execute:python)
  :config
  (progn
    (setq org-babel-python-command "python3"))) ;Default to python 3.x

(use-package ob-ditaa
  :straight nil
  :defer t
  :config
  (progn
    ;; http://pages.sachachua.com/.emacs.d/Sacha.html
    (setq org-ditaa-jar-path (expand-file-name
                              "ditaa.jar"
                              (concat user-emacs-directory "software/")))))

(use-package ob-plantuml
  :straight nil
  :defer t
  :config
  (progn
    (setq org-plantuml-jar-path (expand-file-name
                                 "plantuml.jar"
                                 (concat user-emacs-directory "software/")))

    (defun lem-advice-org-babel-execute:plantuml (orig-fun &rest args)
      "Force `shell-file-name' to be bash as the \">\" operator is used for redirection.

If this forcing is not done, and if `shell-file-name' is tcsh,
\">\" does not work.  When trying to overwrite files, we get a
\"File exists\" error, and \">!\" would need to be used instead.

Instead it's simpler to use bash."
      (let ((shell-file-name (executable-find "bash")))
        (apply orig-fun args)))
    (advice-add 'org-babel-execute:plantuml :around #'lem-advice-org-babel-execute:plantuml)))

(use-package ob-shell
  :straight nil
  :defer t
  :commands
  (org-babel-execute:sh
   org-babel-expand-body:sh
   org-babel-execute:bash
   org-babel-expand-body:bash))

(use-package ob-lisp
  :straight nil
  :defer t
  :commands (org-babel-execute:lisp))

(use-package ob-latex
  :straight nil
  :defer t
  :commands
  (org-babel-execute:latex))

;;; Org Babel Tangle
(use-package ob-tangle
  :straight nil
  :defer t
  :config
  (progn
    ;; Trailing whitespace management
    ;; Delete trailing whitespace in tangled buffer and save it.
    (add-hook 'org-babel-post-tangle-hook #'delete-trailing-whitespace)
    (add-hook 'org-babel-post-tangle-hook #'save-buffer :append)))

;;; Org-Download
;; Drag and drop images to Emacs org-mode. Courtesy of abo-abo.
;; https://github.com/abo-abo/org-download.

(use-package org-download
  :commands (org-download-yank org-download-screenshot org-download-image)
  :custom
  (org-download-method 'directory)
  (org-download-image-dir (concat org-directory "org-pictures/"))
  (org-download-image-latex-width 500)
  (org-download-timestamp "%Y-%m-%d"))

;;; Org Devonthink Integration
(use-package org-devonthink
  :when sys-mac
  :straight (:type git :host github :repo "lasvice/org-devonthink")
  :commands (org-insert-dtp-link org-dtp-store-link))

;;; Org Export Extensions
;;;; Ox-Pandoc
;; Export w/pandoc
(use-package ox-pandoc
  :straight (:type git :host github :repo "a-fent/ox-pandoc")
  :after ox
  :custom
  (org-pandoc-command (expand-file-name "pandoc" homebrew))
  (org-pandoc-options '((standalone .  t)))
  (org-pandoc-options-for-docx '((standalone . nil)))
  (org-pandoc-options-for-beamer-pdf '((pdf-engine . "xelatex")))
  (org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex")))
  (org-pandoc-format-extensions '(org+smart)))

;;;; Ox-Pandoc Export Menu Options
;; Set pandoc export options
(setq org-pandoc-menu-entry
      '(
        ;;(?0 "to jats." org-pandoc-export-to-jats)
        ;;(?0 "to jats and open." org-pandoc-export-to-jats-and-open)
        ;;(?  "as jats." org-pandoc-export-as-jats)
        ;;(?1 "to epub2 and open." org-pandoc-export-to-epub2-and-open)
        ;;(?! "to epub2." org-pandoc-export-to-epub2)
        ;;(?2 "to tei." org-pandoc-export-to-tei)
        ;;(?2 "to tei and open." org-pandoc-export-to-tei-and-open)
        ;;(?" "as tei." org-pandoc-export-as-tei)
        ;;(?3 "to markdown_mmd." org-pandoc-export-to-markdown_mmd)
        ;;(?3 "to markdown_mmd and open." org-pandoc-export-to-markdown_mmd-and-open)
        ;;(?# "as markdown_mmd." org-pandoc-export-as-markdown_mmd)
        ;;(?4 "to html5." org-pandoc-export-to-html5)
        (?4 "to html5 and open." org-pandoc-export-to-html5-and-open)
        (?$ "as html5." org-pandoc-export-as-html5)
        (?5 "to html5-pdf and open." org-pandoc-export-to-html5-pdf-and-open)
        (?% "to html5-pdf." org-pandoc-export-to-html5-pdf)
        ;;(?6 "to markdown_phpextra." org-pandoc-export-to-markdown_phpextra)
        ;;(?6 "to markdown_phpextra and open." org-pandoc-export-to-markdown_phpextra-and-open)
        ;;(?& "as markdown_phpextra." org-pandoc-export-as-markdown_phpextra)
        ;;(?7 "to markdown_strict." org-pandoc-export-to-markdown_strict)
        ;;(?7 "to markdown_strict and open." org-pandoc-export-to-markdown_strict-and-open)
        ;;(?' "as markdown_strict." org-pandoc-export-as-markdown_strict)
        ;; (?8 "to opendocument." org-pandoc-export-to-opendocument)
        ;; (?8 "to opendocument and open." org-pandoc-export-to-opendocument-and-open)
        ;; (?( "as opendocument." org-pandoc-export-as-opendocument)
        (?8 "to opml." org-pandoc-export-to-opml)
        (?9 "to opml and open." org-pandoc-export-to-opml-and-open)
        ;; (?* "as opml." org-pandoc-export-as-opml)
        ;;(?: "to rst." org-pandoc-export-to-rst)
        ;;(?: "to rst and open." org-pandoc-export-to-rst-and-open)
        ;;(?* "as rst." org-pandoc-export-as-rst)
        ;;(?< "to slideous." org-pandoc-export-to-slideous)
        (?< "to slideous and open." org-pandoc-export-to-slideous-and-open)
        (?, "as slideous." org-pandoc-export-as-slideous)
        (?= "to ms-pdf and open." org-pandoc-export-to-ms-pdf-and-open)
        (?- "to ms-pdf." org-pandoc-export-to-ms-pdf)
        ;;(?> "to textile." org-pandoc-export-to-textile)
        ;;(?> "to textile and open." org-pandoc-export-to-textile-and-open)
        ;;(?. "as textile." org-pandoc-export-as-textile)
        ;;(?a "to asciidoc." org-pandoc-export-to-asciidoc)
        ;;(?a "to asciidoc and open." org-pandoc-export-to-asciidoc-and-open)
        ;;(?A "as asciidoc." org-pandoc-export-as-asciidoc)
        (?b "to beamer-pdf and open." org-pandoc-export-to-beamer-pdf-and-open)
        (?B "to beamer-pdf." org-pandoc-export-to-beamer-pdf)
        (?c "to context-pdf and open." org-pandoc-export-to-context-pdf-and-open)
        (?C "to context-pdf." org-pandoc-export-to-context-pdf)
        ;;(?d "to docbook5." org-pandoc-export-to-docbook5)
        (?d "to docbook5 and open." org-pandoc-export-to-docbook5-and-open)
        (?D "as docbook5." org-pandoc-export-as-docbook5)
        (?e "to epub3 and open." org-pandoc-export-to-epub3-and-open)
        (?E "to epub3." org-pandoc-export-to-epub3)
        ;;(?f "to fb2." org-pandoc-export-to-fb2)
        ;;(?f "to fb2 and open." org-pandoc-export-to-fb2-and-open)
        ;;(?F "as fb2." org-pandoc-export-as-fb2)
        ;;(?g "to gfm." org-pandoc-export-to-gfm)
        (?g "to gfm and open." org-pandoc-export-to-gfm-and-open)
        (?G "as gfm." org-pandoc-export-as-gfm)
        ;;(?h "to html4." org-pandoc-export-to-html4)
        (?h "to html4 and open." org-pandoc-export-to-html4-and-open)
        (?H "as html4." org-pandoc-export-as-html4)
        ;;(?i "to icml." org-pandoc-export-to-icml)
        (?i "to icml and open." org-pandoc-export-to-icml-and-open)
        (?I "as icml." org-pandoc-export-as-icml)
        ;;(?j "to json." org-pandoc-export-to-json)
        (?j "to json and open." org-pandoc-export-to-json-and-open)
        (?J "as json." org-pandoc-export-as-json)
        ;; (?k "to markdown." org-pandoc-export-to-markdown)
        (?k "to markdown and open." org-pandoc-export-to-markdown-and-open)
        (?K "as markdown." org-pandoc-export-as-markdown)
        (?l "to latex-pdf and open." org-pandoc-export-to-latex-pdf-and-open)
        (?L "to latex-pdf." org-pandoc-export-to-latex-pdf)
        ;;(?m "to man." org-pandoc-export-to-man)
        (?m "to man and open." org-pandoc-export-to-man-and-open)
        (?M "as man." org-pandoc-export-as-man)
        ;;(?n "to native." org-pandoc-export-to-native)
        (?n "to native and open." org-pandoc-export-to-native-and-open)
        (?N "as native." org-pandoc-export-as-native)
        (?o "to odt and open." org-pandoc-export-to-odt-and-open)
        (?O "to odt." org-pandoc-export-to-odt)
        (?p "to pptx and open." org-pandoc-export-to-pptx-and-open)
        (?P "to pptx." org-pandoc-export-to-pptx)
        ;;(?q "to commonmark." org-pandoc-export-to-commonmark)
        ;;(?q "to commonmark and open." org-pandoc-export-to-commonmark-and-open)
        ;;(?Q "as commonmark." org-pandoc-export-as-commonmark)
        ;;(?r "to rtf." org-pandoc-export-to-rtf)
        (?r "to rtf and open." org-pandoc-export-to-rtf-and-open)
        (?R "as rtf." org-pandoc-export-as-rtf)
        ;;(?s "to s5." org-pandoc-export-to-s5)
        ;;(?s "to s5 and open." org-pandoc-export-to-s5-and-open)
        ;;(?S "as s5." org-pandoc-export-as-s5)
        ;;(?t "to texinfo." org-pandoc-export-to-texinfo)
        ;;(?t "to texinfo and open." org-pandoc-export-to-texinfo-and-open)
        ;;(?T "as texinfo." org-pandoc-export-as-texinfo)
        ;;(?u "to dokuwiki." org-pandoc-export-to-dokuwiki)
        (?u "to dokuwiki and open." org-pandoc-export-to-dokuwiki-and-open)
        (?U "as dokuwiki." org-pandoc-export-as-dokuwiki)
        ;; (?v "to revealjs." org-pandoc-export-to-revealjs)
        (?v "to revealjs and open." org-pandoc-export-to-revealjs-and-open)
        (?V "as revealjs." org-pandoc-export-as-revealjs)
        ;;(?w "to mediawiki." org-pandoc-export-to-mediawiki)
        (?w "to mediawiki and open." org-pandoc-export-to-mediawiki-and-open)
        (?W "as mediawiki." org-pandoc-export-as-mediawiki)
        (?x "to docx and open." org-pandoc-export-to-docx-and-open)
        (?X "to docx." org-pandoc-export-to-docx)
        ;;(?y "to slidy." org-pandoc-export-to-slidy)
        (?y "to slidy and open." org-pandoc-export-to-slidy-and-open)
        (?Y "as slidy." org-pandoc-export-as-slidy)
        ;;(?z "to dzslides." org-pandoc-export-to-dzslides)
        (?z "to dzslides and open." org-pandoc-export-to-dzslides-and-open)
        (?Z "as dzslides." org-pandoc-export-as-dzslides)
        ;;(?{ "to muse." org-pandoc-export-to-muse)
        ;;(?{ "to muse and open." org-pandoc-export-to-muse-and-open)
        ;;(?[ "as muse." org-pandoc-export-as-muse)
        ;;(?} "to zimwiki." org-pandoc-export-to-zimwiki)
        ;;(?} "to zimwiki and open." org-pandoc-export-to-zimwiki-and-open)
        ;;(?] "as zimwiki." org-pandoc-export-as-zimwiki)
        ;;(?~ "to haddock." org-pandoc-export-to-haddock)
        ;;(?~ "to haddock and open." org-pandoc-export-to-haddock-and-open)
        ;;(?^ "as haddock." org-pandoc-export-as-haddock)
        ))


;;;; Ox-Hugo
;; Export to Hugo with Org
;; https://github.com/kaushalmodi/ox-hugo
(use-package ox-hugo :after ox)

;;;;; Batch Export Files with Org-Hugo
;; mark files and then batch export them with this command
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-+")
    (lambda()
      (interactive)
      (diredp-do-apply/eval 'org-hugo-export-wim-to-md '(4)))))

;;;;; Org-Hugo Links
;; New link type for Org-Hugo internal links
(defun org-hugo-link-complete ()
  "Create link with Hugo ref shortcode"
  (concat "{{% ref " (file-relative-name (read-file-name "File: ")) " %}}"))

(defun org-hugo-follow (link)
  (find-file (expand-file-name link)))

(with-eval-after-load 'org
  (org-link-set-parameters "hugo"
                           :complete 'org-hugo-link-complete
                           :follow 'org-hugo-follow))

;;; Org Html Conversion
(use-package htmlize
  :commands (htmlize-buffer))

;;; Org Menu
;; A menu for editing org-mode documents and exploring it’s features in a
;; discoverable way via transient menus.
(use-package org-menu
  :straight (:type git :host github :repo "sheijk/org-menu")
  :bind* (:map org-mode-map
          ("C-c m" . org-menu)))

;;; Org Pomodoro
;; Helps with time tracking
(use-package org-pomodoro
  :commands org-pomodoro
  :init
  (progn
    (setq org-pomodoro-audio-player "/usr/bin/afplay")))

;;; Provide Org Extensions
(provide 'lem-setup-org-extensions)
