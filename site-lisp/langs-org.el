;;; langs-org.el --- Org related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Org related configuration for GNU Emacs.
;; For more see URL `http://orgmode.org'

;;; Code:

;; I use the following directory structure for Org:
;;
;;   $ tree ~/Org
;;   ~/Org
;;   ├── Archive.org
;;   ├── Inbox.org
;;   ├── Personal
;;   │   ├── Backlog.org
;;   │   ├── ...
;;   │   └── X.org
;;   ├── Work
;;   │   ├── Backlog.org
;;   │   ├── ...
;;   │   └── X.org
(use-package org
  :defines
  org-capture-bookmark
  org-capture-templates
  org-agenda-window-setup
  org-agenda-span
  org-agenda-skip-scheduled-if-deadline-is-shown
  org-agenda-todo-ignore-deadlines
  org-agenda-todo-ignore-scheduled
  org-agenda-sorting-strategy
  org-agenda-skip-deadline-prewarning-if-scheduled
  org-src-strip-leading-and-trailing-blank-lines
  :custom
  (org-log-done 'time)
  (org-directory "~/Org")
  (org-agenda-files
   '("~/Org"
     "~/Org/Personal"
     "~/Org/Work"))
  (org-default-notes-file "~/Org/Inbox.org")
  (org-archive-location "~/Org/Archive.org::* From %s")
  ;; Tell Org how to find the possible targets for refile.
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  ;; Tell Org to include the destination file as an element in the path to a
  ;; heading, and to use the full paths as completion targets rather than just
  ;; the heading text itself.
  (org-refile-use-outline-path 'file)
  ;; Tell Org to generate all of the possible completions and present them at
  ;; once.
  (org-outline-path-complete-in-steps nil)
  :config
  (defun my|common-org-hook ()
  "A common hook for `org-mode'."
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (C . t)
     (calc . t)
     (latex . t)
     (java . t)
     (ruby . t)
     (lisp . t)
     (scheme . t)
     (shell . t)
     (sqlite . t)
     (js . t)
     (haskell . t))))
  :hook
  ((org-mode . my|common-org-hook)))

;;;; Org activation bindings

;; Set up some global key bindings that integrate with Org Mode features.

(bind-key "C-c l" 'org-store-link)
(bind-key "C-c c" 'org-capture)
(bind-key "C-c a" 'org-agenda)

(setq org-todo-keywords
      '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED" "INVALID")))

;;;; Org agenda

;; The `org-agenda-files' was set in the host file.
;; See bottom of init.el

;;;; Org capture

(bind-key "C-c c" 'org-capture)

;; The `org-default-notes-file' was set in the host file.
;; See bottom of init.el
(setq org-capture-bookmark nil)

;;;; Org setup

;; Speed commands are a nice and quick way to perform certain actions
;; while at the beginning of a heading.  It's not activated by default.

;; See the doc for speed keys by checking out the documentation for
;; speed keys in Org mode.

(setq org-use-speed-commands t)
(setq org-image-actual-width 550)
(setq org-highlight-latex-and-related '(latex script entities))

;;;; Org tags

;; The default value is -77, which is weird for smaller width windows.
;; I'd rather have the tags align horizontally with the header.
;; 45 is a good column number to do that.

(setq org-tags-column 45)

;; I like to have source blocks properly syntax highlighted and with the
;; editing popup window staying within the same window so all the windows
;; don't jump around.  Also, having the top and bottom trailing lines in
;; the block is a waste of space, so we can remove them.

;; I noticed that fontification doesn't work with markdown mode when the
;; block is indented after editing it in the org src buffer --- the leading
;; #s for headers don't get fontified properly because they appear as Org
;; comments.  Setting `org-src-preserve-indentation' makes things
;; consistent as it doesn't pad source blocks with leading spaces.

(setq org-src-fontify-natively t
      org-src-window-setup 'current-window
      org-src-strip-leading-and-trailing-blank-lines t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t)

(provide 'langs-org)
;;; langs-org.el ends here
