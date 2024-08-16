;;; init.el --- Initialization file. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;; Keywords: configuration, misc

;; This file is NOT part of Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Welcome to my GNU Emacs haven.  This configuration is a reflection of my
;; desire for a minimalist yet powerful editing environment.
;;
;; I started this project on 4 March 2019 from this commit:
;; eb11ce25b0866508e023db4b8be6cca536cd3044

;;; Code:
;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/pre-custom.el
;;
;; The purpose of this file is for the user to define their
;; preferences BEFORE loading any of the modules.
(load (locate-user-emacs-file "pre-custom.el") :no-error :no-message)

;;;; Profiling and Debug
(defconst emacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, Emacs will be verbose.
Set DEBUG=1 in the command line or use --debug-init to enable this.")

;; Set the `debug-on-error' variable as per the runtime context:
;; - Enable debugging on error if Emacs is running in interactive mode, and the
;;   custom variable `emacs-debug-mode' is true.
;; - Do not enable debugging on error in non-interactive mode, regardless of the
;;   `emacs-debug-mode' value.
(setq-default debug-on-error (and (not noninteractive) emacs-debug-mode))

;; Measure the current start up time.
(add-hook
 'emacs-startup-hook
 #'(lambda ()
     (message "Emacs ready in %s with %d garbage collections."
              (format "%.2f seconds"
                      (float-time
                       (time-subtract after-init-time before-init-time)))
              gcs-done)))

;;;; Package management
(custom-set-variables
 ;; Setting up package archives.
 '(package-archives
   '(("melpa"    . "https://melpa.org/packages/")
     ("m-stable" . "https://stable.melpa.org/packages/")
     ("gnu"      . "https://elpa.gnu.org/packages/")
     ("nongnu"   . "https://elpa.nongnu.org/nongnu/")))
 ;; Priorities. Default priority is 0.
 '(package-archive-priorities
   '(("gnu"      . 100)
     ("nongnu"   . 50)
     ("m-stable" . 20)
     ("melpa"    . 10))))

;; Precompute activation actions to speed up startup.
(setq package-quickstart t)

;; Manually initialize packages in daemon or noninteractive mode.
;; `esup' need call `package-initialize'
;; For more see URL `https://github.com/jschaf/esup/issues/84'
(when (or (featurep 'esup-child)
          (daemonp)
          noninteractive)
  (package-initialize))

;; Package management in Emacs can be done in several ways. I personally like
;; `use-package' together with package.el. Some will prefer straight.el, but I
;; haven't found the need for it yet.
(eval-when-compile
  (setq use-package-enable-imenu-support t)
  (unless (ignore-errors (require 'use-package))
    ;; This is a seldomly-run part of my configuration, as `use-package' is
    ;; installed on Emacs' first run.
    (require 'package)
    (package-refresh-contents)
    (package-install 'use-package)
    ;; Only on the first run will all packages configured within this file be
    ;; ensured. This speeds up subsequent startups quite nicely.
    (setq use-package-always-ensure t)
    (require 'use-package)))

(require 'bind-key)

;;;; Common functions
(defun my-ensure-directory-exists (dir)
  "Ensure that the directory DIR exists, create it if it doesn't."
  (unless (file-exists-p dir)
    (make-directory dir t)))

;;;; Backup
;; Silently deletes excess backup versions.
(setq delete-old-versions t)

;; Make numeric backup versions unconditionally.
(setq version-control t)

;; Make backup files even in version controlled directories.
(setq vc-make-backup-files t)

;; Keep all backups in one directory.
(let ((my-backup-dir (concat user-emacs-directory "backup/")))
  (setq backup-directory-alist
        `(("." . ,(file-name-as-directory my-backup-dir))))
  (my-ensure-directory-exists my-backup-dir))

;;;; Auto-Saving
(let ((save-dir (concat user-emacs-directory "auto-save-list/")))
  (setq
   auto-save-file-name-transforms `((".*" ,(expand-file-name "\\2" save-dir) t))
   auto-save-list-file-name
   (concat save-dir (format ".saves-%d-%s~" (emacs-pid) (system-name))))
  (my-ensure-directory-exists save-dir))

;; Save point position between sessions
(use-package saveplace
  :unless noninteractive
  :demand 2
  :init
  (save-place-mode t)) ; Automatically save place in each file.

;;;; History
(use-package savehist
  :unless noninteractive
  :demand 2
  :custom
  ;; The default value is typically lower, but increasing it allows for a more
  ;; comprehensive history, which can be beneficial when needing to recall or
  ;; reuse previous inputs across sessions.
  (history-length 1000)
  ;; Enable the automatic deletion of duplicate entries from history.  By
  ;; default, this is disabled, but enabling it helps in keeping the history
  ;; clean and more manageable, especially when frequently reusing the same
  ;; inputs.
  (history-delete-duplicates t)
  :init
  (savehist-mode t))

(use-package recentf
  :unless noninteractive
  :defer 2
  :custom
  (recentf-max-saved-items 100)
  (recentf-keep '(recentf-keep-default-predicate file-remote-p file-readable-p))
  :config
  (recentf-mode)
  (when (fboundp 'recentf-save-list)
    (run-with-idle-timer (* 3 60) t #'recentf-save-list)))

;;;; Sane defaults
(use-package emacs
  :custom
  ;; Use tab key as completion option.
  (tab-always-indent 'complete)
  ;; No scratch message.
  (initial-scratch-message "")
  ;; Disable start-up screen.
  (inhibit-startup-screen t)
  ;; Configure the Scratch Buffer's Mode.
  (initial-major-mode 'text-mode)
  ;; Save custom variables in custom.el.
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  ;; Disable lockfiles on Windows, as they are a hassle.
  (create-lockfiles (not (member system-type '(windows-nt))))
  ;; No tabs - except for specific files, which Emacs can identify.
  (indent-tabs-mode nil)
  ;; Visually indicate empty lines after the buffer's end.
  (indicate-empty-lines t)
  ;; Redefine line and column format. It will looks like " 278:59 ".
  (mode-line-position '((line-number-mode ("%l" (column-number-mode ":%c")))))
  :hook
  ((text-mode prog-mode) . (lambda () (setq show-trailing-whitespace t)))
  :init
  ;; Show column number next to line number in mode line.
  (column-number-mode t)
  (when (or (file-exists-p custom-file) (file-symlink-p custom-file))
    (load custom-file t t)))

;; I use C source to understand and debug built-in functions.
(let ((src "~/src/emacs.git"))
  (when (or (file-directory-p src)(file-symlink-p src))
    (setq source-directory (expand-file-name (substitute-in-file-name src)))))

;;;; Emacs Server
(use-package server
  :config
  (or (server-running-p) (server-mode)))

;;;; Spell
;; Note: On macOs brew doesn't provide dictionaries.  So you have to install
;; them.  For more info on installing dictionaries see
;; URL `https://passingcuriosity.com/2017/emacs-hunspell-and-dictionaries'
(use-package ispell
  :if (executable-find "hunspell")
  :custom
  ;; Save personal dictionary without asking for confirmation.
  (ispell-silently-savep t)
  ;; Full path to the hunspell executable
  (ispell-program-name (executable-find "hunspell"))
  ;; The default dictionary I use.  To see available dictionaries
  ;; use 'hunspell -D'.
  (ispell-local-dictionary "en_US")
  ;; Setting up dictionary definitions
  (ispell-local-dictionary-alist
   '(("english" "[[:alpha:]]" "[^[:alpha]]" "[']" t
      ("-d" "en_US") nil utf-8)
     ("polish" "[[:alpha:]]" "[^[:alpha]]" "[']" t
      ("-d" "pl") nil utf-8)
     ("russian" "[А-Яа-я]" "[^А-Яа-я]" "[-']" nil
      ("-d" "russian-aot-ieyo") nil utf-8)))
  :config
  (setq ispell-really-aspell nil)
  (setq ispell-really-hunspell t))

(use-package flyspell
  :if (executable-find "hunspell")
  :custom
  ;; Be silent when checking words.
  (flyspell-issue-message-flag nil)
  :hook ((text-mode . flyspell-mode)
         (latex-mode . flyspell-mode))
  :bind ("C-x t s" . flyspell-mode))

(use-package writegood-mode
  :ensure t
  :hook ((text-mode . writegood-mode)
         (latex-mode . writegood-mode)))

;;;; Organization
(defconst my-org-dir
  (file-name-as-directory
   (concat (file-name-as-directory (expand-file-name "~")) "org"))
  "Path to the user org files directory.")

(use-package org
  :ensure t
  :mode ("\\.\\(org\\|org_archive\\)\\'" . org-mode)
  :custom
  ;; When opening an Org file, all top-level headers (first level headers) will
  ;; be collapsed. Since I use large files with a lot of headers, the default
  ;; value does not work for me
  (org-startup-folded t)
  ;; Do not initialize agenda Org files when generating (only) agenda.
  (org-agenda-inhibit-startup t)
  ;; When a TODO is set to a done state, record a timestamp
  (org-log-done (quote time))
  ;; Don' clutter the actual entry with notes.
  (org-log-into-drawer t)
  ;; Hide the markers so you just see bold text as BOLD and not *BOLD*
  (org-hide-emphasis-markers t)
  ;; Resize images to 600px, unless there's an attribute.
  (org-image-actual-width '(600))
  ;; Enable shift+arrow for text selection.
  (org-support-shift-select t)
  ;; Also include diary on org-agenda.
  (org-agenda-include-diary t)
  ;; Set up global org directory.
  (org-direcory (directory-file-name my-org-dir))
  ;; default target for storing notes.
  (org-default-notes-file (concat my-org-dir "inbox.org"))
  ;; Scan this dir for org files
  (org-agenda-files (list (directory-file-name my-org-dir)))
  ;; Undone TODO will block switching the parent to DONE
  (org-enforce-todo-dependencies t)
  ;; I customize this just to redefine 'agenda'
  (org-fold-show-context-detail
   '((agenda . lineage)
     (bookmark-jump . lineage)
     (isearch . lineage)
     (default . ancestors)))
  ;; Tags with fast selection keys.
  ;; For more see: http://orgmode.org/org.html#Setting-tags
  (org-tag-alist
   '(
     ("requirements" . ?r)
     ("issue" . ?i)
     (:startgroup)
     ("someday" . ?s)
     ("reward" . ?R)
     ("focus" . ?f)
     (:endgroup)))
  ;; Allow setting single tags without the menu
  (org-fast-tag-selection-single-key t)
  :config
  ;; Setup languages for org code blocks.  For full list of supported languages
  ;; see: https://orgmode.org/worg/org-contrib/babel/languages/index.html
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (emacs-lisp . t)
     (haskell . t)
     (js . t)
     (latex . t)
     (lisp . t)
     (makefile . t)
     (org . t)
     (python . t)
     (scheme . t)
     (shell . t)
     (sql .t)
     (calc . t)))

  (defun my|org-agenda-refresh-on-idle ()
    "Set up a timer to refresh Org Agenda buffers after 60s of idle time."
    (run-with-idle-timer 60 nil
                         (apply-partially #'org-agenda-prepare-buffers
                                          (org-agenda-files t t))))

  (my-ensure-directory-exists my-org-dir)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . org-display-inline-images)
         (org-agenda-mode . my|org-agenda-refresh-on-idle))
  :bind (("C-c c"       . #'org-capture)
         ("C-c a"       . #'org-agenda)
         ("C-c l"       . #'org-store-link)
         ("C-c r"       . #'org-refile)
         ("C-c C-x C-a" . #'org-archive-subtree)))

(use-package org-mobile
  :after org
  :custom
  ;; Setting up MobileOrg sync path
  (org-mobile-directory (expand-file-name "~/Dropbox/Apps/MobileOrg"))
  ;; Do not generate IDs for all headings
  (org-mobile-force-id-on-agenda-items nil)
  ;; Append captured notes and flags to ~/org/index.org
  (org-mobile-inbox-for-pull (concat my-org-dir "index.org"))
  :config
  (defvar my-org-mobile-sync-timer nil
    "Timer object for automatic Org Mobile sync on idle.")

  (defvar my-org-mobile-sync-idle-secs (* 60 10)
    "Number of idle seconds before triggering Org Mobile sync.")

  (defun my/org-mobile-sync ()
    "Synchronize Org Mobile."
    (interactive)
    (org-mobile-pull)
    (org-mobile-push))

  (defun my/org-mobile-sync-enable ()
    "Enable automatic synchronization of Org Mobile on idle."
    (interactive)
    (setq my-org-mobile-sync-timer
          (run-with-idle-timer my-org-mobile-sync-idle-secs t
                               'my-org-mobile-sync)))

  (defun my/org-mobile-sync-disable ()
    "Disable the automatic Org Mobile synchronization on idle."
    (interactive)
    (cancel-timer my-org-mobile-sync-timer))

  ;; Automatically enable idle sync.
  ;; Source: https://stackoverflow.com/q/8432108/1661465
  (my/org-mobile-sync-enable))

(use-package org-cliplink
  :ensure t
  :after org
  :bind ("C-x p i" . org-cliplink))

;; Define my default keywords as workflow states.
;; The command C-c C-t cycles an entry from 'TODO' to 'CANCELED'.
;; For details see: https://orgmode.org/manual/Workflow-states.html
(setq org-todo-keywords
      '((sequence
         ;; Need action
         "TODO(t)" "STARTED(s)" "WAITING(w@/!)" "SOMEDAY(S!)" "|"
         ;; Done
         "DONE(d!/!)" "CANCELLED(c@/!)")))

;; Define the style of the keywords
(setq org-todo-keyword-faces
      '(("TODO"      :foreground "firebrick1"   :weight bold)
        ("NEXT"      :foreground "firebrick1"   :weight bold)
        ("STARTED"   :foreground "red3"         :weight bold)
        ("DONE"      :foreground "forest green" :weight bold)
        ("WAITING"   :foreground "orange"       :weight bold)
        ("SOMEDAY"   :foreground "magenta"      :weight bold)
        ("CANCELLED" :foreground "lime green"   :weight bold)))

(defconst my-capture-template-simple
  "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
  "A shortcut for a simple heading with keyword TODO.")

;; Define custom Org Capture templates
(setq org-capture-templates
      `(("b" "Bookmark" entry
         (file+headline ,(concat my-org-dir "notes.org") "Bookmarks")
         "* %x%?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        ("n" "Random Note" entry
          (file+headline ,(concat my-org-dir "notes.org") "Random Notes")
         "** %?\n  %U" :empty-lines 1)
        ("i" "Blog Idea" entry
         (file+headline ,(concat my-org-dir "blog.org") "Blog Ideas")
         "** %?\n  %U" :empty-lines 1)
        ("w" "Work Log Entry" entry
         (file+datetree ,(concat my-org-dir "work-log.org"))
         "* TODO %?  :work:" :empty-lines 1)
        ("p" "Personal Tasks" entry
         (file+datetree ,(concat my-org-dir "personal-tasks.org"))
         "* TODO %?  :personal:" :empty-lines 1)
        ("r" "To-Read" checkitem
         (file+headline ,(concat my-org-dir "later.org") "To-Read List")
         "- [ ] %?  :read:" :empty-lines 1)
        ("f" "To-Watch" checkitem
         (file+headline ,(concat my-org-dir "later.org") "To-Watch List")
         "- [ ] %?  :watch:" :empty-lines 1)
        ("t" "Trip Checklist" checkitem
         (file+headline ,(concat my-org-dir "trips.org") "Trip Checklist"))))

;; Define custom Org Agenda commands
(setq org-agenda-custom-commands
      '(("w" "Work Entries" tags-todo "work")
        ("p" "Personal Tasks" tags-todo "personal")
        ("r" "Read Later" tags-todo "read")
        ("f" "Watch Later" tags-todo "watch")))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)

(defun my-org-switch-to-buffer (filename)
  "Switch to the buffer associated with the given Org FILENAME.

If a buffer visiting FILENAME is already open, switch to it.  If
the buffer does not exist, open the FILENAME from the `~/org/`
directory and switch to the newly created buffer.

FILENAME should be the name of the Org file without any directory
path.  The file is expected to reside in the `~/org/` directory.
"
  (switch-to-buffer
   (or (get-buffer filename)
       (find-file-noselect (concat my-org-dir filename)))))

(defun my/org-move-bookmark-to-notes()
  "Move and format bookmark heading from 'index.org' to 'notes.org'.

This function is specifically designed for a workflow where notes
and bookmarks are synchronized from MobileOrg (e.g., from
'mobileorg.org' file) into a catch-all Org file like '~/org/index.org'.

- Remove the keywords 'TODO' and 'Bookmark' from the heading.
- Convert the heading to a second-level heading.
- Insert a creation date property under the heading.
- Move the cleaned and formatted heading to the end of 'notes.org'.

This function is adapted from an implementation by Karl Voit, and
has been reworked to enhance its readability, maintainability,
and alignment with my specific workflow.

For origin see: https://karl-voit.at/2014/08/10/bookmarks-with-orgmode/"
  (interactive)
  (save-excursion
    ;; Step 1: Save the starting point and narrow to the current heading
    (beginning-of-line)
    (let ((start (point)))
      (outline-next-visible-heading 1)
      (save-restriction
        (narrow-to-region start (point))

        ;; Step 2: Clean up heading by removing unwanted keywords
        (goto-char (point-min))
        (while (re-search-forward "\\* \\(TODO \\)?\\(Bookmark \\)?" nil t)
          (replace-match "* " nil nil))

        ;; Step 3: Ensure the heading is at the second level
        (goto-char (point-min))
        (when (looking-at "\\*+")
          (let ((current-level (- (match-end 0) (match-beginning 0))))
            (cond
             ; if level 1, add one more "*"
             ((= current-level 1) (insert "*"))
             ; if level > 2, remove excess "*"
             ((> current-level 2) (delete-char (- current-level 2))))))

        ;; Step 4: Insert creation date properties
        (search-forward-regexp "^\\[20")
        (beginning-of-line)
        (insert ":PROPERTIES:\n:CREATED: ")
        (end-of-line)
        (newline)
        (insert ":END:\n")

        ;; Step 5: Cut the current heading and move it to the notes.org file
        (kill-region start (point))
        (my-org-switch-to-buffer "notes.org")
        (end-of-buffer)
        (newline)
        (yank)  ;; Paste the heading into notes.org

        ;; Step 6: Reapply tags to the previous heading in notes.org
        (outline-previous-visible-heading 1)
        (org-set-tags-command)))))

;;;; Window Handlin
;; Restore old window configurations
(use-package winner
  :commands (winner-undo winner-redo)
  :custom
  ;; List of buffer names whose windows `winner-undo' will not restore.
  (winner-boring-buffers
   '("*Completions*" "*Compile-Log*" "*Apropos*" "*Help*"
     "*Buffer List*" "*Ibuffer*" "*Messages*"))
  :hook (after-init . winner-mode))

;;;; Appearance
;; I prefer the box cursor.
(setq-default cursor-type 'box)

(defun my-theme-dark-p ()
  "Return t if the current Emacs theme is dark."
  (let* ((bg-color (face-background 'default nil 'default))
         (bg-rgb (color-values bg-color))
         (brightness (apply '+ (mapcar (lambda (x) (/ x 65535.0)) bg-rgb))))
    (< brightness 1.5)))

(defun my-set-cursor-color-based-on-theme ()
  "Set cursor color based on whether the theme is dark or light."
  (if (my-theme-dark-p)
      (custom-set-faces
       '(cursor ((t (:background "#F6CB47")))))  ;; Orange
    (custom-set-faces
     '(cursor ((t (:background "#000000")))))))  ;; Black

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(add-hook 'after-load-theme-hook 'my-set-cursor-color-based-on-theme)

;; Modus themes
(use-package modus-themes
  :demand t
  :init
  (load-theme 'modus-vivendi :no-confirm)
  (my-set-cursor-color-based-on-theme)
  :bind (("<f5>" . modus-themes-toggle)
         ("C-<f5>" . consult-theme)))

;; Nicer scrolling
(when (>=  emacs-major-version 29)
  (pixel-scroll-precision-mode 1))

(defun my/terminal-visible-bell ()
   "A friendlier visual bell effect."
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

;; Just blink the modeline on errors.
(setq ring-bell-function #'my/terminal-visible-bell)

;; Highlight matching parentheses when the point is on them.
(add-hook 'after-init-hook #'show-paren-mode)

;; Highlight brackets according to their depth.
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;; I prefer not to have any of the GUI elements.  This keeps the window clean
;; and speeds up loading a bit.
(setq-default use-file-dialog nil)
(setq-default use-dialog-box nil)

;; Show line numbers
(use-package display-line-numbers
  :custom (display-line-numbers-width 4)
  :hook (prog-mode . display-line-numbers-mode))

(use-package elec-pair
  :hook
  (after-init . electric-pair-mode)
  (minibuffer-setup . (lambda () (electric-pair-local-mode 0))))

;;;; Editing
(use-package outline
  :custom
  ;; Have a blank line before a heading.
  (outline-blank-line t)
  :commands (outline-mode outline-minor-mode)
  :diminish outline-minor-mode
  :hook (prog-mode . outline-minor-mode))

;; Delete trailing whitespace on save in all modes.  If there are modes or
;; projects where trailing whitespace should be retained, it is suggested to
;; override this setting on a per-mode or per-project basis by removing
;; `delete-trailing-whitespace' from `before-save-hook' locally or by using
;; directory-local variables to change the behavior as needed.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Ensure a final newline on save in all modes.  If there are modes or projects
;; where a final newline should not be added, it is suggested to override this
;; setting on a per-mode or per-project basis by removing the local setting for
;; `require-final-newline' or by using directory-local variables to change the
;; behavior as needed.
(setq-default require-final-newline t)

;;;; Project management
(use-package project
  :commands (project-find-file
             project-switch-to-buffer
             project-switch-project
             project-switch-project-open-file)
  :bind (:map project-prefix-map
              ("p" . my-project-switch-project)
              ("s" . my-switch-project-and-kill-buffers)
              ("C-b" . my-project-list-buffers)
              ("R" . project-remember-projects-under)
              ("K" . project-kill-buffers))
  :config
  ;; Auto clean up zombie projects from `project-list-file'
  (run-at-time "07:00pm" (* 24 60 60) 'project-forget-zombie-projects)
  ;; Use ripgrep if installed
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  (declare-function project-prompt-project-dir "project")
  (defun my-project-switch-project (dir)
    "\"Switch\" to another project by running an Emacs command.
Directly use `project-find-file' instead of getting prompted.

When called in a program, it will use the project corresponding
to directory DIR."
    (interactive (list (project-prompt-project-dir)))
    (let ((project-current-directory-override dir))
      (project-find-file)))

  (defun my-switch-project-and-kill-buffers ()
    "Kill all buffers of the current project, then switch to a new project."
    (interactive)
    (project-kill-buffers t)
    (call-interactively 'my-project-switch-project))

  (defun my-project-list-buffers ()
    "Display a list of buffers associated with the current project.

This function filters the list to show only file-visiting buffers.
After generating the buffer list, it pops up the `*Buffer List*` buffer
in a new window, allowing you to easily navigate through the buffers
related to your current project."
    (interactive)
    (project-list-buffers t)
    (pop-to-buffer "*Buffer List*")))

;;;; Security
(use-package epg
  :custom
  (epg-gpg-program "gpg"))

(use-package epa
  :after epg
  :init
  ;; For more see "man 1 gpg" for the option "--pinentry-mode"
  (unless (eq (window-system) 'w32)
    (custom-set-variables '(epg-pinentry-mode 'loopback)))
  :config
  ;; Enable automatic encryption/decryption of *.gpg files
  (unless (memq epa-file-handler file-name-handler-alist)
    (epa-file-enable)))

(use-package auth-source
  :custom
  (auth-sources
   `(,(concat user-emacs-directory ".authinfo.gpg")
     "~/.authinfo" "~/.authinfo.gpg")))

;;;; VCS
(use-package git-modes
  :ensure t
  :mode (("/\\.gitattributes\\'" . gitattributes-mode)
         ("/\\.git\\(config\\|modules\\)\\'" . gitconfig-mode)
         ("/\\.\\(git\\|docker\\|elpa\\)ignore\\'" . gitignore-mode)))

(use-package magit
  :ensure t
  :after transient
  :commands (magit magit-status)
  :bind (("C-x g" . magit-status)))

;;;; Setup completion

;; Provide a nicer `completing-read'.
(use-package vertico
  :ensure t
  :custom
  ;; Show more candidates in the minibuffer
  (vertico-count 12)
  ;; Enable cycling through candidates
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Enable Consult for enhanced command completion.  Consult provides
;; replacements for many standard commands, offering better interface and
;; additional features.
(use-package consult
  :ensure t
  :bind (("C-x C-i" . consult-imenu)
         ("C-x l"   . consult-locate)
         ("C-x b"   . consult-buffer)
         ("C-x B"   . consult-buffer-other-window)
         ("C-c k"   . consult-ripgrep)
         ("C-c f"   . consult-recent-file)
         ("C-r"     . consult-history)
         ("C-S-s"   . consult-line)
         :map minibuffer-local-map
         ("C-r"     . consult-history)))

;; Init flyspell-correct for spell correction
(use-package flyspell-correct
  :ensure t
  :defer t
  :after flyspell)

;; Completion of misspelled words in buffer.
(use-package consult-flyspell
  :ensure t
  :after (consult flyspell-correct)
  :bind (:map flyspell-mode-map
              ("C-;" . consult-flyspell))
  :config
  (setq consult-flyspell-select-function 'flyspell-correct-at-point))

;; Add additional context and annotations to completion UI.  Marginalia enriches
;; the completion interface with more information, such as documentation
;; strings, file sizes, etc.
(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

;; Enable Orderless for flexible matching style.  Orderless provides advanced
;; matching capabilities that allow you to combine multiple search patterns.
(use-package orderless
  :ensure t
  :custom
  ;; Use orderless as the default completion style
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  ;; Adjust completion for specific contexts, e.g., file paths
  (completion-category-overrides '((file (styles basic partial-completion))))
  :config
  ;; Ensure case-insensitive matching for all completions
  (setq completion-ignore-case t))

;; Enable Embark for additional actions within completion UI.  Embark adds the
;; ability to perform actions directly from completion buffers (e.g.,
;; minibuffer, `completing-read').
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)           ;; Act on selected completion item
   ("C-;" . embark-dwim)          ;; Do-What-I-Mean (e.g., open or describe)
   :map minibuffer-local-map
   ("C-c C-o" . embark-export)    ;; Export candidates to another buffer
   ("C-c C-c" . embark-collect))) ;; Collect and act on multiple candidates

;; Prescient for improved sorting and filtering.  Prescient enhances sorting and
;; filtering of candidates based on your history and frequently used items.
(use-package prescient
  :ensure t
  :custom
  ;; The default settings seem a little forgetful to me. Let's try
  ;; this out.
  (prescient-history-length 1000)
  ;; Common sense.
  (prescient-sort-full-matches-first t)
  :config
  ;; Remember usage statistics across Emacs sessions.
  (prescient-persist-mode +1))

;; Enable Vertico Prescient integration.  Integrate Prescient with Vertico to
;; combine powerful sorting and filtering with the Vertico completion system.
(use-package vertico-prescient
  :ensure t
  :after (vertico prescient)
  :init (vertico-prescient-mode 1)
  :config (prescient-persist-mode 1))

;; Enable CTRLF as a modern replacement for Isearch and Swiper.  CTRLF provides
;; a modernized interface for incremental search, offering more intuitive
;; navigation and search options.
(use-package ctrlf
  :ensure t
  :init
  (ctrlf-mode 1))

;;;; IRC and other communication
(use-package erc
  :after auth-source
  :commands (erc erc-tls)
  :custom
  (erc-autojoin-channels-alist '(("Libera.Chat" "#emacs" "#org-mode" "#re2c")))
  (erc-autojoin-timing 'ident)
  (erc-user-full-name user-full-name)
  (erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-password nil)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  :config
  (defun my|setup-erc-modules ()
    "Set up modules for ERC."
    (add-to-list 'erc-modules 'log)
    (erc-update-modules))
  :hook ((erc-mode . my|setup-erc-modules)))

(use-package erc-log
  :after erc
  :custom
  (erc-log-channels-directory #'my-erc-monthly-log-directory)
  (erc-generate-log-file-name-function #'my-erc-log-file-name-short)
  (erc-log-insert-log-on-open t)
  (erc-save-buffer-on-part nil)
  (erc-save-queries-on-quit nil)
  (erc-log-write-after-insert t)
  (erc-log-write-after-send t)
  :config
  (defconst my-erc-base-log-directory
    (concat (expand-file-name "~") "/logs/erc/")
    "The base directory where ERC logs will be stored.

This directory serves as the root for all ERC logs, with further
subdirectories being created for specific periods. The log files
are organized by year and month to ensure easy access and
management of chat history.")

  (defun my-erc-monthly-log-directory (&rest _ignore)
    "Return the directory path for storing ERC logs for the current month.

This function generates a directory path based on the current
year and month, ensuring that logs are organized chronologically.
If the directory does not already exist, it will be created.
The resulting path is of the form: ~/logs/erc/YYYY/MM/."
    (let ((directory (concat my-erc-base-log-directory (format-time-string "%Y/%m"))))
      (my-ensure-directory-exists directory)
      directory))

  (defun my-erc-log-file-name-short (buffer target nick server port)
    "Computes a log file name from the TARGET and SERVER only.

This results in a filename of the form #channel@server.txt, for example:
#emacs@irc.libera.chat.txt."
    (let ((file (concat target "@" server ".txt")))
      (convert-standard-filename file))))

(use-package erc-services
  :after erc
  :custom
  (erc-prompt-for-nickserv-password nil)
  (erc-use-auth-source-for-nickserv-password t)
  :config
  (erc-services-mode 1))

(use-package erc-spelling
  :after erc
  :config
  (erc-spelling-mode 1))

(use-package erc-track
  :after erc
  :custom
  (erc-track-exclude-types
   '("JOIN" "MODE" "NICK" "PART" "QUIT"
     "301" ; away notice
     "305" ; return from awayness
     "306" ; set awayness
     "324" ; modes
     "329" ; channel creation date
     "332" ; topic notice
     "333" ; who set the channel topic
     "353" ; listing of users on the current channel
     "477")))

(use-package erc-hl-nicks
  :ensure t
  :after erc)

(defun my/erc-buffer-connected-p (buffer)
  "Check if ERC BUFFER is connected."
  (with-current-buffer buffer
    (and (erc-server-process-alive)
         erc-server-connected)))

(declare-function erc-track-switch-buffer (arg))
(defun my/erc-start-or-switch ()
  "Connects to ERC, or switch to last active buffer.

This function serves multiple purposes:

1. Check Active Buffers: It iterates through a predefined list of ERC buffers
   to determine if any of them are actively connected to an IRC server.

2. Verify Connection Status: For each buffer, it checks whether the associated
   ERC process is alive and whether there is an established network connection
   to the server. This is done using the `erc-server-process-alive' function and
   the `erc-server-connected' variable.

3. Switch to Active Buffer: If any buffer is found to be actively connected,
   the function switches to that buffer using `erc-track-switch-buffer'.

4. Reconnect if Disconnected: If none of the checked buffers are connected,
   the function prompts the user to reconnect to the IRC server. If the user
   confirms, a new connection is initiated using the `erc' command with the
   server and port specified (`irc.libera.chat` on port 6667)."
  (interactive)
  (let ((erc-buffers '("Libera.Chat" "irc.libera.chat" "irc.libera.chat:6667"))
        (connected nil))
    (dolist (buffer erc-buffers)
      (when (and (get-buffer buffer)
                 (my/erc-buffer-connected-p buffer))
        (setq connected t)))
    (if connected
        (erc-track-switch-buffer 1)
      (when (y-or-n-p "Start ERC? ")
        (erc :server "irc.libera.chat" :port 6667)))))

(global-set-key (kbd "C-c e f") #'my/erc-start-or-switch)

;;;; Shell
(use-package eshell
  :commands (eshell-mode eshell)
  :custom
  ;; Always scrolls the window to the end when entering a new command
  (eshell-scroll-to-bottom-on-input t)
  ;; Scrolls the window to the end when new output appears
  (eshell-scroll-to-bottom-on-output 'all)
  :bind
  (("M-s e" . eshell)))

;;;; Programming Languages, Markup and Configurations
(use-package css-mode
  :mode "\\.css$"
  :custom
  (css-indent-offset 2))

(use-package js
  :mode (("\\.js\\'" . js-mode))
  :custom
  (js-indent-level 2))

(use-package rainbow-mode
  :ensure t
  :defer t
  :hook css-mode)

(use-package yaml-mode
  :ensure t
  :mode "\\.ya?ml\\'")

(use-package rst
  :mode ("\\.rst\\'" . rst-mode)
  :custom
  ;; This setting is used to automate the creation of section adornments in rst
  ;; files.  When a new section is created, `rst-new-adornment-down' ensures the
  ;; adornment level is one level lower than the previous section, rather than
  ;; keeping the same level.
  (rst-new-adornment-down t)
  :hook ((rst-mode . visual-line-mode)))

(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode))
  :hook ((markdown-mode . visual-line-mode)))

(use-package sql
  :mode ("\\.sql\\'" . sql-mode)
  :bind (("M-s s" . sql-sqlite)))

(use-package sql-indent
  :ensure t
  :hook ((sql-mode . sqlind-minor-mode)))

(use-package sqlite-mode
  :bind (("M-s q" . sqlite-mode-open-file)
         :map sqlite-mode-map
         ("M-K" . sqlite-mode-list-tables)
         ("TAB" . sqlite-mode-list-data)))

(use-package csv-mode
  :ensure t
  :mode (("\\.csv\\'" . csv-mode))
  :hook ((csv-mode . csv-align-mode)))

(use-package python
  :defer t
  :custom
  (python-shell-interpreter "python3")
  (python-shell-interpreter-args "-i --simple-prompt --pprint"))

(use-package anaconda-mode
  :ensure t
  :after python
  :hook ((python-mode . anaconda-mode)
         (python-mode . anaconda-eldoc-mode)))

;;;;; Lisp and company
(defun my|lisp-modes-setup ()
  "Custom configurations for Lisp modes."
  (turn-on-eldoc-mode)
  (rainbow-delimiters-mode)

  ;; Disable optimization for finding previous index positions.
  ;; Ensures custom imenu expressions work correctly.
  (setq-local imenu-prev-index-position-function nil)

  ;; Disable custom index name extraction.
  ;; Allows using the full heading text in imenu without modification.
  (setq-local imenu-extract-index-name-function nil)

  ;; Set the default function for creating the imenu index.
  ;; Ensures our customizations integrate with the standard index creation process.
  (setq-local imenu-create-index-function 'imenu-default-create-index-function)

  ;; Disable skipping of comments and strings when creating the index.
  ;; Ensures that headings in comments are included in imenu.
  (setq-local imenu-generic-skip-comments-and-strings nil)

  (add-to-list
   'imenu-generic-expression
   '("Sections" "^\\(;;[;]\\{1,8\\}[\s\t]+\\)\\(.*\\)$" 2) t))

(add-hook 'emacs-lisp-mode-hook #'my|lisp-modes-setup)
(add-hook 'lisp-interaction-mode-hook #'my|lisp-modes-setup)

(define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)

;;;; Helpers
(use-package which-key
  :ensure t
  :defer 1
  :config
  (which-key-mode 1))

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; ~/.emacs.d/post-custom.el
;;
;; The purpose of the "post customisations" is to make tweaks to what
;; I already define, such as to change the default theme.  See above
;; for the `pre-custom.el' to make changes BEFORE loading any of my
;; other configurations.
(load (locate-user-emacs-file "post-custom.el") :no-error :no-message)

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:

;;; init.el ends here
