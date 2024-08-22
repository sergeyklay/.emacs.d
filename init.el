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
;; `~/.emacs.d/pre-custom.el'
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
 (lambda ()
   (message "Emacs ready in %s with %d garbage collections."
            (format "%.2f seconds"
                    (float-time
                     (time-subtract after-init-time before-init-time)))
            gcs-done)))

;;;; Package management
;; Setting up package archives.
(setq package-archives
   '(("melpa"    . "https://melpa.org/packages/")
     ("m-stable" . "https://stable.melpa.org/packages/")
     ("gnu"      . "https://elpa.gnu.org/packages/")
     ("nongnu"   . "https://elpa.nongnu.org/nongnu/")))

;; Priorities. Default priority is 0.
(setq package-archive-priorities
      '(("gnu"      . 100)
        ("nongnu"   . 50)
        ("m-stable" . 20)
        ("melpa"    . 10)))

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
;; `use-package' together with `package.el'. Some will prefer straight.el, but I
;; haven't found the need for it yet.
(eval-when-compile
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

;; Create and setup my own keyboard map.
;; For details see: https://elpa.gnu.org/packages/bind-key.html
(bind-keys
 :prefix-map my-keyboard-map
 :prefix-docstring "My own keyboard map"
 :prefix "C-c C-/"
 ("-" . text-scale-decrease)
 ("+" . text-scale-increase)
 ;; because "+" needs "S-=" and I might forget to press shift
 ("=" . text-scale-increase))

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

;; Enable `save-place-mode' to save point position in files across sessions.
;; This feature is extremely useful when working on large projects or when
;; you frequently switch between different files. It ensures that you return
;; to the exact spot where you left off, enhancing workflow efficiency.

;; First, ensure that the `saveplace' package is available, as it is required
;; for this functionality.
(require 'saveplace)

;; Enable the `save-place-mode' globally, which activates this feature for all
;; buffers by default.
(save-place-mode t)

;;;; History
;; Enable `savehist-mode' to preserve the minibuffer history across sessions.
;; This is particularly useful for recalling previous commands, search terms,
;; or any other input you may want to reuse across Emacs sessions.

;; First, ensure that the `savehist' package is available, as it is required
;; for this functionality.
(require 'savehist)

;; Set the maximum number of entries to save in the history. A larger value
;; allows for a more extensive history, which can be handy if you often need
;; to recall older entries.
(setq history-length 1000)

;; Enable the removal of duplicate entries in the history. This keeps your
;; history cleaner and more manageable, avoiding clutter from repeated entries.
(setq history-delete-duplicates t)

;; Enable `savehist-mode', which automatically saves the minibuffer history
;; to a file and loads it on startup, preserving your history between sessions.
(savehist-mode t)

;; Enable `recentf-mode' to keep track of recently opened files.
;; This is a very useful feature that allows you to quickly access files you
;; have worked on recently. The list of recent files is updated automatically.
(when (not noninteractive)
  ;; First, ensure that the `recentf' package is available, as it is required
  ;; for this functionality.
  (require 'recentf)

  ;; Set the maximum number of recent files to save. This limits the list
  ;; to the most recent 100 files, which is a good balance between utility
  ;; and performance.
  (setq recentf-max-saved-items 100)

  ;; Customize the conditions under which files are kept in the recent list.
  ;; Here, we keep files that match the default predicate, are remote files,
  ;; or are readable.
  (setq recentf-keep
        '(recentf-keep-default-predicate
          file-remote-p file-readable-p))

  ;; Enable `recentf-mode', which automatically tracks recently opened files.
  (recentf-mode t)

  ;; To ensure the recentf list is periodically saved during idle time, we
  ;; check if the `recentf-save-list' function is available and then set up
  ;; a timer to run it every 3 minutes.
  (when (fboundp 'recentf-save-list)
    (run-with-idle-timer (* 3 60) t #'recentf-save-list)))

;;;; Sane defaults

;; Use the tab key for both indentation and completion. This allows for more
;; intuitive behavior when working in various modes, especially in text and
;; programming modes.
(setq tab-always-indent 'complete)

;; Remove the initial message from the scratch buffer.
(setq initial-scratch-message "")

;; Disable the startup screen, allowing for a quicker start directly into
;; your working environment.
(setq inhibit-startup-screen t)

;; Set the initial major mode of the scratch buffer to text-mode.
(setq initial-major-mode 'text-mode)

;; Disable the creation of lockfiles on Windows, as they can cause issues
;; with file systems on that platform.
(setq create-lockfiles (not (member system-type '(windows-nt))))

;; Use spaces instead of tabs for indentation, except in cases where
;; Emacs specifically recognizes that tabs are required.
(setq-default indent-tabs-mode nil)

;; Visually indicate empty lines after the buffer's end. This helps in
;; identifying where the actual content of the file ends.
(setq-default indicate-empty-lines t)

;; Customize the format of the mode line to show line and column numbers
;; in the format "278:59".
(setq mode-line-position '((line-number-mode ("%l" (column-number-mode ":%c")))))

;; Enable column number mode to display column numbers in the mode line.
(column-number-mode t)

;; Highlight trailing whitespace in text-mode and prog-mode.
(add-hook 'text-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))

;; Set up source-directory for C source files, used to understand and debug
;; built-in Emacs functions. This is especially useful for my, who need to
;; dive into the internals of Emacs.
(let ((src (expand-file-name "~/src/emacs.git")))
  (when (and (file-directory-p src)
             (not (file-remote-p src)))
    (setq source-directory src)))

;;;; Emacs Server
;; Enable Emacs server mode, allowing Emacs to act as a server for external
;; clients. This is especially useful for quickly opening files from the
;; command line with 'emacsclient' or integrating with other tools.

;; Ensure the `server' package is available, as it is necessary for running
;; Emacs in server mode.
(require 'server)

;; Check if the server is already running. If not, start the server mode.
;; This prevents errors when trying to start the server multiple times.
(unless (server-running-p)
  (server-mode t))

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

;;;; Security
(defconst my-gpg-program
  (cond
   ((executable-find "gpg") "gpg")
   ((executable-find "gpg2") "gpg2")
   (t nil))
  "Path to the GPG program to use.
If neither 'gpg' nor 'gpg2' is found, this is set to nil.")

;; Check if GPG is available, then set `epg-gpg-program',
;; otherwise display a warning.
(if my-gpg-program
    (setq epg-gpg-program my-gpg-program)
  (warn (concat "Neither 'gpg' nor 'gpg2' is available. "
                "Encryption and decryption features will not be available.")))

;; Initialize epa settings
(unless (eq (window-system) 'w32)
  ;; Set pinentry mode to 'loopback' for all systems except Windows.
  ;; For more see "man 1 gpg" for the option "--pinentry-mode".
  (setq epg-pinentry-mode 'loopback))

;; Specify the key to use for file encryption
;; Also used for `org-crypt-key' (see bellow).
(setq epa-file-encrypt-to '("1E0B5331219BEA88"))

;; Enable automatic encryption/decryption of *.gpg files
(require 'epa-file)

;; Only enable `epa-file` if it's not already enabled
(unless (memq epa-file-handler file-name-handler-alist)
  (epa-file-enable))

(require 'auth-source)

;; Cleanup original value of authentication sources to
;; use only password-store (sell bellow).
(setq auth-sources '())

(use-package password-store
  :ensure t
  :defer 5
  :commands (password-store-insert
             password-store-copy
             password-store-get))

;; See https://www.passwordstore.org/
(use-package pass
  :ensure t
  :after password-store
  :commands (pass pass-view-mode))

(add-to-list 'auto-mode-alist
             '("\\<password-store\\>/.*\\.gpg\\'" . pass-view-mode)

(require 'auth-source-pass)

; Enable extra query keywords for auth-source-pass
(setq auth-source-pass-extra-query-keywords t)

;; Enable `auth-source-pass' to use pass for `auth-sources'.
(auth-source-pass-enable)

;;;; Organization
(defconst my-org-files-path
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
  ;; When a TODO is set to a done state, record a timestamp
  (org-log-done '(time))
  ;; Don' clutter the actual entry with notes.
  (org-log-into-drawer t)
  ;; Hide the markers so you just see bold text as BOLD and not *BOLD*
  (org-hide-emphasis-markers t)
  ;; Resize images to 600px, unless there's an attribute.
  (org-image-actual-width '(600))
  ;; Enable shift+arrow for text selection.
  (org-support-shift-select t)
  ;; Set up global org directory.
  (org-direcory (directory-file-name my-org-files-path))
  ;; Undone child's TODO will block switching the parent to DONE.
  (org-enforce-todo-dependencies t)
  ;; I customize this just to redefine 'agenda'
  (org-fold-show-context-detail
   '((agenda . lineage)
     (bookmark-jump . lineage)
     (isearch . lineage)
     (default . ancestors)))
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
  (my-ensure-directory-exists my-org-files-path)
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . org-display-inline-images)))

(global-set-key (kbd "C-c l") #'org-store-link)

;;;;; Helpers
(defun my-org-get-created-date ()
  "Retrieve the CREATED property if it exists, otherwise return nil.

This function utilizes Org-mode's built-in date parsing
capabilities by using `org-parse-time-string' to handle various
date formats.  If no valid date is found, it returns nil."
  (let* ((created (org-entry-get nil "CREATED" t))
         (parsed-date (and created (org-parse-time-string created))))
    (when (and parsed-date
               ;; Ensure that year, month, and day are all present
               (nth 4 parsed-date)
               (nth 5 parsed-date)
               (nth 3 parsed-date))
      (format "%04d-%02d-%02d"
              (nth 5 parsed-date)  ;; Year
              (nth 4 parsed-date)  ;; Month
              (nth 3 parsed-date)))))

(defun my-org-sanitize-heading-for-id (heading)
  "Sanitize the HEADING text to create a slug suitable for use as an ID.

Removes progress indicators, priority markers, links, timestamps,
non-alphanumeric characters and replaces spaces with hyphens."
  (let ((slug heading))
    ;; Replacements for common symbols
    (setq slug (replace-regexp-in-string "&" "and" slug))
    (setq slug (replace-regexp-in-string "\\." "dot" slug))
    (setq slug (replace-regexp-in-string "\\+" "plus" slug))
    ;; Remove progress indicators like "[25%]" or "[2/7]"
    (setq slug (replace-regexp-in-string "\\(\\[[0-9]+%\\]\\)" "" slug))
    (setq slug (replace-regexp-in-string "\\(\\[[0-9]+/[0-9]+\\]\\)" "" slug))
    ;; Remove priority indicators like "[#A]"
    (setq slug (replace-regexp-in-string "\\(\\[#[ABC]\\]\\)" "" slug))
    ;; Remove links but keep their descriptions
    (setq slug (replace-regexp-in-string "\\[\\[\\(.+?\\)\\]\\[" "" slug t))
    ;; Remove timestamps (active and inactive)
    (setq slug (replace-regexp-in-string
                "<[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)?>"
                "" slug t))
    (setq slug (replace-regexp-in-string
                "\\[[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)\\]"
                "" slug t))
    ;; Replace spaces with hyphens and remove non-alphanumeric characters
    (setq slug (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" slug))
    (replace-regexp-in-string " +" "-" slug)
    (setq slug (replace-regexp-in-string " +" "-" slug))
    ;; Remove trailing hyphens
    (replace-regexp-in-string "-+$" "" slug)))

(defun my/org-generate-id ()
  "Generate a human-readable ID for the current Org heading.

The ID is created by sanitizing the heading text to form a slug,
prepended by the creation date.  This ensures the ID is
informative and unique within a reasonable scope."
  (interactive)
  ;; Check if the ID property already exists
  (unless (org-id-get)
    ;; Retrieve the heading text
    (let* ((heading (nth 4 (org-heading-components)))
           (clean-heading (my-org-sanitize-heading-for-id heading))
           ;; Use CREATED property if available
           (date-prefix (or (my-org-get-created-date)
                            ;; Fallback to today's date
                            (format-time-string (org-time-stamp-format t t))))
           (new-id (concat date-prefix "-" clean-heading)))
      ;; Set the ID property
      (org-set-property "ID" new-id)

      ;; Add the ID with location FILE to the database of ID locations.
      (org-id-add-location new-id (buffer-file-name (buffer-base-buffer)))

      ;; Copy the ID to the kill-ring
      (kill-new (concat "id:" new-id))
      new-id)))  ; Return the new ID

(defun my/org-id-advice (&rest args)
  "The advice to update Org ID locations."
  ;; Create ID if needed
  (my/org-generate-id)
  (org-id-update-id-locations)
  args)

(advice-add 'org-store-link :before #'my/org-id-advice)

(bind-key (kbd "I") #'my/org-generate-id my-keyboard-map)

(defun my/org-mark-as-project ()
  "Mark the current Org heading as a project.

This function ensures that the current heading:
1. Has the tag ':project:'
2. Has the property 'COOKIE_DATA' set to 'todo recursive'
3. Has a 'TODO' keyword
4. Starts with a progress indicator '[/]'

This setup helps in organizing projects within Org Mode
and ensures that projects are easily identifiable and managed.

I find this approach particularly appealing, and I first learned
about it from an article by Karl Voit.  The original function was
created by Karl, and this version is simply a refined and
optimized adaptation tailored to my specific needs.  For origin
see: https://karl-voit.at/2019/11/03/org-projects/"
  (interactive)
  ;; Try to move to a heading if not already there
  (unless (org-at-heading-p)
    (org-back-to-heading t)
    (unless (org-at-heading-p)
      (user-error
       "No Org heading found. Please place the cursor at or near a heading.")))

  ;; Ensure the :project: tag is added
  (org-toggle-tag "project" 'on)

  ;; Set the COOKIE_DATA property to 'todo recursive'
  (org-set-property "COOKIE_DATA" "todo recursive")

  ;; Ensure the :ID: property is added
  (my/org-generate-id)

  (let* ((title (nth 4 (org-heading-components)))
         (keyword (nth 2 (org-heading-components))))

    ;; Add 'TODO' keyword if missing
    (unless keyword (org-todo "TODO"))

    ;; Ensure that the heading starts with a progress indicator '[/]'
    (unless (string-match-p "\\[.*\\]" title)
      (org-edit-headline (concat "[/] " title))))

  ;; Update statistics cookies
  (org-back-to-heading t)
  (org-update-statistics-cookies nil)

  ;; Save changes
  (save-buffer)

  ;; Inform the user of success
  (message "Heading marked as a project."))

(bind-key (kbd "P") #'my/org-mark-as-project my-keyboard-map)

;;;; Org Crypt
;; Check if GPG is available, then require `org-crypt'.
;; Otherwise, display a warning.
(if my-gpg-program
    (require 'org-crypt)
  (warn "GPG is not available. 'org-crypt' could not be loaded."))

;; Set my encrypt key from `epa-file-encrypt-to' (see above).
(setq org-crypt-key epa-file-encrypt-to)

;; Do not ask for disabling `auto-save-mode'.
(setq org-crypt-disable-auto-save nil)

;; Encrypt all entries before saving.
(org-crypt-use-before-save-magic)

;; Projects are tagged with ':project:' and ':crypt:' is used to mark headings
;; to be encrypted.  I don't want all subitems to pop up in the corresponding
;; agenda view.
(setq org-tags-exclude-from-inheritance '("project" "crypt"))

;;;;; Org Contib
(use-package org-cliplink
  :ensure t
  :after org
  :bind ("C-x p i" . org-cliplink))

;;;;; Org TODO
;; Define my default keywords as workflow states.
;; The command C-c C-t cycles an entry from 'TODO' to 'CANCELED'.
;; For details see: https://orgmode.org/manual/Workflow-states.html
(setq org-done-keywords '("DONE" "CANCELED"))
(setq org-todo-keywords
      '((sequence
         ;; Need action
         "TODO(t)" "STARTED(s)" "WAITING(w@/!)" "|"
         ;; Done
         "DONE(d!/!)" "CANCELED(c@/!)")))

;; Define the style of the keywords
(setq org-todo-keyword-faces
      '(("TODO"     :foreground "firebrick1"   :weight bold)
        ("STARTED"  :foreground "red3"         :weight bold)
        ("WAITING"  :foreground "orange"       :weight bold)
        ("DONE"     :foreground "forest green" :weight bold)
        ("CANCELED" :foreground "lime green"   :weight bold)))

;;;;; Org Capture
(defconst my-org-capture-template-simple
  "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n"
  "A template for a simple heading with keyword TODO in Org files.")

(defconst my-org-capture-template-blog
  ;; Quick template expansion explanation:
  ;;
  ;; %?         - position point here
  ;; %^g        - prompt for tags
  ;; %U         - date
  ;; %^{PROMPT} - prompt for a string
  ;;
  ;; For the full documentation see:
  ;; https://orgmode.org/org.html#Template-expansion-1
  (concat
   "* TODO %?        :blog:%^g\n"
   ":PROPERTIES:\n:CREATED: %U\n:ID: %^{PROMPT}\n:END:\n\n"
   "-----------------------\n"
   "- [ ] Link on older/similar articles?\n"
   "- [ ] Link on tag pages?\n\n")
  "A template for capturing blog-related ideas.")

;; Default target for capturing notes. Also used for mobile sync.
;; See `org-mobile-inbox-for-pull' bellow.
(setq org-default-notes-file (concat my-org-files-path "inbox.org"))

;; Define custom Org Capture templates
(setq org-capture-templates
      `(("s" "Shorts" entry
         (file+headline ,(concat my-org-files-path "misc.org") "Shorts")
         ,my-org-capture-template-simple :empty-lines 1)
        ("e" "Event" entry
         (file+headline ,(concat my-org-files-path "misc.org") "Events")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        ("t" "Trip Checklist" checkitem
         (file+headline ,(concat my-org-files-path "misc.org") "Trip Checklist"))
        ("b" "Bookmark" entry
         (file+headline ,(concat my-org-files-path "notes.org") "Bookmarks")
         "* %x%?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
        ("i" "Blog Idea" entry
         (file+headline ,(concat my-org-files-path "blog.org") "Captured Blog Ideas")
         ,my-org-capture-template-blog :empty-lines 1)
        ("n" "Random Note" entry
          (file+headline ,(concat my-org-files-path "notes.org") "Random Notes")
          "** %?\n  %U" :empty-lines 1)
        ("I" "Inbox, refile later" entry
         (file ,(concat my-org-files-path "inbox.org"))
          "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 0)
        ("r" "To-Read" entry
         (file+headline ,(concat my-org-files-path "misc.org") "To-Read List")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%x\n\n" :empty-lines 1)
        ("w" "To-Watch" entry
         (file+headline ,(concat my-org-files-path "misc.org") "To-Watch List")
         "* TODO %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n%x\n\n" :empty-lines 1)

        ("B" "Business")
        ("Bs" "Shorts" entry
         (file+headline ,(concat my-org-files-path "business.org") "Shorts")
         ,my-org-capture-template-simple :empty-lines 1)
        ("Be" "Event" entry
         (file+headline ,(concat my-org-files-path "business.org") "Events")
         "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)))

(global-set-key (kbd "C-c c") #'org-capture)

;;;;; Org Mobile
;; Setting up Beorg sync path.  Open 'Files' in Beog mobile app
;; and create file called 'mobileorg' (w/o extension) finally
;; setup sync Beorg to Dropbox 'Notes'.
(setq org-mobile-directory (expand-file-name "~/Dropbox/Notes"))

;; Do not generate IDs for all headings.
(setq org-mobile-force-id-on-agenda-items nil)

;; Append captured notes and flags to `org-default-notes-file' file.
(setq org-mobile-inbox-for-pull org-default-notes-file)

(defun my/mobile-org-import ()
  "Import content from `org-mobile-capture-file' to `org-mobile-inbox-for-pull'.

This function performs the following steps:
1. Executes `org-mobile-pull' to synchronize mobile Org data.
2. Opens the `org-mobile-inbox-for-pull' file.
3. Removes trailing whitespace from the buffer.

It is intended to be used as part of the mobile Org synchronization
workflow, ensuring that captured tasks and notes are properly imported
and cleaned up for further processing."
  (interactive)
  (org-mobile-pull)
  (find-file org-mobile-inbox-for-pull)
  (delete-trailing-whitespace))

(bind-key "i" #'my/mobile-org-import my-keyboard-map)

(defun my/mobile-org-export ()
  "Push a custom agenda setup to MobileOrg directory.

This function temporarily modifies `org-agenda-custom-commands' to set up
an agenda suitable for MobileOrg, including a 1-month agenda view and a filtered
list of standalone tasks (excluding projects and completed tasks). After pushing
the agenda to MobileOrg, the original `org-agenda-custom-commands' is restored."
  (interactive)
  (let ((original-org-agenda-custom-commands org-agenda-custom-commands))
    ;; Temporarily set agenda commands for MobileOrg export.
    (let ((org-agenda-custom-commands
           '(("m" "Monthly Overview"
              ((agenda "1 month"
                       ((org-agenda-span 31)
                        (org-agenda-time-grid nil)
                        (org-agenda-entry-types '(:timestamp :sexp))))))
             ("s" "Standalone Tasks"
              ((tags-todo "-project-DONE-CANCELED"
                          ((org-agenda-skip-function
                            '(my/org-agenda-skip-if-parent-has-tag
                              "project")))))))))
      ;; Generate MobileOrg export.
      (org-mobile-push))
    ;; Restore the original agenda commands.
    (setq org-agenda-custom-commands original-org-agenda-custom-commands)))

(bind-key "e" #'my/mobile-org-export my-keyboard-map)

;;;;; Org Agenda
(defconst my-org-agenda-files-work
  `(,(concat my-org-files-path "business.org")
    ,(concat my-org-files-path "airslate.org"))
  "The list of my work agenda files.")

(defconst my-org-agenda-files-life
  `(,(concat my-org-files-path "blog.org")
    ,(concat my-org-files-path "contacts.org")
    ;; Finances / Legal / Assure / Insure / Regulate
    ,(concat my-org-files-path "flair.org")
    ,(concat my-org-files-path "housing.org")
    ,(concat my-org-files-path "inbox.org")
    ,(concat my-org-files-path "misc.org")
    ,(concat my-org-files-path "notes.org"))
  "The list of my non-work agenda files.")

;; I maintain two categories of agenda files: work and non-work files.
(setq org-agenda-files
      (append my-org-agenda-files-work my-org-agenda-files-life))

(defun my|create-missing-org-files ()
  "Create missing files listed in `org-agenda-files'."
  (dolist (file org-agenda-files)
    (unless (file-exists-p file)
      (with-temp-buffer
        (write-file file))
      (message "Created missing org file: %s" file))))

(add-hook 'emacs-startup-hook #'my|create-missing-org-files)

(defun my|org-agenda-refresh-on-idle ()
    "Set up a timer to refresh Org Agenda buffers after 60s of idle time."
    (run-with-idle-timer 60 nil
                         (apply-partially #'org-agenda-prepare-buffers
                                          (org-agenda-files t t))))

(add-hook 'org-agenda-mode  #'my|org-agenda-refresh-on-idle)

;; Do not initialize agenda Org files when generating (only) agenda.
(setq org-agenda-inhibit-startup t)

;; Compact the block agenda view
(setq org-agenda-compact-blocks t)

;; Number of days to include in overview display.
(setq org-agenda-span 1)

;; Also include diary on org-agenda.
(setq org-agenda-include-diary t)

;; Restore windows layout after quit agenda.
(setq org-agenda-restore-windows-after-quit t)

;; Open agenda in the current window.
(setq org-agenda-window-setup 'current-window)

;; Super agenda mode.
;; For documentation  see: https://github.com/alphapapa/org-super-agenda
(use-package org-super-agenda
  :ensure t
  :custom
  (org-super-agenda-hide-empty-groups t)
  (org-super-agenda-groups
   '(;; Each group has an implicit boolean OR operator between its selectors.
     (:name "Today" :scheduled today)
     (:name "DEADLINES" :deadline t :order 1)
     (:name "Important" :priority "A" :order 2 :face (:append t :weight bold))
     (:name "Prio ≤ B" :priority<= "B" :order 30)
     (:name "Started"
            :and
            (:todo "STARTED" :not (:tag "someday")
             :not (:priority "C") :not (:priority "B"))
            :order 10)
     (:todo "WAITING" :order 18)
     (:name "Someday"
            :and (:tag "someday" :not (:priority "C") :not (:priority "B"))
            :order 25)))
  :hook (org-agenda-mode . org-super-agenda-mode))

;; HTML export functionality used bellow in `org-agenda-custom-commands'.
;; The main purpose of this package in my configuration is to call the
;; `org-store-agenda-view' function, which uses certain commands defined in
;; `org-agenda-custom-commands' to generate HTML reports.
(use-package htmlize
  :ensure t
  :defer t
  :config
  (require 'htmlize))

(defun my-skip-non-stuck-projects ()
  "Skip projects that are not stuck."
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (or (org-entry-is-done-p)  ; Skip if project is done
            (save-excursion
              (re-search-forward "^\\*+ \\(TODO\\)" subtree-end t))
            (save-excursion
              (re-search-forward
               "^\\*+.* SCHEDULED:.*\\|^[[:space:]]*SCHEDULED:.*"
               subtree-end t)))
        subtree-end  ; Skip if project has TODO or SCHEDULED descendants
      nil)))         ; Do not skip if the project is stuck

(defun my/org-agenda-skip-if-parent-has-tag (tag)
  "Skip an entry if any of its parents have the specified TAG.

TAG should be a string without the colons, e.g., 'project'."
  (let ((parent-skipped nil))
    (save-excursion
      (while (and (not parent-skipped) (org-up-heading-safe))
        (when (member tag (org-get-tags))
          (setq parent-skipped t))))
    (when parent-skipped
      (or (outline-next-heading)
          (goto-char (point-max))))))

;; For details see: https://orgmode.org/manual/Special-Agenda-Views.html
(setq org-agenda-custom-commands
      `(("g" "Agenda" agenda "")
        ;; List of tasks to add missed tags
        ("u" "Untagged Headings" tags "-{.*}"
         ((org-agenda-overriding-header "Untagged Headings")))
        ;; Probably need to add :someday: tag
        ("o" "Unfinished, but not scheduled tasks"
         ((tags-todo "-someday-project"
                     ((org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'scheduled))
                      (org-agenda-overriding-header "Unscheduled Tasks")))))
        ;; Probably need to remove the scheduled date
        ("O" "Scheduled, but with :someday: tag"
         ((tags "+someday"
                ((org-agenda-skip-function
                  '(or (org-agenda-skip-entry-if 'notscheduled)
                       (org-agenda-skip-entry-if 'todo 'done)))
                 (org-agenda-overriding-header "Scheduled Someday Tasks")))))
        ;; Unscheduled with :someday: tag
        ("p" "Pick a task from unscheduled list"
         ((tags "+someday"
                ((org-agenda-skip-function
                  '(or (org-agenda-skip-entry-if 'scheduled)
                       (org-agenda-skip-entry-if 'todo 'done)))
                 (org-agenda-overriding-header "Backlog")))))
        ;; Non-business tasks that currenly in my focus
        ("N", "Non-business: Open focus projects"
         ((tags "+focus+project-CATEGORY={airslate\\|business}"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header
                  "Personal Focus: Open Projects")))))
        ;; Business tasks that currenly in my focus
        ("B" "Business: Open focus projects"
         ((tags "+focus+project+CATEGORY={airslate\\|business}"
                ((org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-overriding-header
                  "Business Focus: Open Projects")))))
        ;; Something went wrong with these projects
        ("P" "Stuck Projects"
         ((tags "+project-someday-DONE-CANCELED"
                ((org-agenda-skip-function 'my-skip-non-stuck-projects)
                 (org-agenda-overriding-header
                  "Stuck Projects with open but not scheduled sub-tasks")))))
        ;; Filtered tasks excluding those with :project: tag, their children,
        ;; and DONE/CANCELED tasks
        ("-" "Standalone Tasks"
         ((tags-todo "-project-DONE-CANCELED"
                     ((org-agenda-skip-function
                       '(my/org-agenda-skip-if-parent-has-tag "project"))
                      (org-agenda-overriding-header
                       "Standalone Tasks: No Projects or DONE Items")))))
        ;; This command creates an agenda view for the next 180 days, excluding
        ;; all TODO items. It displays all scheduled events that are not tasks
        ;; with TODO states, over the specified period. To save the result as an
        ;; HTML file, you must manually call the `org-store-agenda-view' which
        ;; generates and saves the HTML report directly based on the defined
        ;; commands, without needing to first display the agenda view. The
        ;; resulting file will be saved in the directory specified by
        ;; `my-org-files-path' and will be named 'agenda_180d_filtered.html'.
        ("n" "No TODO events +180d"
         ((agenda "No TODO events +180d"
                  ((org-agenda-span 180)
                   (org-agenda-time-grid nil)
                   (org-agenda-entry-types '(:timestamp :sexp))
                   (org-agenda-skip-function
                    '(org-agenda-skip-entry-if 'todo 'any)))))
         nil
         (,(concat my-org-files-path "agenda_180d_filtered.html")))
        ;; Full agenda for the next 31 days.
        ("D", "Detail agenda +31d"
         ((agenda "Detail agenda"
                  ((org-agenda-span 31)
                   (org-agenda-time-grid nil))))
         nil
         (,(concat my-org-files-path "agenda_details_raw.html")))))

;; Always highlight the current agenda line.
(add-hook 'org-agenda-mode-hook (lambda () (hl-line-mode 1)))

(global-set-key (kbd "C-c a") #'org-agenda)

;;;;; Org Bookmarks
(defun my-org-switch-to-buffer (filename)
  "Switch to the buffer associated with the given Org FILENAME.

If a buffer visiting FILENAME is already open, switch to it.  If
the buffer does not exist, open the FILENAME from the 'my-org-files-path'
directory and switch to the newly created buffer.

FILENAME should be the name of the Org file without any directory
path.  The file is expected to reside in the `my-org-files-path' directory."
  (switch-to-buffer
   (or (get-buffer filename)
       (find-file-noselect (concat my-org-files-path filename)))))

(defun my/org-move-bookmark-to-notes()
  "Move and format mobile bookmark heading.

This function is designed to integrate with workflows where you
use a mobile app for capturing notes. These notes are
synchronized into a file specified by `org-mobile-inbox-for-pull'
using `org-mobile-pull'. The primary purpose of this function is
to move bookmarks, which you add on your mobile device and which
end up in the `org-mobile-inbox-for-pull' file, into 'notes.org'
while formatting them according to my preferred style. I have
accounted for the specific formats used by MobileOrg and Beorg in
this implementation, but if you encounter issues, please let me
know so I can address them.

Steps performed by this function:

1. Remove unwanted keywords from beggining of the heading:
   - 'TODO Bookmark'
   - 'Bookmark'
   - 'BOOKMARK'
   - 'TODO'
2. Convert the heading to a second-level heading if needed.
3. Insert a creation date property under the heading.
4. Move the cleaned and formatted bookmark to the end of 'notes.org'.
5. Reapply tags to the previous heading in 'notes.org'.

This function is adapted from an implementation by Karl Voit, and
has been reworked to enhance its readability, maintainability,
and alignment with my specific workflow. For origin see:
https://karl-voit.at/2014/08/10/bookmarks-with-orgmode/"
  (interactive)
  (save-excursion
    ;; Use `org-back-to-heading' instead `beginning-of-line' here
    ;; just because the cursor may not be on the heading, but below it,
    ;; in a block.
    (org-back-to-heading t)
    ;; Save the starting point and narrow to the current heading
    (let ((start (point)))
      (outline-next-visible-heading 1)
      (save-restriction
        (narrow-to-region start (point))

        ;; Step 1: Clean up heading by removing unwanted keywords
        (goto-char (point-min))
        (when (re-search-forward
               (concat "\\*\\s-+\\(?:"
                       "\\(?:TODO\\s-+\\)?Bookmark\\|BOOKMARK\\|TODO"
                       "\\)\\s-+")
               nil t)
          (replace-match "* " nil nil))

        ;; Step 2: Ensure the heading is at the second level
        (goto-char (point-min))
        (when (looking-at "\\*+")
          (let ((current-level (- (match-end 0) (match-beginning 0))))
            (cond
             ; if level 1, add one more "*"
             ((= current-level 1) (insert "*"))
             ; if level > 2, remove excess "*"
             ((> current-level 2) (delete-char (- current-level 2))))))

        ;; Step 3: Insert creation date property
        (unless (org-entry-get nil "CREATED")
          (goto-char (point-min))
          ;; Check if the date already persist
          (if (re-search-forward org-ts-regexp-inactive nil t)
              (let ((timestamp (match-string-no-properties 0)))
                (org-back-to-heading)
                (org-entry-put nil "CREATED" timestamp)
                ;; Move the cursor to the end of the metadata,
                ;; including the end of the :PROPERTIES block and remove old
                ;; date stamp
                (org-end-of-meta-data t)
                (when (re-search-forward (regexp-quote timestamp) nil t)
                  (replace-match "")))
            ;; Otherwise use current date
            (org-entry-put nil "CREATED"
                           (format-time-string (org-time-stamp-format t t)))))

        ;; Step 4: Cut the current heading and move it to the 'notes.org' file
        (org-cut-subtree)
        (my-org-switch-to-buffer "notes.org")
        (end-of-buffer)
        (newline)
        (org-paste-subtree)  ; Paste the heading into the 'notes.org' file

        ;; Step 5: Reapply tags to the previous heading in the 'notes.org' file
        (org-back-to-heading t)
        (org-set-tags-command)))))

(bind-key "b" #'my/org-move-bookmark-to-notes my-keyboard-map)

;;;;; Org Refile
(defun my-org-opened-buffer-files ()
  "Return the list of org files currently opened in emacs."
  (delq nil
        (mapcar (lambda (x)
                  (if (and (buffer-file-name x)
                           (string-match "\\.org$"
                                         (buffer-file-name x)))
                      (buffer-file-name x)))
                (buffer-list))))

;; Refile targets include this file and any file contributing
;; to the agenda - up to 4 levels deep.
(setq org-refile-targets
      '((nil :maxlevel . 4)  ; nil means current buffer
        (my-org-opened-buffer-files :maxlevel . 4)
        (org-agenda-files :maxlevel . 4)))

;; Targets start with the file name allows creating level 1 tasks.
(setq org-refile-use-outline-path 'file)

;; Speed up refiling by caching target locations, a must for large Org files.
(setq org-refile-use-cache t)

;; Make `org-refile' outline working IDO/consult/helm/ivy.
(setq org-outline-path-complete-in-steps nil)

;; Allow the creation of new nodes as refile targets.
;; The new node creation must be confirmed by the user.
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; Set a timer to regenerate refile cache automatically every time
;; my Emacs has been idled for 10 mins.
(run-with-idle-timer 600 t (lambda ()
                             (org-refile-cache-clear)
                             (org-refile-get-targets)))

(defun my|org-update-statistics-cookies-after-refile ()
  "Update statistics cookies after refiling a task."
  (org-update-statistics-cookies nil))

(add-hook 'org-after-refile-insert-hook
          #'my|org-update-statistics-cookies-after-refile)

;; Local keys
(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c r") #'org-refile )
  (define-key org-mode-map (kbd "C-c C-x C-a") #'org-archive-subtree))

;;;; Window Handling
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

(add-hook 'after-load-theme-hook #'my-set-cursor-color-based-on-theme)

;; Modus themes
(load-theme 'modus-vivendi t)
(my-set-cursor-color-based-on-theme)

(global-set-key (kbd "<f5>") #'modus-themes-toggle)

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

(defun my/duplicate-line (arg)
  "Duplicate current line, ARG times leaving point in lower line.
This function is for interactive use only;"
  (interactive "*p")
  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol
         (save-excursion
           (beginning-of-line)
           (point)))
        eol)

    (save-excursion
      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t)
            (count arg))
        ;; insert the line arg times
        (while (> count 0)
          (newline)         ;; because there is no newline in 'line'
          (insert line)
          (setq count (1- count))))

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))

  ;; put the point in the lowest line and return
  (forward-line arg))

(global-set-key (kbd "C-c d") #'my/duplicate-line)

;;;; Project management
(require 'project)

;; Auto clean up zombie projects from `project-list-file'
(run-at-time "07:00pm" (* 24 60 60) 'project-forget-zombie-projects)

;; Use ripgrep if installed
(when (executable-find "rg")
  (setq xref-search-program 'ripgrep))

(defun my/project-switch-project (dir)
  "Switch to another project by running an Emacs command.
Directly use `project-find-file' instead of getting prompted.

When called in a program, it will use the project corresponding
to directory DIR."
  (interactive (list (project-prompt-project-dir)))
  (let ((project-current-directory-override dir))
    (project-find-file)))

(defun my/switch-project-and-kill-buffers ()
  "Kill all buffers of the current project, then switch to a new project."
  (interactive)
  (project-kill-buffers t)
  (call-interactively 'my-project-switch-project))

(defun my/project-list-buffers ()
  "Display a list of buffers associated with the current project.

This function filters the list to show only file-visiting buffers.
After generating the buffer list, it pops up the `*Buffer List*` buffer
in a new window, allowing you to easily navigate through the buffers
related to your current project."
  (interactive)
  (project-list-buffers t)
  (pop-to-buffer "*Buffer List*"))

;; Bind keys to commands within the `project-prefix-map'
(define-key project-prefix-map (kbd "p") #'my/project-switch-project)
(define-key project-prefix-map (kbd "s") #'my/switch-project-and-kill-buffers)
(define-key project-prefix-map (kbd "C-b") #'my/project-list-buffers)
(define-key project-prefix-map (kbd "R") #'project-remember-projects-under)
(define-key project-prefix-map (kbd "K") #'project-kill-buffers)

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
  :bind (("C-x l"   . consult-locate)
         ("C-x b"   . consult-buffer)
         ("C-x B"   . consult-buffer-other-window)
         ("C-c k"   . consult-ripgrep)
         ("C-c f"   . consult-recent-file)
         ("C-r"     . consult-history)
         ("C-S-s"   . consult-line)
         ("C-<f5>"  . consult-theme)
         :map minibuffer-local-map
         ("C-r"     . consult-history)))

(defun my/consult-jump-in-buffer ()
  "Jump in buffer with `consult-imenu' or `consult-org-heading' if in org-mode."
  (interactive)
  (call-interactively
   (cond
    ((eq major-mode 'org-mode) 'consult-org-heading)
    (t 'consult-imenu))))

(global-set-key (kbd "C-x <tab>") #'my/consult-jump-in-buffer)

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

(use-package embark-consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

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
        ;; No need to manually call `password-store-get' here if you
        ;; properly setup `auth-source' above.
        ;;
        ;; In my case I store only password in:
        ;;
        ;;   ~/.password-store/irc/irc.libera.chat/<username>.gpg
        ;;
        ;; and this is simple wroks.
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

(defconst pandoc-executable-path (executable-find "pandoc")
  "The pandoc executable path on this system.")

(defconst gfm-patterns
  (rx (or "README" "CONTRIBUTTING" "BACKERS"
          (: "CHANGELOG" (? "-" (+ (in "." digit))))
          "CODE_OF_CONDUCT"
          "PULL_REQUEST_TEMPLATE"
          "pull_request_template"
          "bug_report"
          "feature_request")
      "."
      (or "markdown" "md")
      string-end)
  "Regexp to match files with GFM format.")

(use-package markdown-mode
  :ensure t
  :mode "\\.\\(?:m\\(?:arkdown\\|d\\)\\)\\'"
  :custom
  (markdown-command pandoc-executable-path)
  (markdown-enable-wiki-links t)
  (markdown-enable-math t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-make-gfm-checkboxes-buttons t)
  (markdown-gfm-additional-languages
   '("sh" "python" "js" "lisp" "elisp"))
  (markdown-gfm-uppercase-checkbox t)
  (markdown-fontify-code-blocks-natively t)
  (markdown-hide-urls nil)
  :hook
  ((markdown-mode . auto-fill-mode)
   (markdown-mode . visual-line-mode))
  :config
  (add-to-list 'auto-mode-alist
	       `(,gfm-patterns . gfm-mode)))

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

  ;; Automatically rescan imenu on each call
  (setq-local imenu-auto-rescan t)

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

;; Save custom variables in a separate file named custom.el in your
;; user-emacs-directory. This keeps your main configuration file clean.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Load the custom file if it exists or is a symbolic link. This should be done
;; at the very end of the configuration to ensure that it does not override any
;; settings made earlier in this file.
(when (or (file-exists-p custom-file) (file-symlink-p custom-file))
  (load custom-file t t))

;; For those who use my dotfiles and need an easy way to write their
;; own extras on top of what I already load.  The file must exist at
;; `~/.emacs.d/post-custom.el'
;;
;; The purpose of the "post customisations" is to make tweaks to what
;; I already define, such as to change the default theme.  See above
;; for the 'pre-custom.el' to make changes BEFORE loading any of my
;; other configurations.
(load (locate-user-emacs-file "post-custom.el") :no-error :no-message)

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:

;;; init.el ends here
