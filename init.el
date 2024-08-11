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
;; Package management in Emacs can be done in several ways. I personally like
;; `use-package' together with package.el. Some will prefer straight.el, but I
;; haven't found the need for it yet.
(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/")))
(setq package-quickstart t)

;; Manually initialize packages in daemon or noninteractive mode.
(when (or (daemonp) noninteractive) (package-initialize))

;; For the actual package configuration, I use `use-package'.
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
  (unless (file-exists-p my-backup-dir)
    (make-directory my-backup-dir t)))

;;;; Auto-Saving
(let ((save-dir (concat user-emacs-directory "auto-save-list/")))
  (setq
   auto-save-file-name-transforms `((".*" ,(expand-file-name "\\2" save-dir) t))
   auto-save-list-file-name
   (concat save-dir (format ".saves-%d-%s~" (emacs-pid) (system-name))))
  (unless (file-exists-p save-dir) (make-directory save-dir t)))

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

;;;; Organization
(use-package org
  :mode ("\\.org\\'" . org-mode)
  :custom
  ;; Resize images to 300px, unless there's an attribute.
  (org-image-actual-width '(300))
  ;; Enable shift+arrow for text selection.
  (org-support-shift-select t)
  ;; Also include diary on org-agenda.
  (org-agenda-include-diary t)
  ;; Don' clutter the actual entry with notes.
  (org-log-into-drawer t)
  :config
  (declare-function org-agenda-prepare-buffers "org" files)
  (declare-function org-agenda-files "org" (&optional unrestricted archives))
  (defun my|org-agenda-refresh-on-idle-hook ()
    "Set up a timer to refresh Org Agenda buffers after 60s of idle time."
    (run-with-idle-timer 60 nil
                         (apply-partially #'org-agenda-prepare-buffers
                                          (org-agenda-files t t))))
  (unless (file-exists-p org-directory)
    (make-directory org-directory))
  :hook ((org-mode . visual-line-mode)
         (org-mode . org-indent-mode)
         (org-mode . org-display-inline-images)
         (org-agenda-mode . #'my|org-agenda-refresh-on-idle-hook))
  :bind (("C-c c" . #'org-capture)
         ("C-c a" . #'org-agenda)
         ("C-c l" . #'org-store-link)))

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
(load-theme 'modus-vivendi)

;; I prefer the thin cursor.
(setq-default cursor-type '(bar . 2))

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
  (when (shell-command-to-string "command rg --version")
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

This function calls `project-list-buffers' with a non-nil argument,
which filters the list to show only file-visiting buffers. After
generating the buffer list, it pops up the `*Buffer List*` buffer
in a new window, allowing you to easily navigate through the
buffers related to your current project."
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

;;;; IRC and other communication
(use-package erc
  :after auth-source
  :commands (erc erc-tls)
  :custom
  (erc-autojoin-channels-alist '(("Libera.Chat" "#emacs" "#re2c")))
  (erc-autojoin-timing 'ident)
  (erc-user-full-name user-full-name)
  (erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-threshold-time 43200)
  (erc-prompt-for-password nil)
  (erc-fill-function 'erc-fill-static)
  (erc-fill-static-center 22)
  :config
  (add-to-list 'erc-modules 'log)

  (defconst erc-logging-directory
    (concat user-emacs-directory "logs/erc/"))

  (defun my/erc-logging-hook ()
    "Setting up channel logging for `erc'."
    (eval-when-compile (require 'erc-log nil t))
    (custom-set-variables
     '(erc-log-channels-directory erc-logging-directory)
     '(erc-log-insert-log-on-open t)
     '(erc-save-buffer-on-part nil)
     '(erc-save-queries-on-quit nil)
     '(erc-log-write-after-insert t)
     '(erc-log-write-after-send t))
    (unless (file-exists-p erc-logging-directory)
      (make-directory erc-logging-directory t)))
  :hook
  (erc-mode . my/erc-logging-hook))

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

(declare-function erc-track-switch-buffer (arg))
(declare-function erc-update-modules ())

(defun my/erc-start-or-switch ()
  "Connects to ERC, or switch to last active buffer."
  (interactive)
  (if (get-buffer "Libera.Chat")
      (erc-track-switch-buffer 1)
    (when (y-or-n-p "Start ERC? ")
      (erc :server "irc.libera.chat" :port 6667))))

(global-set-key (kbd "C-c e f") #'my/erc-start-or-switch)

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
  :defer t)

(use-package sql-indent
  :ensure t
  :hook ((sql-mode . sqlind-minor-mode)))

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


;;;; Helpers
(use-package which-key
  :ensure t
  :defer 1
  :config
  (which-key-mode 1))

(use-package writegood-mode
  :ensure t
  :hook ((text-mode . writegood-mode)
         (org-mode . writegood-mode)
         (rst-mode . writegood-mode)
         (markdown-mode . writegood-mode)
         (latext-mode . writegood-mode)))

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:

;;; init.el ends here
