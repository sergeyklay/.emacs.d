;;; core.el -- Base initialization file. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d

;;; Commentary:

;; This file is used to set up base features

;;; Code:


;;; Setting up global variables and directories

(defconst emacs-start-time (current-time)
  "This variable used for the profiling purposes.")

(defconst emacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all Emacs will be verbose.
Set DEBUG=1 in the command line or use --debug-init to enable this.")

(defconst session-macos-p (equal system-type 'darwin)
  "This constant indicate whether the current session used on macOS.")

(defvar user-emacs-dir (file-truename user-emacs-directory)
  "The path to this emacs.d directory.
The real path of this directory is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.")

(defvar user-local-dir (concat user-emacs-dir ".local/")
  "Root directory for local Emacs files.
Use this as permanent storage for files that are safe to share across
systems (if this config is symlinked across several computers).")

(defvar user-etc-dir (concat user-local-dir "etc/")
  "Directory for non-volatile storage.
Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data.")

(defvar user-cache-dir (concat user-local-dir "cache/")
  "Directory for volatile storage.
Use this for files that change often, like cache files.")

(defvar user-features-dir (concat user-emacs-dir "features/")
  "All the features should located here.")

(defvar user-host-dir (concat user-etc-dir "hosts/" (system-name))
  "The directory with user-specific Emacs settings.
Function `system-name' returns the host name of the machine you
are running on, as a string.")


;;; Encoding

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)


;;; Backup behaviour

;; Delete excess backup versions silently
(setq delete-old-versions t)

;; Make numeric backup versions unconditionally.
(setq version-control t)

;; Make backup files even in version controlled directories
(setq vc-make-backup-files t)

;; Keep all backup in one directory.
(let ((my-backup-dir (concat user-cache-dir "backup/")))
  (setq backup-directory-alist
        `(("." . ,(file-name-as-directory my-backup-dir))))
  (unless (file-exists-p my-backup-dir)
    (make-directory my-backup-dir t)))

;; Setting up Auto-Saving.
;; For more see URL `https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto_002dSaving.html'
(let ((my-auto-save-dir (concat user-cache-dir "autosave/")))
  (setq
   auto-save-file-name-transforms
   `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t))

   auto-save-list-file-name
   (concat my-auto-save-dir
            (format ".saves-%d-%s~" (emacs-pid) (system-name))))

  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir t)))

(setq auto-save-default t
      auto-save-timeout 10
      auto-save-interval 200)


;;; Sane defaults

(setq-default
 debug-on-error (and (not noninteractive) emacs-debug-mode)
 history-length 500
 history-delete-duplicates t
 ;; Files
 recentf-save-file            (concat user-cache-dir "recentf")
 savehist-file                (concat user-cache-dir "minibuffer-history.el")
 mc/list-file                 (concat user-etc-dir "mc-lists.el")
 tramp-auto-save-directory    (concat user-cache-dir "autosave/tramp/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat user-cache-dir "tramp-persistency.el"))

(savehist-mode 1)

;; Startup message customization
(setq inhibit-startup-message t)


;;; Bootstrap packaging system

;; Set up package
(require 'package)
(setq package-archives
      '(("org"          . "http://orgmode.org/elpa/")
        ("melpa"        . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")
        ("gnu"          . "https://elpa.gnu.org/packages/")))

(setq package--init-file-ensured t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))

;; Install quelpa
(unless (package-installed-p 'quelpa)
  (package-refresh-contents)
  (package-install 'quelpa))

(require 'quelpa)
(setq quelpa-update-melpa-p nil)

;; Install quelpa-use-package
(quelpa
 '(quelpa-use-package
   :fetcher github
   :repo "quelpa/quelpa-use-package"))

(require 'quelpa-use-package)
(setq use-package-ensure-function 'quelpa)

;; Always automatically install missing packages
(setq use-package-always-ensure t)

;; Install diminish
(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(require 'diminish)
(require 'bind-key)


;;; Setting up the dependencies, features and packages

(add-to-list 'load-path user-features-dir)

(setq custom-file (concat user-etc-dir "custom.el"))
(load custom-file t)

(add-to-list 'load-path user-host-dir)

(provide 'core)
;;; core.el ends here
