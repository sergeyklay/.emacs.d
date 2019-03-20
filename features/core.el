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

;;; Sane defaults

(setq-default
 debug-on-error (and (not noninteractive) emacs-debug-mode)
 ;; Files
 mc/list-file (concat user-etc-dir "mc-lists.el"))

;;; Customize settings

(setq custom-file (concat user-etc-dir "custom.el"))
(load custom-file t)

(add-to-list 'load-path user-host-dir)

(when (file-exists-p user-host-dir)
  (mapc 'load (directory-files user-host-dir nil "^[^#].*el$")))

;;; Startup message customization

(setq inhibit-startup-message t)
(setq initial-scratch-message
      (concat
       ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"
       ";; To create a file, visit it with \\[find-file] and enter text in its buffer.\n"
       ";; To evaluate an S-expr in this buffer, use \\[eval-print-last-sexp].\n"
       ";;\n"
       ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(provide 'core)
;;; core.el ends here
