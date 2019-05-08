;;; core.el --- Base initialization file. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>
;; Copyright (c) 2016-2019 Henrik Lissner

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; This file is used to set up GNU Emacs configuration.
;;
;; Some code parts was created in inspiration of Doom Emacs by Henrik Lissner
;; which is licensed under the MIT License.  For more see URL
;; `https://github.com/hlissner/doom-emacs'.

;;; Code:

;; Actually this project is my personal configuration
;; so I use GNU Emacs 26.1 now.
(eval-when-compile
  (and (version< emacs-version "26.1")
       (error
        (concat
         "Detected Emacs %s. "
         "This configuration is designed to work "
         "only with Emacs 26.1 and higher. I'm sorry.")
        emacs-version)))

;;; Setting up global variables and directories

(defconst emacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all Emacs will be verbose.
Set DEBUG=1 in the command line or use --debug-init to enable this.")

(defconst user-emacs-dir (file-truename user-emacs-directory)
  "The path to this emacs.d directory.
The real path of this directory is found by chasing symbolic links
both at the level of the file and at the level of the directories
containing it, until no links are left at any level.")

(defconst user-site-lisp-dir (concat user-emacs-dir "site-lisp/")
  "Local site-lisp directory.")

(defconst user-local-dir (concat user-emacs-dir ".local/")
  "Root directory for local Emacs files.
Use this as permanent storage for files that are safe to share across
systems (if this config is symlinked across several computers).")

(defconst user-etc-dir (concat user-local-dir "etc/")
  "Directory for non-volatile storage.
Use this for files that don't change much, like servers binaries,
external dependencies or long-term shared data.")

(defconst user-cache-dir (concat user-local-dir "cache/")
  "Directory for volatile storage.
Use this for files that change often, like cache files.")

(defconst user-core-dir (concat user-emacs-dir "core/")
  "All the core configuration located here.")

(defconst user-features-dir (concat user-emacs-dir "features/")
  "All the features should located here.")

(defconst user-host-dir (concat user-etc-dir "hosts/" (system-name))
  "The directory with user-specific Emacs settings.
Function `system-name' returns the host name of the machine you
are running on, as a string.")

;; Setting up the dependencies, features and packages
(add-to-list 'load-path user-core-dir)
(add-to-list 'load-path user-features-dir)
(add-to-list 'load-path user-host-dir)
(add-to-list 'load-path user-site-lisp-dir)

;; Add feature groups to the load path
(dolist (feature-group (directory-files user-features-dir t "\\w+"))
  (when (file-directory-p feature-group)
    (add-to-list 'load-path feature-group)))

(require 'core-backup)

(global-set-key (kbd "C-x t d") #'toggle-debug-on-error)

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(add-to-list 'auto-coding-alist '("/#[^/]+#\\'" . utf-8))

;;; Sane defaults

(setq-default
 debug-on-error (and (not noninteractive) emacs-debug-mode)
 vc-follow-symlinks t ; Don't ask for confirmation when opening symlinks
 ;; Files
 tramp-auto-save-directory    (concat user-cache-dir "tramp/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat user-cache-dir "tramp-persistency.el"))

;; Startup message customization
(setq inhibit-startup-message t)

;; Turn off mouse interface early in startup to avoid momentary display
(when window-system
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1))

;; Enable disabled by default commands permanently
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Open URLs with xdg-open
(setq browse-url-browser-function 'browse-url-xdg-open)

;; I use C source to understand and debug built-in functions
(setq source-directory
      (expand-file-name (substitute-in-file-name "$HOME/src/emacs")))

;; I don't use this at all.
;; However, it always trolls me when I want to open a file.
(global-unset-key "\C-xf")

(setenv "GPG_AGENT_INFO" nil)

(require 'core-defuns)
(require 'core-packaging)
(require 'core-news)

(require 'server)
(unless (server-running-p)
    (server-start))

(provide 'core)
;;; core.el ends here
