;;; core-dirs.el --- Directories initialization. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setting up global variables and directories.

;;; Code:

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

(provide 'core-dirs)
;;; core-dirs.el ends here
