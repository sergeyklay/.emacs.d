;;; directories.el --- Global directories configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setting up global variables and directories.

;;; Code:

(defconst user-site-lisp-dir (concat user-emacs-directory "site-lisp/")
  "Local site-lisp directory.")

(defconst user-defuns-dir (concat user-emacs-directory "defuns/")
  "A common place to store user's defuns.")

(defconst user-local-dir (concat user-emacs-directory ".local/")
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

(defconst user-host-dir (concat user-etc-dir "hosts/" (system-name))
  "The directory with user-specific Emacs settings.")

(provide 'directories)
;;; directories.el ends here
