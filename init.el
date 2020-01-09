;;; init.el --- Initialization file. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;;   This file is used to initialize GNU Emacs for my daily needs.
;; I started this project on 4 March 2019 from this commit:
;; eb11ce25b0866508e023db4b8be6cca536cd3044

;;; Code:

;; Begin initialization
(require 'directories (concat user-emacs-directory "settings/prelude"))

(require 'packaging)   ; Package management stuff and various related settings
(require 'appearance)  ; Set up appearance as soon as we can
(require 'backup)      ; Backup, auto-saving and history configuration
(require 'bookmarks)   ; Setting for bookmarks, recentf, etc
(require 'editor)      ; Features related to the behavior of the editor
(require 'p13n)        ; Personalization related stuff
(require 'vcs)         ; VCS (mostly git) related features
(require 'completion)  ; Set up completion system
(require 'projects)    ; Project navigation and management

(require 'langs-conf)  ; Add support for the configuration like languages
(require 'langs-md)    ; Add markdown support
(require 'langs-cmake) ; Add cmake support

;;; init.el ends here
