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

(require 'packaging)  ; Package management stuff and various related settings
(require 'appearance) ; Set up appearance as soon as we can
(require 'backup)     ; Backup, auto-saving and history configuration
(require 'bookmarks)  ; Setting for bookmarks, recentf, etc
(require 'editor)     ; Features related to the behavior of the editor
(require 'p13n)       ; Personalization related stuff
(require 'vcs)        ; VCS (mostly git) related features

;;; Languages Support

;; Add support for the configuration like languages for GNU Emacs.

;; `yaml-mode' gives me the possibility to easily manage .yml files
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :interpreter ("yml" . yml-mode))

;;; init.el ends here
