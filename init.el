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
(require 'prelude (concat user-emacs-directory "site-lisp/prelude"))

(require 'packaging)    ; Package management stuff and various related settings
(require 'appearance)   ; Set up appearance as soon as we can
(require 'devtools)     ; Various devtools
(require 'modeline)     ; Modeline related configuration
(require 'shells)       ; Shells configuration
(require 'defaults)     ; Sane defaults + settings for which there is no group
(require 'windows)      ; Windows management features
(require 'bookmarks)    ; Setting for bookmarks, recentf, etc
(require 'docs)         ; Setting up documentation features
(require 'vcs)          ; VCS (mostly git) related features
(require 'completion)   ; Set up completion system
(require 'security)     ; GPG and security related features
(require 'projects)     ; Project navigation and management
(require 'jump)         ; Setting up tags and code navigation
(require 'expansion)    ; Configuration of expansions
(require 'editor)       ; Features related to editor behavior
(require 'syntax-check) ; Syntax checkers
(require 'spelling)     ; Spell configuration

(require 'langs-human)  ; Settings for Human Languages
(require 'langs-conf)   ; Add support for the configuration like languages
(require 'langs-md)     ; Add markdown support
(require 'langs-cmake)  ; Add cmake support
(require 'langs-cc)     ; Support for the C-family of languages
(require 'langs-lisp)   ; Configure the Lisp-family of languages
(require 'langs-org)    ; Org related configuration

(require 'grammars)     ; Various language grammars

(require 'chats)        ; Chats support

;; Load settings specific for the current site
(when (file-exists-p user-host-dir)
  (mapc 'load (directory-files user-host-dir nil "^[^#].*el$")))

;;; init.el ends here
