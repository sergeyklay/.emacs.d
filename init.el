;;; init.el --- Initialization file. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; This file is used as a bootstrap for GNU Emacs.

;;; Code:

(require 'core (concat user-emacs-directory "core/core"))

(setq custom-file (concat user-etc-dir "custom.el"))
(load custom-file t)

;; One less file to load at startup
(setq site-run-file nil)

(require 'bookmarks)    ; Setting for bookmarks, recentf, etc
(require 'docs)         ; Setting up documentation features
(require 'appearance)   ; Appearance related settings
(require 'secure)       ; Security related features
(require 'windows)      ; Windows management features
(require 'modeline)     ; Modeline related configuration
(require 'projects)     ; Project navigation and management
(require 'comp-any)     ; Setting up company
(require 'comp-ivy)     ; Initialize ivy, counsel and swiper
(require 'jump)         ; Setting up tags and code navigation
(require 'vcs)          ; VCS related features
(require 'spelling)     ; Spell configuration
(require 'shells)       ; Shells configuration

(require 'org-lang)     ; Org related configuration
(require 'cc-lang)      ; Support for the C-family of languages
(require 'php-lang)     ; PHP related configuration
(require 'hs-lang)      ; Haskell configuration
(require 'lisp-lang)    ; Configure the Lisp-family of languages
(require 'md-lang)      ; Add markdown support
(require 'ms-lang)      ; Add support for M$ scripts
(require 'web-lang)     ; Add support for the Web-based languages
(require 'conf-lang)    ; Add support for the configuration like languages
(require 'go-lang)      ; Go related configuration
(require 'grammars)     ; Support of language grammars

(require 'editor)       ; Features related to the behavior of the editor
(require 'irc)          ; IRC tools
(require 'docker-io)    ; Docker related configuration
(require 'syntax-check) ; Syntax checkers
(require 'snippets)     ; Add snippets support

;;; init.el ends here
