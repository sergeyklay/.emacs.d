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
;;
;; This configuration is partially based on the following user
;; configurations:
;;
;; - Sacha Chua (sachac)
;;   (see URL `https://github.com/sachac')
;; - Daniel Mai (danielmai)
;;   (see URL `https://github.com/danielmai')
;; - Terencio Agozzino (rememberYou)
;;   (see URL `https://github.com/rememberYou')
;;
;; Thanks to them for their incredible work!

;;; Code:

;;; Begin initialization

;; Turn off mouse interface early in startup to avoid momentary display.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))     ; Disable the menu bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))     ; Disable the tool bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1)) ; Disable the scroll bar
(if (fboundp 'tooltip-mode) (tooltip-mode -1))       ; Disable the tooltips

;; Don't save customizations at all, prefer to set things explicitly.
(setq custom-file null-device)

;; One less file to load at startup.
(setq site-run-file nil)

;; Disable start-up screen.
(setq inhibit-startup-screen t)

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

(defconst emacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all Emacs will be verbose.
Set DEBUG=1 in the command line or use --debug-init to enable this.")

(require 'directories (concat user-emacs-directory "settings/directories"))

;; Setting up the dependencies, features and packages
(add-to-list 'load-path user-host-dir)
(add-to-list 'load-path user-site-lisp-dir)
(add-to-list 'load-path user-settings-dir)

;;; Personal information

(setq user-full-name "Serghei Iakovlev"
      user-mail-address "egrep@protonmail.ch")

(require 'packaging)  ; Package management stuff and various related settings
(require 'appearance) ; Set up appearance as soon as we can
(require 'backup)
(require 'vcs)        ; VCS (mostly git) related features

;;; Languages Support

;; Add support for the configuration like languages for GNU Emacs.

;; `yaml-mode' gives me the possibility to easily manage .yml files
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :interpreter ("yml" . yml-mode))

;;; init.el ends here
