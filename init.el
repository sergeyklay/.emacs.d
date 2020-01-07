
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
(when window-system
  (menu-bar-mode -1)   ; Disable the menu bar
  (tool-bar-mode -1)   ; Disable the tool bar
  (scroll-bar-mode -1) ; Disable the scroll bar
  (tooltip-mode -1))   ; Disable the tooltips

;; Don't save customizations at all, prefer to set things explicitly.
(setq custom-file null-device)

;; One less file to load at startup.
(setq site-run-file nil)

;; Actually this project is my personal configuration
;; so I use GNU Emacs 26.1 now.
(eval-when-compile
  (and (version< emacs-version "26.0")
       (error
        (concat
         "Detected Emacs %s. "
         "This configuration is designed to work "
         "only with Emacs 26.0 and higher. I'm sorry.")
        emacs-version)))

(defconst emacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all Emacs will be verbose.
Set DEBUG=1 in the command line or use --debug-init to enable this.")

;;; Setting up global variables and directories

(defconst user-site-lisp-dir (concat user-emacs-directory "site-lisp/")
  "Local site-lisp directory.")

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
  "The directory with user-specific Emacs settings.
Function `system-name' returns the host name of the machine you
are running on, as a string.")

;;; Bootstrap packaging system

(require 'package)

(setq package--init-file-ensured t)
(setq package-user-dir
      (concat
       (substitute-in-file-name "$HOME/.local/lib/emacs/")
       "packages/" emacs-version "/elpa"))

(unless (file-exists-p package-user-dir)
  (make-directory package-user-dir t))

(setq package-gnupghome-dir
      (expand-file-name "gnupg" user-local-dir))

(setq package-archives
      '(("org"      . "http://orgmode.org/elpa/")
        ("melpa"    . "http://melpa.org/packages/")
        ("m-stable" . "http://stable.melpa.org/packages/")
        ("elpa"     . "https://elpa.gnu.org/packages/")))

;; Priorities. Default priority is 0.
(setq package-archive-priorities
      '(("m-stable" . 10)
        ("melpa" . 20)))

;; Initialize package manager.
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))


;; All packages should be installed.
(setq use-package-always-ensure t)
(setq use-package-verbose emacs-debug-mode)

(require 'bind-key)

;;; Personal information

(setq user-full-name "Serghei Iakovlev"
      user-mail-address "egrep@protonmail.ch")

;;; Theme

;; I tend to switch themes more often than normal.
;; Thus there are convenient theme functions to manipulate
;; them intercatively.

(defun switch-theme (theme)
  "Disables any currently active themes and loads THEME."
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
			     (mapc 'symbol-name
				   (custom-available-themes))))))
  (let ((enabled-themes custom-enabled-themes))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme theme t)))

(defun disable-active-themes ()
  "Disables any currently active themes listed in `custom-enabled-themes'."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(bind-key "M-<f12>" 'switch-theme)
(bind-key "M-<f11>" 'disable-active-themes)

;; One Dark Theme

(use-package one-themes
  :if (window-system)
  :config
  (switch-theme 'one-dark))

;; Steve Purcell's Tomorrow theme

(use-package color-theme-sanityinc-tomorrow
  :config
  (switch-theme 'sanityinc-tomorrow-night))

;;; Languages Support

;; Add support for the configuration like languages for GNU Emacs.

;; `yaml-mode' gives me the possibility to easily manage .yml files
(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :interpreter ("yml" . yml-mode))

;;; init.el ends here
