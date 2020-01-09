;;; prelude.el --- Begin initialization. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Begin initialization.

;;; Code:

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

;; Set up load path.
(add-to-list 'load-path user-host-dir)
(add-to-list 'load-path user-site-lisp-dir)
(add-to-list 'load-path user-settings-dir)
(add-to-list 'load-path user-defuns-dir)

;; Add external projects to load path.
(dolist (pkg (directory-files user-site-lisp-dir t "\\w+"))
  (when (file-directory-p pkg)
    (add-to-list 'load-path pkg)))

;; Maximize GNU Emacs frame on startup.
;; It also applies to new clients that connect to GNU Emacs server
;; (with e.g. emacsclient -c).
(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(provide 'prelude)
;;; prelude.el ends here
