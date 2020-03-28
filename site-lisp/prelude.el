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

(require 'directories (concat user-emacs-directory "site-lisp/directories"))

(defvar my--file-name-handler-alist file-name-handler-alist
  "Backup `file-name-handler-alist' to restore in the future.")

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(setq file-name-handler-alist nil)

;; Increasing GC is a common way to speed up Emacs.
;; `gc-cons-threshold' sets at what point Emacs should
;; invoke its garbage collector.  When set it temporarily
;; to a large number, we only garbage collect once on startup.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(defvar file-name-handler-alist-backup
  file-name-handler-alist
  "The backup of `file-name-handler-alist' we'll restore on `after-init-hook'.")

;; Reset `gc-cons-threshold' to the standard value
;; after initializing the Emacs session.
;; Also we restore `file-name-handler-alist' here.
(defun reset-performance ()
  "Reset Emacs performace settings to the standard ones."
  (garbage-collect)
  (setq gc-cons-threshold
	(car (get 'gc-cons-threshold 'standard-value))
        gc-cons-percentage
	(car (get 'gc-cons-percentage 'standard-value))
        file-name-handler-alist
	(append
	 file-name-handler-alist-backup
	 file-name-handler-alist)))

(defun compile-init-file ()
  "Bytecompile init file."
  (interactive)
  (when user-init-file
    (byte-compile-file (file-truename user-init-file))))

(add-hook 'after-init-hook #'reset-performance)
(add-hook 'kill-emacs-hook #'compile-init-file)

;; One less file to load at startup
(setq site-run-file nil)

(custom-set-variables
 ;; Turn off mouse interface early in startup to avoid momentary display.
 '(menu-bar-mode 1)            ; I'm working in menu addition
 '(tool-bar-mode nil)          ; Disable the tool bar
 '(scroll-bar-mode nil)        ; Disable the scroll bar
 '(tooltip-mode nil)           ; Disable the tooltips

 '(confirm-kill-processes nil) ; Prevent annoying query when quit Emacs
 `(custom-file ,(concat user-etc-dir "custom.el")))

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

;; Set up load path.
(add-to-list 'load-path user-host-dir)
(add-to-list 'load-path user-site-lisp-dir)
(add-to-list 'load-path user-defuns-dir)

;; Add external projects to load path.
(dolist (pkg (directory-files user-site-lisp-dir t "\\w+"))
  (when (file-directory-p pkg)
    (add-to-list 'load-path pkg)))

;; Add private packages to load path.
(dolist (pkg (directory-files user-private-dir t "\\w+"))
  (when (file-directory-p pkg)
    (add-to-list 'load-path pkg)))

(provide 'prelude)
;;; prelude.el ends here
