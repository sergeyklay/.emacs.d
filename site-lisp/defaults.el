;;; defaults.el --- Sane defaults. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Sane defaults for GNU Emacs.

;;; Code:

(require 'prelude)
(require 'backup) ; Backup, auto-saving and history configuration
(require 'directories)

;; C-h h runs the command `view-hello-file'.
;; I never used this feature and actually don't need it.
(global-unset-key (kbd "C-h h"))

(setq default-directory "~/")

(global-set-key (kbd "C-x t d") #'toggle-debug-on-error)

;;;; Encoding

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

(prefer-coding-system 'utf-8)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

(setq locale-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8)

(add-to-list 'auto-coding-alist '("/#[^/]+#\\'" . utf-8))

;;;; Sane defaults

(setq-default
 debug-on-error (and (not noninteractive) emacs-debug-mode)
 vc-follow-symlinks t ; Don't ask for confirmation when opening symlinks

 ;; tramp
 tramp-auto-save-directory    (concat user-cache-dir "tramp/")
 tramp-backup-directory-alist backup-directory-alist
 tramp-persistency-file-name  (concat user-cache-dir "tramp-persistency.el")

 ;; nsm
 nsm-settings-file            (concat user-etc-dir "network-security.data")

 ;; url
 url-cache-directory          (concat user-cache-dir "url/")
 url-configuration-directory  (concat user-etc-dir "url/"))

;; Enable disabled by default commands permanently
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Open URLs with xdg-open
(setq browse-url-browser-function 'browse-url-xdg-open)

;; I use C source to understand and debug built-in functions
(let ((src "$HOME/src/emacs"))
  (when (or (file-directory-p src)
	    (file-symlink-p src))
  (setq source-directory
	(expand-file-name (substitute-in-file-name src)))))

;;;; Session direcory

(eval-after-load 'x-win
  (let ((session-dir(concat user-cache-dir "session/")))
    `(progn
       (make-directory ,session-dir t)
       (defun emacs-session-filename (session-id)
	 "Construct a filename to save the session in based on SESSION-ID.
This function overrides the one on `x-win' to use my personal directory."
	 (expand-file-name session-id ,session-dir)))))

;;;; Tutorial

(advice-add
 'tutorial--saved-dir
 :override (lambda ()
             (let ((tutorial-dir (concat user-cache-dir "tutorial/")))
               (unless (file-exists-p tutorial-dir)
                 (make-directory tutorial-dir t))
               tutorial-dir)))

;;;; Emacs Server

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'defaults)
;;; defaults.el ends here
