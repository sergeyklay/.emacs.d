;;; defaults.el --- Sane defaults. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021, 2022 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d

;; This file is NOT part of Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Sane defaults for GNU Emacs.

;;; Code:

(require 'prelude)
(require 'backup)
(require 'directories)

(setq default-directory (concat (getenv "HOME") "/"))

(global-set-key (kbd "C-x t d") #'toggle-debug-on-error)

;; Garbage Collector Magic Hack
(use-package gcmh
  :diminish
  :hook (emacs-startup . gcmh-mode)
  :custom
  ;; Idle time to wait in seconds before triggering GC.
  (gcmh-idle-delay 'auto)
  ;; Factor to compute the idle delay when in idle-delay auto mode.
  (gcmh-auto-idle-delay-factor 10)
  ;; High cons GC threshold.
  (gcmh-high-cons-threshold #x1000000))

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

(custom-set-variables
 '(confirm-kill-processes nil) ; Prevent annoying query when quit Emacs
 '(initial-scratch-message "") ; No scratch message
 '(inhibit-startup-screen t)   ; Disable start-up screen
 `(custom-file ,(concat user-etc-dir "custom.el")))

;; Configure the Scratch Buffer's Mode
(setq initial-major-mode 'text-mode)

;; Open URLs with xdg-open;  TODO: macOs
(setq browse-url-browser-function 'browse-url-xdg-open)

;; I use C source to understand and debug built-in functions
(let ((src "$HOME/src/emacs"))
  (when (or (file-directory-p src)
	    (file-symlink-p src))
  (setq source-directory
	(expand-file-name (substitute-in-file-name src)))))

;;;; Session directory

(defun my--emacs-session-filename (session-id)
  "Construct a filename to save the session in based on SESSION-ID.
This function overrides the one on `x-win' to use my personal directory."
  (let ((session-dir (concat user-cache-dir "session/")))
    (unless (file-exists-p session-dir)
      (make-directory session-dir t))
    (expand-file-name session-id session-dir)))

(advice-add
 'emacs-session-filename
 :override #'my--emacs-session-filename)

;;;; Tutorial

(advice-add
 'tutorial--saved-dir
 :override (lambda ()
             (let ((tutorial-dir (concat user-cache-dir "tutorial/")))
               (unless (file-exists-p tutorial-dir)
                 (make-directory tutorial-dir t))
               tutorial-dir)))

;;;; Emacs Server

(declare-function server-running-p "server") ; Why!? :(
(add-hook 'after-init-hook
	  #'(lambda ()
	      (require 'server)
	      (unless (server-running-p)
		(server-start))))

(provide 'defaults)
;;; defaults.el ends here
