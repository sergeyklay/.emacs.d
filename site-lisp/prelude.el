;;; prelude.el --- Begin initialization. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Serghei Iakovlev <egrep@protonmail.ch>

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

;; Begin initialization.

;;; Code:

(require 'directories (concat user-emacs-directory "site-lisp/directories"))

(defvar file-name-handler-alist-backup
  file-name-handler-alist
  "The backup of `file-name-handler-alist' we'll restore on `after-init-hook'.")

;; Every file opened and loaded by Emacs will run through this list to check for
;; a proper handler for the file, but during startup, it won’t need any of them.
(setq file-name-handler-alist nil)

;; Reset `gc-cons-threshold' to the standard value after initializing the Emacs
;; session.  Also we restore `file-name-handler-alist' here.
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

(add-hook 'after-init-hook #'reset-performance)

;; One less file to load at startup
(setq site-run-file nil)

;; Actually this project is my personal configuration
;; so I use GNU Emacs 27.1 now.
(eval-when-compile
  (and (version< emacs-version "27.1")
       (error
        (concat
         "Detected Emacs %s. "
         "This configuration is designed to work "
         "only with Emacs 27.1 and higher. I'm sorry.")
        emacs-version)))

(defconst emacs-debug-mode (or (getenv "DEBUG") init-file-debug)
  "If non-nil, all Emacs will be verbose.
Set DEBUG=1 in the command line or use --debug-init to enable this.")

;; Set up load path.
(add-to-list 'load-path user-host-dir)
(add-to-list 'load-path user-site-lisp-dir)

;; Add external projects to load path.
(dolist (pkg (directory-files user-site-lisp-dir t "\\w+"))
  (when (file-directory-p pkg)
    (add-to-list 'load-path pkg)))

;; Add private packages to load path.
(dolist (pkg (directory-files user-private-dir t "\\w+"))
  (when (file-directory-p pkg)
    (add-to-list 'load-path pkg)))

;; Measure the current start up time.
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(provide 'prelude)
;;; prelude.el ends here
