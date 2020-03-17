;;; bookmarks.el --- Setting for bookmarks, recentf, etc. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setting for bookmarks, recentf, etc.

;;; Code:

(eval-when-compile
  (require 'utils)
  (require 'directories))

(use-package recentf
  :ensure nil
  :defer 1
  :custom
  (recentf-max-saved-items 200)
  (recentf-save-file (concat user-cache-dir "recentf"))
  (recentf-auto-cleanup 10)
  :hook (find-file . my|common-recentf-hook)
  :config
  (require 'cl-macs) ; cl-flet

  (defun my|common-recentf-hook ()
  "Common hook for function `recentf-mode'."
  (unless recentf-mode
    (recentf-mode)
    (when (fboundp 'recentf-track-opened-file)
      (recentf-track-opened-file))))

  (defvar recentf-list-prev nil)

  (defadvice recentf-save-list
      (around no-message activate)
    "If `recentf-list' and previous recentf-list are equal,
do nothing. And suppress the output from `message' and
`write-file' to minibuffer."
    (unless (equal recentf-list recentf-list-prev)
      (cl-flet ((message (format-string &rest args)
                      (eval `(format ,format-string ,@args)))
             (write-file (file &optional confirm)
                         (let ((str (buffer-string)))
                           (with-temp-file file
                             (insert str)))))
        ad-do-it
        (setq recentf-list-prev recentf-list))))

  (setq recentf-exclude
        `(,(concat "/\\(\\(\\"
		  "(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)"
		  "_EDIT\\|MERGE_\\|\\)MSG\\|\\(BRANCH\\|EDIT\\)"
		  "_DESCRIPTION\\)\\'")
	  ,(expand-file-name package-user-dir)
	  "github.*txt$" "auto-save-list\\*"
	  ".cache" "[/\\]elpa/" ".cask" "bookmarks"
	  "/dev/.*"))

  ;; Suppress "Cleaning up the recentf...done (0 removed)"
  (advice-add 'recentf-cleanup :around #'suppress-messages)
  (run-with-idle-timer 30 t 'recentf-save-list))

(defun my/undo-kill-buffer (arg)
  "Re-open the last buffer killed.
With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
	 (buffer-files-list
	  (delq nil (mapcar (lambda (buf)
			      (when (buffer-file-name buf)
				(expand-file-name (buffer-file-name buf))))
			    (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
	     (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))

(provide 'bookmarks)
;;; bookmarks.el ends here
