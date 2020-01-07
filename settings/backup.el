;;; backup.el --- Backup configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Backup, auto-saving and history configuration for GNU Emacs.

;;; Code:

(require 'directories)

;;; Backup

;; Delete excess backup versions silently.
(setq delete-old-versions t)

;; Make numeric backup versions unconditionally.
(setq version-control t)

;; Make backup files even in version controlled directories.
(setq vc-make-backup-files t)

;; Keep all backup in one directory.
(let ((my-backup-dir (concat user-cache-dir "backup/")))
  (setq backup-directory-alist
        `(("." . ,(file-name-as-directory my-backup-dir))))
  (unless (file-exists-p my-backup-dir)
    (make-directory my-backup-dir t)))

;;; Auto-Saving

(let ((my-auto-save-dir (concat user-cache-dir "autosave/")))
  (setq
   auto-save-file-name-transforms
   `((".*" ,(expand-file-name "\\2" my-auto-save-dir) t))

   auto-save-list-file-name
   (concat my-auto-save-dir
            (format ".saves-%d-%s~" (emacs-pid) (system-name))))

  (unless (file-exists-p my-auto-save-dir)
    (make-directory my-auto-save-dir t)))

(setq auto-save-default t
      auto-save-timeout 10
      auto-save-interval 200)

;;; History

(setq-default
 history-length 500
 history-delete-duplicates t
 savehist-file (concat user-cache-dir "minibuffer-history.el"))

(savehist-mode 1)

(provide 'backup)
;;; backup.el ends here
