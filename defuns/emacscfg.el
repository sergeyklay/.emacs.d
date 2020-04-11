;;; emacscfg.el --- Simple way to configure Emacs projects. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Simple way to configure Emacs projects.

;;; Code:

(require 'cl-lib)
(require 'projectile)

(defconst ecfg-config-file ".emacscfg"
  "The configuration file name being used to configure current project.")

(defun ecfg--create-config ()
  "Create an empty config."
  (make-hash-table :test 'equal))

(defun ecfg--read-from-file (filename)
  "Read data from FILENAME."
  (let (data)
    (with-temp-buffer
      (insert-file-contents filename)
      (cl-assert (eq (point) (point-min)))
      (setq data (read (current-buffer)))
      (unless (hash-table-p data)
        (setq data (ecfg--create-config))))
    data))

(defun ecfg--print-to-file (filename data)
  "Write a DATA to the FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun ecfg-read-project-config ()
  "Read the project configuration."
  (let ((path (concat (projectile-project-root) ecfg-config-file)))
    (if (file-exists-p path)
        (ecfg--read-from-file path)
      (ecfg--create-config))))

(defun ecfg-save-project-config (data)
  "Save the project configuration to file.
DATA should represent a valid hashtable object."
  (let ((path (concat (projectile-project-root) ecfg-config-file)))
    (when (hash-table-p data)
      (ecfg--print-to-file path data))))

(provide 'emacscfg)
;;; emacscfg.el ends here
