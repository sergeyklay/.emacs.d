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

(defun ecfg--read-from-file (filename)
  "Read data from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

(defun ecfg--print-to-file (filename data)
  "Write DATA for FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun ecfg-read-project-config ()
  "Read project configuration."
  (let ((path (concat (projectile-project-root) ".emacscfg")))
    (if (file-exists-p path)
        (ecfg--read-from-file path)
      (make-hash-table :test 'equal))))

(defun ecfg-save-project-config (config)
  "Save project CONFIG."
  (let ((path (concat (projectile-project-root) ".emacscfg")))
    (ecfg--print-to-file path config)))

(provide 'emacscfg)
;;; emacscfg.el ends here
