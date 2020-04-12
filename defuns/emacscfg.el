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

(defconst ecfg-project-root-dir-use-truename t
  "Non-nil means always expand filenames using function `file-truename'.")

(defconst ecfg-config-file ".ecfg.el"
  "The configuration file name being used to configure current project.")

(defun ecfg--get-project-root ()
  "Get the project root directory of the curent opened buffer."
  (let ((file-name (buffer-file-name))
        (project-root (when (fboundp 'projectile-project-root)
                        (projectile-project-root))))
    (unless project-root
      ;; Get the current working directory using `buffer-file-name'
      ;; or `default-directory'
      (if file-name
          (setq project-root (file-name-directory file-name))
        (setq project-root (expand-file-name default-directory)))

      ;; Expand real path of the obtained working directory (if enabled)
      (when ecfg-project-root-dir-use-truename
        (setq project-root (file-truename project-root)))

      ;; Scan for the real project root of the opened file.
      ;; We're looking either for the `ecfg-config-file' file,
      ;; or the '.projectile' file, or the '.git' directory.
      (let (last-dir)
        (while
            (not (or (file-exists-p (concat project-root ecfg-config-file))
                     (file-exists-p (concat project-root ".projectile"))
                     (file-directory-p (concat project-root ".git"))
                     (string= project-root "/")))
          (setq last-dir project-root
                project-root (file-name-directory
                              (directory-file-name project-root)))
          (when (string= last-dir project-root)
            (setq project-root "/")))))

    (when (or (string= project-root "/") (equal project-root ""))
      (message "ECFG: Unable to resolve project root")
      (setq project-root nil))

    ;; Return resolved project root
    project-root))

(defun ecfg--init-project (project-root force)
  "Init configuration located at PROJECT-ROOT taking into account FORCE flag."
  )

(defun ecfg-init ()
  "Initialize ECFG configuration."
  (interactive)
  (ecfg--init-project (ecfg--get-project-root) t))

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
