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

(defconst ecfg-use-truename t
  "Non-nil means always expand filenames using function `file-truename'.")

(defconst ecfg-workspace-name ".ecfg"
  "Workspace directory name being used to configure current project.")

(defun ecfg--workspace-root ()
  "Resolve workspace root path based on currently opened buffer.

Tries to resolve workspace root by lookup either for the
`ecfg-config-dir' directiry, or the '.projectile' file, or the
'.git' directory."
  (let ((file-name (buffer-file-name))
        (workspace (when (fboundp 'projectile-project-root)
                     (projectile-project-root))))
    (unless workspace
      ;; Get the current working directory using `buffer-file-name'
      ;; or `default-directory'
      (if file-name
          (setq workspace (file-name-directory file-name))
        (setq workspace (expand-file-name default-directory)))

      ;; Expand real path of the obtained working directory (if enabled)
      (when ecfg-use-truename
        (setq workspace (file-truename workspace)))

      ;; Scan for the real workspace path of the opened file.
      ;; We're looking either for the `ecfg-config-dir' directiry,
      ;; or the '.projectile' file, or the '.git' directory.
      (let (last-dir)
        (while
            (not (or (file-directory-p (concat workspace ecfg-workspace-name))
                     (file-exists-p (concat workspace ".projectile"))
                     (file-directory-p (concat workspace ".git"))
                     (string= workspace "/")))
          (setq last-dir workspace
                workspace (file-name-directory
                           (directory-file-name workspace)))
          (when (string= last-dir workspace)
            (setq workspace "/")))))

    (when (or (string= workspace "/") (equal workspace ""))
      (message "ECFG: Unable to resolve workspace root path")
      (setq workspace nil))

    ;; Return resolved workspace root path
    workspace))

(defun ecfg--storage-path (project-root)
  "Return absolute path to directory where ECFG configuration should be located.

Uses PROJECT-ROOT as a path to the directory, where ECFG configuration should be
located.  If this directory does not exist, tries to create it."
  (message "Lookup for the storage directory...")
  (let (workspace-dir)
    (cl-assert (not (null project-root))
               t "project root is unknown")
    (setq workspace-dir (concat project-root ecfg-workspace-name))
    (unless (f-exists? workspace-dir)
      (mkdir workspace-dir t))
    (f-full workspace-dir)))

(defun ecfg--create-workspace (project-root force)
  "Create a workspace located at PROJECT-ROOT taking into account FORCE flag."
  (let (workspace-dir)
    (if project-root
        (setq workspace-dir (ecfg--storage-path project-root))
      (message
       "Unable to create workspace configuration: project root is unknown"))))

(defun ecfg-init-workspace ()
  "Initialize ECFG workspace.

ECFG workspace is usually just your project root folder.  Using
\\[universal-argument] will force to re-initialize the the current workspace
even if it already known."
  (interactive)
  (ecfg--create-workspace (ecfg--workspace-root)
                          (not (null current-prefix-arg))))

;; (defun ecfg--create-config ()
;;   "Create an empty config."
;;   (make-hash-table :test 'equal))

;; (defun ecfg--read-from-file (filename)
;;   "Read data from FILENAME."
;;   (let (data)
;;     (with-temp-buffer
;;       (insert-file-contents filename)
;;       (cl-assert (eq (point) (point-min)))
;;       (setq data (read (current-buffer)))
;;       (unless (hash-table-p data)
;;         (setq data (ecfg--create-config))))
;;     data))

;; (defun ecfg--print-to-file (filename data)
;;   "Write a DATA to the FILENAME."
;;   (with-temp-file filename
;;     (prin1 data (current-buffer))))

;; (defun ecfg-read-project-config ()
;;   "Read the project configuration."
;;   (let ((path (concat (projectile-project-root) ecfg-config-file)))
;;     (if (file-exists-p path)
;;         (ecfg--read-from-file path)
;;       (ecfg--create-config))))

;; (defun ecfg-save-project-config (data)
;;   "Save the project configuration to file.
;; DATA should represent a valid hashtable object."
;;   (let ((path (concat (projectile-project-root) ecfg-config-file)))
;;     (when (hash-table-p data)
;;       (ecfg--print-to-file path data))))

(provide 'emacscfg)
;;; emacscfg.el ends here
