;;; ecfg.el --- Simple way to configure Emacs projects. -*- lexical-binding: t; -*-

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

(require 'pp)
(require 'cl-lib)

;;;; Customization

(defconst ecfg-workspace-name ".ecfg"
  "Workspace directory name being used to configure current project.")

(defconst ecfg-config-name "settings.el"
  "Configuration file name being used to configure current project.")

(defvar ecfg-config-cache nil
  "Holds in-memory database for per-project configuration.")

(defconst ecfg-config-template
  (list :version "1.0.0"
        :tags-frontend nil
        :cc (list :include-path nil))
  "ECFG configuration template.")

(defconst ecfg-footer-template
  (concat "\n"
          ";; Local Variables:\n"
          ";; flycheck-disabled-checkers: (emacs-lisp-checkdoc emacs-lisp)\n"
          ";; End:\n")
  "ECFG footer template.")

;;;; Utils

(defun ecfg-read-from-file (filename)
  "Read a project configuration from FILENAME file."
  (let (data)
    (if (file-readable-p filename)
        (with-current-buffer (get-buffer-create " *ECFG Project Configuration*")
          (delete-region (point-min) (point-max))
          (insert-file-contents filename)
          (goto-char (point-min))
          (setq data (with-demoted-errors "Error reading ECFG file: %S"
                       (car (read-from-string
                             (buffer-substring (point-min) (point-max))))))
          (kill-buffer (current-buffer)))
      (setq data ecfg-config-template))
    data))

(defun ecfg-write-data (data path)
  "Write out object DATA into located at PATH."
  (let ((coding-system-for-write 'utf-8))
    (with-current-buffer (get-buffer-create " *ECFG Project Configuration*")
      (delete-region (point-min) (point-max))
      (insert (format ";;; -*- coding: %s -*-\n\n;; ECFG Configuration File\n\n"
                      (symbol-name coding-system-for-write)))
      (let ((print-length nil)
            (print-level nil))
        (pp data (current-buffer)))
      (insert ecfg-footer-template)
      (condition-case nil
          (write-region (point-min) (point-max) path)
        (file-error (message "ECFG: can't write file %s" path)))
      (kill-buffer (current-buffer)))))

(defun ecfg-workspace-root ()
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
      (setq workspace (file-truename workspace))

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

(defun ecfg-storage-path (project-root)
  "Return absolute path to directory where ECFG configuration should be located.

Uses PROJECT-ROOT as a path to the directory, where ECFG configuration should be
located.  If this directory does not exist, tries to create it."
  (let (workspace-dir)
    (cl-assert (not (null project-root))
               t "project root is unknown")
    (setq workspace-dir (concat project-root ecfg-workspace-name))
    (unless (file-directory-p workspace-dir)
      (mkdir workspace-dir t))
    workspace-dir))

(defun ecfg-config-path (project-root)
  "Return absolute path to workspace confguration.
Uses PROJECT-ROOT as a project root."
  (let (workspace-dir config-path)
    (cl-assert (not (null project-root))
               t "project root is unknown")
    (setq workspace-dir (ecfg-storage-path project-root)
          config-path (expand-file-name ecfg-config-name workspace-dir))
    config-path))

(defun ecfg-create-workspace (project-root force)
  "Create a workspace located at PROJECT-ROOT taking into account FORCE flag."
  (let (config-file)
    (if project-root
        (progn
          (setq config-file (ecfg-config-path project-root))
          ;; Check for file existence and its size
          (when (or (not (file-exists-p config-file))
                    (< (file-attribute-size (file-attributes config-file)) 4)
                    force)
            (ecfg-write-data ecfg-config-template config-file)))
      (message
       "Unable to create workspace configuration: project root is unknown"))))

(defun ecfg-load-config (&optional project-root)
  "Return workspace configuration.

Tries to use in memory cache if possible.  Optional PROJECT-ROOT argument can be
passed to point desired project root working to."
  (let* ((project-root (or project-root (ecfg-workspace-root)))
         (config-file (ecfg-config-path project-root))
         (config-data (nth 1 (assoc-string project-root ecfg-config-cache))))
    (unless config-data
      (when (file-readable-p config-file)
        (setq config-data (ecfg-read-from-file config-file))
        (push (list project-root config-data) ecfg-config-cache)))
    config-data))

(defun ecfg-init-workspace ()
  "Initialize ECFG workspace.
Using \\[universal-argument] will force to re-initialize the the current
workspace even if it already known."
  (interactive)
  (ecfg-create-workspace (ecfg-workspace-root)
                         (not (null current-prefix-arg))))

(defun ecfg-get (key &optional dflt)
  "Look up KEY in project configuration and return its associated value.
If KEY is not found, return DFLT which default to nil."
  (let ((data (ecfg-load-config)) retval)
    (setq retval (plist-get data key))
    (if retval retval dflt)))

(defun ecfg-set (key value)
  "Associate KEY with VALUE in a project configuration."
  (let ((data (ecfg-load-config))
        (project-root (ecfg-workspace-root)))
    (plist-put data key value)
    (setq ecfg-config-cache (assoc-delete-all project-root ecfg-config-cache))
    (push (list project-root data) ecfg-config-cache)))

(defun ecfg-unset (key)
  "Remove the association for KEY from a project configuration, if there is one."
  (ecfg-set key nil))

(provide 'ecfg)
;;; ecfg.el ends here
