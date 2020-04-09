;;; setup-cedet.el --- Setup CEDET. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Setup CEDET.

;;; Code:

(require 'directories)
(require 'cc-utils)

(require 'xref)
(require 'semantic/ia)
(require 'semantic/dep)
(require 'semantic/db-mode)
(require 'semantic/db-global)
(require 'semantic/idle)
(require 'semantic/util-modes)

(defconst ede-custom-file (concat user-etc-dir "cc-mode-projects.el")
  "The default EDE project configuration file.")

(use-package ede
  :ensure nil
  :defer t
  :custom
  (ede-project-placeholder-cache-file
   (concat user-cache-dir "ede-projects.el"))
  :config
  ;; In EDE project configuration file, user can configure a project in this
  ;; form:
  ;;
  ;; (ede-cpp-root-project "project_name"
  ;;                       :file "dir/to/project/project_root/Makefile"
  ;;                       :include-path '("user_include1"
  ;;                                       "user_include2")
  ;;                       :system-include-path '("sys_include1"
  ;;                                              "sys_include2"))
  ;;
  ;; `:include-path' specifies local directories of the project relative to the
  ;; project root specified by `:file' that EDE should search
  ;; first. `:system-include-path' specifies system header files path that are
  ;; not belong to the project.
  (when (file-exists-p ede-custom-file)
    (load ede-custom-file))

  (global-ede-mode 1))

(defun semantic--setup-default-submodules ()
  "Setup `semantic' submodules."
  (let ((submodules
	 '(global-semantic-idle-scheduler-mode  ; Reparse buffer when idle.
	   global-semantic-idle-summary-mode    ; Show summary of tag at point.
	   global-semanticdb-minor-mode         ; Maintain tag database.
	   global-semantic-mru-bookmark-mode))) ; Keybinding for tag names.
    (while submodules
      (add-to-list 'semantic-default-submodes (car submodules))
      (setq submodules (cdr submodules)))))

(defun semantic--setup-directories ()
  "Setup `semantic' directories."
  (when (boundp 'semanticdb-default-save-directory)
    (unless (file-exists-p semanticdb-default-save-directory)
      (make-directory semanticdb-default-save-directory t))))

(defun semantic--enable ()
  "Enable `semantic'."
  (message "Enable Semantic...")
  (global-semanticdb-minor-mode 1)
  (global-semantic-idle-scheduler-mode 1)
  (global-semantic-stickyfunc-mode 1)
  (semantic-mode 1)

  (when (executable-find "global")
    (message "Enable the use of the GNU GLOBAL SemanticDB...")
    (semanticdb-enable-gnu-global-databases 'c-mode)
    (semanticdb-enable-gnu-global-databases 'c++-mode)))

(defun semantic-include-hook ()
  "Modifies a mode-local version of `semantic-dependency-system-include-path'."
  (let ((include-dirs (cc-get-standard-include-dirs)))
     (mapc (lambda (dir)
     	    ;; Trim ‘-I’ from beginning.
    	    (let ((ldir (substring dir 2)))
     	      (progn
     		(semantic-add-system-include ldir 'c++-mode)
     		(semantic-add-system-include ldir 'c-mode))))
     	  include-dirs)))

(defun semantic-cc-hook ()
  "Setup `semantic' keybindings to use for C/C++ buggers."

  (local-set-key "\C-c\C-j" #'semantic-ia-fast-jump)
  (local-set-key "\C-c\C-s" #'semantic-ia-show-summary))

(use-package semantic
  :ensure nil
  :custom
  (semanticdb-default-save-directory (concat user-cache-dir "semanticdb/"))
  :config
  (semantic--setup-default-submodules)
  (semantic--setup-directories)
  (semantic--enable)
  (advice-add 'semantic-ia-fast-jump :before
              #'(lambda (_)
                  "Push marker for jump back."
                  (xref-push-marker-stack))))

(add-hook 'semantic-init-hook #'semantic-include-hook)
(add-hook 'c-mode-common-hook #'semantic-cc-hook)

(provide 'setup-cedet)
;;; setup-cedet.el ends here
