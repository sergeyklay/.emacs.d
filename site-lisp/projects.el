;;; projects.el --- Project management related features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Project navigation and management library for GNU Emacs.

;;; Code:

(require 'directories)

;;;; Ivy

(use-package projectile
  :after ivy
  :diminish projectile-mode
  :init
  ;; Ignore directories
  (setq projectile-globally-ignored-directories
	(append '("elpa" ".cache" "node_modules" "bower_components")
		projectile-globally-ignored-directories))
  ;; Ignore files
  (setq projectile-globally-ignored-files
	(append '(".DS_Store" "Icon" "GRTAGS" "GTAGS" "GPATH")
		projectile-globally-ignored-files))
  ;; Ignore suffixes
  (setq projectile-globally-ignored-file-suffixes
	(append '(".elc" ".pyc" ".o" ".lo" ".la" ".out" ".sock" ".zwc")
		projectile-globally-ignored-file-suffixes ))
  :custom
  ;; The command-line option ‘-batch’ causes Emacs to run `noninteractively'.
  (projectile-enable-caching (not noninteractive))
  (projectile-cache-file (concat user-cache-dir "projectile.cache"))
  ;; The alien indexing method optimizes to the limit the speed
  ;; of the hybrid indexing method.
  (projectile-indexing-method 'alien)
  ;; Projectile will consider the current directory the project root.
  (projectile-require-project-root nil)
  (projectile-completion-system 'ivy)
  (projectile-known-projects-file
   (concat user-cache-dir "projectile-bookmarks.eld"))
  :config
  ;; Use the faster searcher to handle project files:
  ;; - user-friendly alternative of find `fd'
  ;; - ripgrep `rg'
  ;; - the platinum searcher `pt'
  ;; - the silver searcher `ag'
  (let ((command
	 (cond
	  ((executable-find "fd")
	   (concat "fd . --color=never --type f -0 -H "
		   (mapconcat #'identity
			      (cons "" projectile-globally-ignored-directories)
			      " -E ")))
	  ((executable-find "rg")
	   (let ((rg-cmd ""))
	     (dolist (dir projectile-globally-ignored-directories)
	       (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
	     (concat "rg -0 --files --color=never --hidden" rg-cmd)))
	  ((executable-find "ag")
	   (concat "ag -0 -l --nocolor --hidden"
		   (mapconcat #'identity
			      (cons "" projectile-globally-ignored-directories)
			      " --ignore-dir=")))
	  ((executable-find "pt")
	   (concat "pt -0 -l --nocolor --hidden ."
		   (mapconcat #'identity
			      (cons "" projectile-globally-ignored-directories)
			      " --ignore="))))))
    (setq projectile-generic-command command)
    ;; See URL `https://github.com/bbatsov/projectile/issues/1148'
    (setq projectile-git-command command)))

;;;; Counsel Projectile

(use-package counsel-projectile
  :after ivy
  :defer nil
  :custom
  ;; Removing the current project or buffer from the list of candidates
  (counsel-projectile-remove-current-project t)
  (counsel-projectile-remove-current-buffer t)
  :config
  (add-to-list 'ivy-initial-inputs-alist
	       '(counsel-projectile-switch-project . ""))
  (counsel-projectile-mode t)
  :bind (("M-s b" . counsel-projectile-switch-to-buffer)
	 ("M-s d" . counsel-projectile-find-dir)
	 ("M-s f" . counsel-projectile-find-file)
	 ([f12]   . (lambda ()
		      (interactive)
		      (counsel-projectile-switch-project 2)))))

(provide 'projects)
;;; projects.el ends here
