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

(use-package projectile
  :diminish projectile-mode
  :custom
  ;; The command-line option ‘-batch’ causes Emacs to run `noninteractively'.
  (projectile-enable-caching (not noninteractive))
  (projectile-cache-file (concat user-cache-dir "projectile.cache"))
  ;; The alien indexing method optimizes to the limit the speed
  ;; of the hybrid indexing method.
  (projectile-indexing-method 'alien)
  ;; Projectile will consider the current directory the project root.
  (projectile-require-project-root nil)
  (projectile-known-projects-file
   (concat user-cache-dir "projectile-bookmarks.eld"))
  :hook (projectile-after-switch-project . setup-tags-fronted)
  :config
  (projectile-mode 1)
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
  ;; Use the faster searcher to handle project files:
  ;; - user-friendly alternative of find `fd'
  ;; - the silver searcher `ag'
  (let ((command
	 (cond
	  ((executable-find "fd")
	   (concat "fd . --color=never --type f -0 -H "
		   (mapconcat #'identity
			      (cons "" projectile-globally-ignored-directories)
			      " -E ")))
	  ((executable-find "ag")
	   (concat "ag -0 -l --nocolor --hidden"
		   (mapconcat #'identity
			      (cons "" projectile-globally-ignored-directories)
			      " --ignore-dir="))))))
    (setq projectile-generic-command command)
    ;; See URL `https://github.com/bbatsov/projectile/issues/1148'
    (setq projectile-git-command command)))

(provide 'projects)
;;; projects.el ends here
