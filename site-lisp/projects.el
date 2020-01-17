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
  :custom
  ;; The command-line option ‘-batch’ causes Emacs to run `noninteractively'.
  (projectile-enable-caching (not noninteractive))
  (projectile-cache-file (concat user-cache-dir "projectile.cache"))
  ;; The alien indexing method optimizes to the limit the speed
  ;; of the hybrid indexing method.
  (projectile-indexing-method 'alien)
  ;; Projectile will consider the current directory the project root.
  (projectile-require-project-root nil)
  ;; Ignores
  (projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS"))
  (projectile-globally-ignored-file-suffixes
   '(".elc" ".pyc" ".o" ".lo" ".la" ".out" ".sock"))
  (projectile-completion-system 'ivy)
  (projectile-known-projects-file
   (concat user-cache-dir "projectile-bookmarks.eld")))

;; See URL `https://github.com/bbatsov/projectile/issues/1148'
(eval-after-load 'projectile
  '(progn
     (let ((fd-binary (executable-find "fd")))
       (when fd-binary
	 (setq projectile-git-command
	       (concat fd-binary " . --color=never --type f -0 -H -E .git")
	       projectile-generic-command projectile-git-command)))))

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
