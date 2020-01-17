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
  :delight '(:eval (concat " " (projectile-project-name)))
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

;;;; Counsel Projectile

(use-package counsel-projectile
  :after ivy
  :defer nil
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
