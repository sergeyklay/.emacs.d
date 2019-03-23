;;; pm.el --- Project management related features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Project navigation and management library for Gnu Emacs.

;;; Code:

(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-cache-file (concat user-cache-dir "projectile.cache")
        projectile-known-projects-file (concat user-cache-dir "projectile-bookmarks.eld")
        ;; The command-line option ‘-batch’ causes Emacs to run `noninteractively'.
        projectile-enable-caching (not noninteractive)
        ;; The alien indexing method optimizes to the limit the speed
        ;; of the hybrid indexing method.
        projectile-indexing-method 'alien
        ;; Projectile will consider the current directory the project root.
        projectile-require-project-root nil
        ;; Ignores
        projectile-globally-ignored-files '(".DS_Store" "Icon" "TAGS")
        projectile-globally-ignored-file-suffixes
        '(".elc" ".pyc" ".o" ".lo" ".la" ".out" ".sock"))

  :bind (("C-c p p"   . projectile-switch-project)
         ("C-c p s s" . projectile-ag))
  :config
  (projectile-mode t)
  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook))

(provide 'pm)
;;; pm.el ends here
