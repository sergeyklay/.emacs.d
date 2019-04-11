;;; projects.el --- Project management related features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Project navigation and management library for Gnu Emacs.

;;; Code:

(use-package projectile
  :after ivy
  :delight '(:eval (concat " " (projectile-project-name)))
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
  :bind (("C-c p p"   . projectile-switch-project))
  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories "elpa")
  (add-hook 'dired-before-readin-hook #'projectile-track-known-projects-find-file-hook))

(use-package counsel-projectile
  :after (ivy counsel projectile)
  :config
  (counsel-projectile-mode 1)
  (ivy-set-display-transformer #'counsel-projectile-find-file nil))

(provide 'projects)
;;; projects.el ends here

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
