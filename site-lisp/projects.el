;;; projects.el --- Project management related features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021, 2022 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d

;; This file is NOT part of Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Project navigation and management library for GNU Emacs.

;;; Code:

(eval-when-compile
  (require 'directories)
  (require 'search-tools))

;;;; Projectile

(use-package projectile
  :after ivy
  :custom
  ;; The static part of the lighter
  (projectile-mode-line-prefix " Pr")
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
  (projectile-globally-ignored-directories
   my/globally-ignored-directories)
  ;; Command used by projectile to get the files in a git project
  ;; See URL https://github.com/bbatsov/projectile/issues/1148
  (projectile-git-command
   (concat "comm -23 "
           "<(git ls-files -co --exclude-standard | sort) "
           "<(git ls-files -d | sort) "
           "| tr '\n' '\0'"))
  :config
  (projectile-mode 1)
  ;; Ignore files
  (setq projectile-globally-ignored-files
	(append '(".DS_Store" "Icon" "GRTAGS" "GTAGS" "GPATH")
		projectile-globally-ignored-files))
  ;; Ignore suffixes
  (setq projectile-globally-ignored-file-suffixes
	(append '(".elc" ".pyc" ".o" ".lo" ".la" ".out" ".sock" ".zwc")
		projectile-globally-ignored-file-suffixes ))
    ;; Command used by projectile to get the files in a generic project
  (setq projectile-generic-command
   (my/find-fd-command (cons "" projectile-globally-ignored-directories))))

;;;; Counsel Projectile

(use-package counsel-projectile
  :after ivy
  :custom
  ;; Removing the current project or buffer from the list of candidates
  (counsel-projectile-remove-current-project t)
  (counsel-projectile-remove-current-buffer t)
  :config
  (add-to-list 'ivy-initial-inputs-alist
	       '(counsel-projectile-switch-project . ""))
  (counsel-projectile-mode t)
  :bind (("C-c p b" . counsel-projectile-switch-to-buffer)
	 ("C-c p d" . counsel-projectile-find-dir)
	 ("C-c p f" . counsel-projectile-find-file)
	 ("C-c p p" . counsel-projectile-switch-project)
         ("C-c p r" . counsel-projectile-rg)
         ("C-c p a" . counsel-projectile-ag)
         ("C-c p g" . counsel-projectile-grep)))

(provide 'projects)
;;; projects.el ends here
