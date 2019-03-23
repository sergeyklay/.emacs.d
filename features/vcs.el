;;; vcs.el --- Emacs configuration for VCS. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev

;; Author: Serghei Iakovlev (concat "sadhooklay" "@" "gmail" ".com")
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; VCS related features for GNU Emacs.

;;; Code:

;;; Magit

;; A great interface for git projects.  It's much more pleasant to use
;; than the git interface on the command line.  Use an easy keybinding
;; to access magit.  For more see URL `https://magit.vc'

(use-package magit
  :ensure t
  :bind (("C-x g s" . magit-status)
         ("C-x g x" . magit-checkout)
         ("C-x g c" . magit-commit)
         ("C-x g p" . magit-push)
         ("C-x g u" . magit-pull)
         ("C-x g e" . magit-ediff-resolve)
         ("C-x g r" . magit-rebase-interactive))
  :config
  ;; DWIM prompting when creating new branches.
  (setq magit-branch-read-upstream-first 'fallback))

;;; Git

(use-package git-link
  :ensure t)

(use-package gitconfig-mode
  :ensure t
  :mode (("\\.gitconfig\\'" . gitconfig-mode)
	 ("\\.git/config\\'" . gitconfig-mode)
	 ("\\.gitmodules\\'" . gitconfig-mode)))

(use-package gitignore-mode
  :ensure t
  :mode (("\\.gitignore\\'" . gitignore-mode)
         ("\\.dockerignore\\'" . gitignore-mode)))

(use-package gitattributes-mode
  :ensure t
  :mode (("\\.gitattributes\\'" . gitattributes-mode)
         ("\\.git/info/attributes\\'" . gitattributes-mode)))

(provide 'vcs)
;;; vcs.el ends here
