;;; init.el --- VCS related features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; VCS related features for GNU Emacs.

;;; Code:

(require 'directories)

;;;; Magit

;; A great interface for git projects.  It's much more pleasant to use
;; than the git interface on the command line.  Use an easy keybinding
;; to access magit.  For more see URL `https://magit.vc'

(use-package transient
  :config
  (setq transient-history-file (concat user-cache-dir "transient/history.el")
        transient-values-file (concat user-cache-dir "transient/values.el")
        transient-levels-file (concat user-cache-dir "transient/levels.el")))

(use-package magit
  :after (ivy transient)
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-completing-read-function #'ivy-completing-read))

;;;; Git

(defun my|git-common-hook()
  "The common hook for git-related modes."
  (set (make-local-variable 'require-final-newline) t))

(use-package gitconfig-mode
  :mode "\\.gitconfig\\'"
  :mode "\\.git/config\\'"
  :mode "\\.gitmodules\\'"
  :hook
  ((gitignore-mode . my|git-common-hook)))

(use-package gitignore-mode
  :mode "\\.gitignore\\'"
  :mode "\\.dockerignore\\'"
  :mode "\\..elpaignore\\'"
  :hook
  ((gitignore-mode . my|git-common-hook)))

(use-package gitattributes-mode
  :mode "\\.gitattributes\\'"
  :mode "\\.git/info/attributes\\'"
  :hook
  ((gitignore-mode . my|git-common-hook)))

;;;; `git-gutter'

(use-package git-gutter
  :defer 2
  :diminish git-gutter-mode
  :custom
  (git-gutter:update-interval 2)
  (git-gutter:hide-gutter nil)
  :config
  (global-git-gutter-mode +1)
  (set-face-background 'git-gutter:modified "#95B2CE")
  (set-face-foreground 'git-gutter:modified "#95B2CE")
  (set-face-background 'git-gutter:added "#A0C495")
  (set-face-foreground 'git-gutter:added "#A0C495")
  (set-face-background 'git-gutter:deleted "#9F9F9F")
  (set-face-foreground 'git-gutter:deleted "#9F9F9F"))

(provide 'vcs)
;;; vcs.el ends here
