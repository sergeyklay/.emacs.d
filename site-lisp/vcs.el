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

(use-package gitconfig-mode
  :mode "\\.gitconfig\\'"
  :mode "\\.git/config\\'"
  :mode "\\.gitmodules\\'")

(use-package gitignore-mode
  :mode "\\.gitignore\\'"
  :mode "\\.dockerignore\\'"
  :mode "\\..elpaignore\\'"
  :preface
  (defun gitignore-common-hook()
    "The common hook for the `gitignore-mode'."
    (set (make-local-variable 'require-final-newline) t))
  :hook
  ((gitignore-mode . gitignore-common-hook)))

(use-package gitattributes-mode
  :mode "\\.gitattributes\\'"
  :mode "\\.git/info/attributes\\'")

;;;; `git-gutter'

(use-package git-gutter
  :defer 2
  :diminish git-gutter-mode
  :custom
  (git-gutter:update-interval 2)
  (git-gutter:modified-sign "┃")
  (git-gutter:added-sign "┃")
  (git-gutter:deleted-sign "┃")
  (git-gutter:hide-gutter nil)
  :config
  (global-git-gutter-mode +1)
  (set-face-foreground 'git-gutter:modified "DeepSkyBlue3")
  (set-face-foreground 'git-gutter:added "SeaGreen4")
  (set-face-foreground 'git-gutter:deleted "IndianRed3"))

(provide 'vcs)
;;; vcs.el ends here
