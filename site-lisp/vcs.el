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
(require 'hooks)

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
  :mode "\\.gitmodules\\'"
  :hook
  ((gitignore-mode . my|final-newline-hook)))

(use-package gitignore-mode
  :mode "\\.gitignore\\'"
  :mode "\\.dockerignore\\'"
  :mode "\\..elpaignore\\'"
  :hook
  ((gitignore-mode . my|final-newline-hook)))

(use-package gitattributes-mode
  :mode "\\.gitattributes\\'"
  :mode "\\.git/info/attributes\\'"
  :hook
  ((gitignore-mode . my|final-newline-hook)))

;;;; `diff-hl'

(use-package diff-hl
  :hook
  ((dired-mode . diff-hl-dired-mode)
   (after-init . global-diff-hl-mode)
   (vc-dir-mode . turn-on-diff-hl-mode)
   (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (if (display-graphic-p)
      (diff-hl-flydiff-mode t)
    (diff-hl-margin-mode t)))

(provide 'vcs)
;;; vcs.el ends here
