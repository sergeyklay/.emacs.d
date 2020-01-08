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

(use-package gitconfig-mode
  :mode "\\.gitconfig\\'"
  :mode "\\.git/config\\'"
  :mode "\\.gitmodules\\'")

(use-package gitignore-mode
  :mode "\\.gitignore\\'"
  :mode "\\.dockerignore\\'"
  :mode "\\..elpaignore\\'")

(use-package gitattributes-mode
  :mode "\\.gitattributes\\'"
  :mode "\\.git/info/attributes\\'")

(provide 'vcs)
;;; vcs.el ends here
