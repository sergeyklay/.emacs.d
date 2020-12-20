;;; init.el --- VCS related features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Serghei Iakovlev <egrep@protonmail.ch>

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

;; VCS related features for GNU Emacs.

;;; Code:

(require 'directories)

(defun my|final-newline-hook()
  "Add final newline when the file is about to be saved."
  (set (make-local-variable 'require-final-newline) t))

;;;; Magit

(use-package transient
  :custom
  (transient-history-file (concat user-cache-dir "transient/history.el"))
  (transient-values-file (concat user-cache-dir "transient/values.el"))
  (transient-levels-file (concat user-cache-dir "transient/levels.el")))

(use-package magit
  :after transient
  :bind (("C-x g" . magit-status)))

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
