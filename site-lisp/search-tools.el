;;; search-tools.el --- Setup search tools for Emacs. -*- lexical-binding: t; -*-

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

;; Setup search tools for Emacs.

;;; Code:

(defconst my/rg-base-command
  "rg --ignore-case --no-heading --line-number --color=never"
  "Base 'rg' command.")

(defconst my/globally-ignored-directories
  '("__pycache__" "elpa" ".cache" "node_modules"
    "bower_components"  ".idea" ".vscode"
    ".ensime_cache" ".eunit" ".git" ".hg"
    ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox"
    ".svn" ".stack-work" ".ccls-cache" ".cache"
    ".clangd")
  "Globally excluded directories.")

(defun my/find-fd-command (&optional exclude)
  "Fd command to get files in a project.
When optional EXCLUDE list is given 'fd' will use it as a list
to exclude files and directories."
  (concat "fd . --color=never --type f -0 -H"
	  (unless (null exclude)
            (mapconcat #'identity exclude " -E "))))

(defun my/format-rg-exclude (exclude)
  "Format EXCLUDE list for 'rg' command."
  (let ((retval ""))
    (dolist (file-or-dir exclude)
      (setq retval (format "%s --glob '!%s'" retval file-or-dir)))
    retval))

(defun my/find-rg-command (&optional exclude)
  "Rg command to get files in a project.
When optional EXCLUDE list is given 'rg' will use it as a list
to exclude files and directories."
  (let ((rg-cmd "rg -0 --files --color=never --hidden")
        (to-exclude ""))
    (unless (null exclude)
      (setq to-exclude (my/format-rg-exclude exclude)))
    (concat rg-cmd to-exclude)))

(provide 'search-tools)
;;; search-tools.el ends here
