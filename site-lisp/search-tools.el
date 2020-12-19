;;; search-tools.el --- Setup search tools for Emacs. -*- lexical-binding: t; -*-

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

;; Setup search tools for Emacs.

;;; Code:

(defun my/find-fd-command (&optional exclude)
  "Fd command to get files in a project.
When optional EXCLUDE list is given 'fd' will use it as a list
to exclude files and directories."
  (concat "fd . --color=never --type f -0 -H"
	  (unless (null exclude)
            (mapconcat #'identity exclude " -E "))))

(defun my/find-rg-command (&optional exclude)
  "Rg command to get files in a project.
When optional EXCLUDE list is given 'rg' will use it as a list
to exclude files and directories."
  (let ((rg-cmd "rg -0 --files --color=never --hidden")
        (to-exclude ""))
    (unless (null exclude)
      (dolist (file-or-dir exclude)
        (setq to-exclude (format "%s --glob '!%s'" to-exclude file-or-dir))))
    (concat rg-cmd to-exclude)))

(defun my/find-ag-command (&optional exclude)
  "Ag command to get files in a project.
When optional EXCLUDE list is given 'ag' will use it as a list
to exclude files and directories."
  (concat "ag -0 -l --nocolor --hidden"
	  (unless (null exclude)
            (mapconcat #'identity exclude " --ignore="))))

(provide 'search-tools)
;;; search-tools.el ends here
