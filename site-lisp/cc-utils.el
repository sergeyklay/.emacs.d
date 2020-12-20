;;; cc-utils.el --- Utils to use for C/C++ modes. -*- lexical-binding: t; -*-

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

;; Utils to use for C/C++ modes.

;;; Code:

(defconst cc-standard-include-dirs-cmd
  (concat
   "echo | cpp -x c++ -Wp,-v 2>&1 | "
   "ag --nocolor '^[ \t]*/([^/ ]+(/)?)+$' | sed 's/^ //g'")
  "Command used to retrieve the standard C/C++ include directories.")

(defun cc-get-standard-include-dirs ()
  "Retrieve the standard C/C++ include directories."
  (mapcar (lambda (include-path)
	    (concat "-I" include-path))
	  (split-string
	   (shell-command-to-string cc-standard-include-dirs-cmd) "\n" t)))

(provide 'cc-utils)
;;; cc-utils.el ends here
