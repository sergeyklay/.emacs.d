;; passive-voice-check.el --- Check for passive voice in documentation. -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Serghei Iakovlev <egrep@protonmail.ch>

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

;; Check the specified files for passive voice usage using writegood-mode.
;; The test fails if passive voice is detected in any of the files.

;;; Code:

(package-initialize)

(require 'writegood-mode)

(defun check-passive-voice-in-file (file)
  "Check for passive voice in FILE using writegood-mode.
Return t if passive voice is found, nil otherwise."
  (with-temp-buffer
    (insert-file-contents file)
    (writegood-mode 1)
    (let ((passive-found nil))
      (font-lock-fontify-buffer)  ; Force font-lock to apply highlighting
      (goto-char (point-min))
      (while
          (and (not passive-found)
               (re-search-forward
                (writegood-passive-voice-font-lock-keywords-regexp) nil t))
        (setq passive-found t))
      passive-found)))

(let* ((config-directory (expand-file-name "~/.emacs.d/"))
       (files-to-check (list (concat config-directory "README.org"))))
  (dolist (file files-to-check)
    (when (check-passive-voice-in-file file)
      (error "Passive voice detected in file: %s" file)))
  (message "No passive voice detected in the checked files."))

(provide 'passive-voice-check)

;; Local Variables:
;; fill-column: 80
;; eval: (outline-minor-mode)
;; eval: (display-fill-column-indicator-mode)
;; coding: utf-8-unix
;; End:

;;; passive-voice-check.el ends here
