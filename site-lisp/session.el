;;; session.el --- Setting for bookmarks, recentf, etc. -*- lexical-binding: t; -*-

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

;; Setting for bookmarks, recentf, etc.

;;; Code:

(eval-when-compile
  (require 'directories))

;;;; Bookmark

(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file
   (concat user-etc-dir "bookmarks.el")))

;;;; Recentf

(use-package recentf
  :ensure nil
  :defer 1
  :custom
  (recentf-max-saved-items 500)
  (recentf-save-file (concat user-cache-dir "recentf"))
  ;; I'll clean recent failes manually
  (recentf-auto-cleanup 'never)
  (recentf-keep '(recentf-keep-default-predicate
		  file-remote-p file-readable-p))
  (recentf-exclude
   `(,(rx "/" (or "COMMIT_EDITMSG" "NOTES_EDITMSG"
		  "PULLREQ_EDITMSG" "MERGEREQ_EDITMSG"
		  "TAG_EDITMSG" "BRANCH_DESCRIPTION"
		  "EDIT_DESCRIPTION"))
     ,(expand-file-name package-user-dir)
     ,(expand-file-name recentf-save-file)
     ,(rx (or "TAGS" "GPATH" "GRTAGS" "GTAGS") eol)
     ,(rx (or "." "/") "cache")
     ,(rx bol (1+ (not (or "/" ":"))) ":")
     ,(rx "." (or "gz" "gpg" "gif") eol)
     ,(rx bol (? "/var") "/tmp")
     "auto-save-list/" "elpa/" ".cask" "/dev/.*"))
  :hook (find-file . my|common-recentf-hook)
  :config
  (require 'cl-macs) ; cl-flet

  (defun my|common-recentf-hook ()
  "Common hook for function `recentf-mode'."
  (unless recentf-mode
    (recentf-mode)
    (when (fboundp 'recentf-track-opened-file)
      (recentf-track-opened-file))))

  (defvar recentf-list-prev nil)

  (defadvice recentf-save-list
      (around no-message activate)
    "If `recentf-list' and previous recentf-list are equal,
do nothing. And suppress the output from `message' and
`write-file' to minibuffer."
    (unless (equal recentf-list recentf-list-prev)
      (cl-flet ((message (format-string &rest args)
                      (eval `(format ,format-string ,@args)))
             (write-file (file &optional confirm)
                         (let ((str (buffer-string)))
                           (with-temp-file file
                             (insert str)))))
        ad-do-it
        (setq recentf-list-prev recentf-list)))))

(provide 'session)
;;; session.el ends here
