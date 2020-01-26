;;; utils.el --- Various utulity functions. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Various utulity functions.

;;; Code:

;; from https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-util.el
(defun my/add-to-hooks (func hooks)
  "Add FUNC to HOOKS."
  (dolist (hook hooks)
    (add-hook hook func)))

;; from https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-util.el
(defun my/add-to-hook (hook funs)
  "Add list of FUNS to HOOK."
  (dolist (fun funs)
    (add-hook hook fun)))

(defun my/ruby-locate-executable ()
  "Search for the Ruby executable using ’rbenv’.

This function will try to find the Ruby executable by calling
’rbenv’.  If it is not available, the function will utilize
`executable-find'.  The function will return full path to the actual
Ruby if found or nil otherwise."
  (let ((rbenv (executable-find "rbenv"))
	(ruby-path ""))
    (when rbenv
      (setq ruby-path
	    (replace-regexp-in-string
	     "\n\\'" ""
	     (shell-command-to-string
	      (concat rbenv " which ruby 2>/dev/null")))))
    (if (= (length ruby-path) 0)
	(executable-find "ruby")
      ruby-path)))

(provide 'utils)
;;; utils.el ends here
