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

(provide 'utils)
;;; utils.el ends here
