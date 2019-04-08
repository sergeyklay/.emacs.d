;;; core-defuns.el --- Core defuns. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>
;; Copyright (c) 2019 Sylvain Benner <sylvain.benner@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Core defuns for GNU Emacs

;;; Code:

;; from https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-util.el#L38
(defun my/add-to-hooks (func hooks)
  "Add FUNC to HOOKS."
  (dolist (hook hooks)
    (add-hook hook func)))

(defun my/mplist-get-values (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))

;; from https://github.com/syl20bnr/spacemacs/blob/develop/core/core-funcs.el
(defun my/mplist-get-value (plist prop)
  "Get a single value associated to PROP in PLIST, a modified plist.

You should always use this function instead of builtin `plist-get'
in Spacemacs.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (car (my/mplist-get-values plist prop)))

(provide 'core-defuns)
;;; core-defuns.el ends here
