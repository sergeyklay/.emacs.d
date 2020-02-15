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

;; https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-util.el
(defun my/add-to-hooks (func hooks)
  "Add FUNC to HOOKS."
  (dolist (hook hooks)
    (add-hook hook func)))

;; https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-util.el
(defun my/add-to-hook (hook funs)
  "Add list of FUNS to HOOK."
  (dolist (fun funs)
    (add-hook hook fun)))

;; https://github.com/Andersbakken/rtags/issues/987
(defun fontify-string (str mode)
  "Return STR fontified according to MODE."
  (with-temp-buffer
    (insert str)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region
     (point-min) (point-max) nil)
    (buffer-string)))

(provide 'utils)
;;; utils.el ends here
