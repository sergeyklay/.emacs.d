;;; hooks.el --- Common hooks used in GNU Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Common hooks used in GNU Emacs.

;;; Code:

(defun my|final-newline-hook()
  "Add final newline when the file is about to be saved."
  (set (make-local-variable 'require-final-newline) t))

(provide 'hooks)
;;; hooks.el ends here
