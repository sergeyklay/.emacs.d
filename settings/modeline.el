;;; modeline.el --- Modeline related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Modeline related configuration for GNU Emacs.

;;; Code:

;; Activate column and line number in the modeline
(column-number-mode t)
(line-number-mode t)

(use-package delight
  :after use-package)

(provide 'modeline)
;;; modeline.el ends here
