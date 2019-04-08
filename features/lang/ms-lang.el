;;; ms-lang.el --- Add support for M$ scripts. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for M$ scripts

;;; Code:

;;; PowerShell

(use-package powershell
  :mode "\\.ps[dm]?1\\'"
  :commands (powershell-mode powershell))

(provide 'ms-lang)
;;; ms-lang.el ends here
