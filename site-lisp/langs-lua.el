;;; langs-lua.el --- Lua support for GNU Emacs. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Lua support for GNU Emacs.

;;; Code:

(use-package lua-mode
  :defer t
  :mode  ("\\.lua\\'" . lua-mode)
  :interpreter ("lua" . lua-mode))

(with-eval-after-load 'company
  (add-hook 'lua-mode-hook 'company-mode))

(provide 'langs-lua)
;;; langs-lua.el ends here
