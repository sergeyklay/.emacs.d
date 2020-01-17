;;; grammars.el --- Language grammars. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support of language grammars for GNU Emacs.

(use-package bnf-mode
  :defer t
  :mode "\\.bnf\\'")

(use-package lemon-mode
  :defer t
  :mode "\\.y\\'"
  :mode "\\.lemon\\'")

;;; Code:

(provide 'grammars)
;;; grammars.el ends here
