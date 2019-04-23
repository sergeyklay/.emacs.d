;;; grammars.el --- Language grammars. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support of language grammars for GNU Emacs.

;;; Code:

(use-package bnf-mode
  :mode "\\.bnf\\'"
  :init
  (setq bnf-mode-algol-comments-style t))

(use-package lemon-mode
  :mode "\\.y\\'"
  :mode "\\.lemon\\'")

(provide 'grammars)
;;; grammars.el ends here
