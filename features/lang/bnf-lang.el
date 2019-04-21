;;; bnf-lang.el --- BNF grammars. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for BNF grammars for GNU Emacs.

;;; Code:

(use-package bnf-mode
  :mode "\\.bnf\\'")

(provide 'bnf-lang)
;;; bnf-lang.el ends here
