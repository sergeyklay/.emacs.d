;;; autoconf-lang.el --- Add support for the m4-family of languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for the m4-family of languages for GUN Emacs.

;;; Code:

(require 'sh-autoconf)  ; Autoconf flavour for sh-mode

(add-to-list 'auto-mode-alist
             '("/config\\(ure\\)?\\.\\(ac\\|in\\|m4\\)\\'" . sh-mode))

(add-to-list 'auto-mode-alist
             '("/ac\\(include\\|local\\)\\.m4\\'" . sh-mode))

(provide 'autoconf-lang)
;;; autoconf-lang.el ends here
