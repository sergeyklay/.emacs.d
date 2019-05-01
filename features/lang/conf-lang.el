;;; conf-lang.el --- Configuration like languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for the configuration like languages for GNU Emacs.

;;; Code:

;;;; JSON

(use-package json-mode
 :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)\\'")

;;;; Vim

(use-package vimrc-mode
  :mode "/\\.?g?vimrc\\'"
  :mode "\\.vim\\'"
  :mode "\\.?vimperatorrc\\'"
  :mode "\\.vimp\\'")

;;;; Yaml

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;;;; Autoconf

(add-to-list 'auto-mode-alist
             '("/config\\.\\(ac\\|in\\|m4\\)\\'" . autoconf-mode))

(use-package nginx-mode
  :mode (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
         ("nginx.conf" . nginx-mode)))

(use-package company-nginx
  :after (company nginx-mode)
  :hook
  ((nginx-mode . company-mode)
   (nginx-mode . company-nginx-keywords)))

(provide 'conf-lang)
;;; conf-lang.el ends here
