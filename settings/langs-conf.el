;;; langs-conf.el --- Configuration like languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Add support for the configuration like languages for GNU Emacs.

;;; Code:

;;;; JSON

(use-package json-mode
  :mode "\\.js\\(on\\|[hl]int\\(rc\\)?\\)\\'"
  :config
  (bind-keys :map json-mode-map
	     ("C-c <tab>" . json-mode-beautify)))

;;;; Ini

(use-package ini-mode
  :mode "\\.ini\\'")

;;;; Dotenv

(use-package dotenv-mode
  :mode "\\.env\\'"
  :mode "\\.env\\..*\\'")

;;;; Vim

(use-package vimrc-mode
  :mode "/\\.?g?vimrc\\'"
  :mode "\\.vim\\'"
  :mode "\\.?vimperatorrc\\'"
  :mode "\\.vimp\\'")

;;;; Yaml

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :interpreter ("yml" . yml-mode))

;;;; Autoconf

(add-to-list 'auto-mode-alist
             '("/config\\.\\(ac\\|in\\|m4\\)\\'" . autoconf-mode))

;;;; Nginx

(use-package nginx-mode
  :mode (("/nginx/sites-\\(?:available\\|enabled\\)/" . nginx-mode)
         ("nginx.conf" . nginx-mode)))

(use-package company-nginx
  :after (company nginx-mode)
  :hook
  ((nginx-mode . company-mode)
   (nginx-mode . company-nginx-keywords)))

(provide 'langs-conf)
;;; langs-conf.el ends here
