;;; http-tools.el --- Various http tools. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;;; Code:

(require 'core-defuns)
(require 'org-lang)
(require 'comp-any)

(use-package restclient
  :requires org-lang
  :after org
  :commands
  (restclient-mode)
  :config
  (add-to-list 'org-babel-load-languages '(restclient . t)))

(use-package company-restclient
  :requires comp-any
  :after (restclient company)
  :defer t
  :init
  (add-company-backends!!
    :backends company-restclient
    :modes restclient-mode))

(provide 'http-tools)
;;; http-tools.el ends here
