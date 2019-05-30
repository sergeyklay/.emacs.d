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

(use-package restclient
  :commands
  (restclient-mode)
  :hook
  (restclient-mode . company-mode))

(provide 'http-tools)
;;; http-tools.el ends here
