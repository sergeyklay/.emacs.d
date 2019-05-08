;;; news.el --- News/Email related features. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; News/Email related features for GNU Emacs.

;;; Code:

(use-package bbdb
  :pin melpa)

(use-package counsel-bbdb
  :after (bbdb counsel))

(provide 'news)
;;; news.el ends here
