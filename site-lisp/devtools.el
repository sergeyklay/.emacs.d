;;; devtools.el --- Various devtools. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Various devtools to profile and debug Emacs.

;;; Code:

;;;; Emacs Startup Profiler

(use-package esup
  :commands (esup))

(provide 'devtools)
;;; devtools.el ends here
