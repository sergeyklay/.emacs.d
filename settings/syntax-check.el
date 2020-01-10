;;; syntax-check.el --- Syntax checkers. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Syntax checkers for GNU Emacs.

;;; Code:

;;;; Flycheckf


;; Flycheck is a general syntax highlighting framework which
;; other packages hook into.  It's an improvment on the built
;; in flymake.

(use-package flycheck
  :preface
  ;; See URL `https://github.com/flycheck/flycheck/issues/302'
  (defun unless-error-buffer (errors)
    (unless (get-buffer-window flycheck-error-list-buffer)
      (when (fboundp 'flycheck-display-error-messages)
	(flycheck-display-error-messages errors))))

  (global-flycheck-mode 1)
  :config
  (setq flycheck-indication-mode 'right-fringe
	flycheck-standard-error-navigation nil
	flycheck-display-errors-function #'unless-error-buffer
	flycheck-emacs-lisp-load-path 'inherit))

(provide 'syntax-check)
;;; syntax-check.el ends here
