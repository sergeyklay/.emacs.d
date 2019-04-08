;;; syntax-check.el --- Syntax checkers. -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Serghei Iakovlev <sadhooklay@gmail.com>

;; Author: Serghei Iakovlev <sadhooklay@gmail.com>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is not part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Syntax checkers

;;; Code:

;;; Flycheckf

;; Flycheck is a general syntax highlighting framework which
;; other packages hook into.  It's an improvment on the built
;; in flymake.

(use-package flycheck
  :delight
  :init
  (progn
    (global-flycheck-mode)

    ;; See URL `https://github.com/flycheck/flycheck/issues/302'
    (defun unless-error-buffer (errors)
      (unless (get-buffer-window flycheck-error-list-buffer)
        (flycheck-display-error-messages errors)))

    (setq flycheck-indication-mode 'right-fringe
        flycheck-standard-error-navigation nil
        flycheck-display-errors-function #'unless-error-buffer)))

(provide 'syntax-check)
;;; syntax-check.el ends here
