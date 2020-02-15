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

;;;; Flycheck

(declare-function
 flycheck-display-error-messages-unless-error-list
 "flycheck" (errors))

;; Flycheck is a general syntax highlighting framework which
;; other packages hook into.  It's an improvment on the built
;; in flymake.
(use-package flycheck
  :unless (equal system-type 'windows-nt)
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-standard-error-navigation nil)
  (flycheck-emacs-lisp-load-path 'inherit)
  ;; Remove ‘new-line’ checks, since they would trigger an immediate
  ;; check when we want the idle-change-delay to be effect while
  ;; editing.
  (flycheck-check-syntax-automatically
   '(save idle-change mode-enabled))
  ;; See URL https://github.com/flycheck/flycheck/issues/302
  (flycheck-display-errors-function
   #'flycheck-display-error-messages-unless-error-list)
  :hook
  ((after-init . global-flycheck-mode)
   (flycheck-after-syntax-check . my|adjust-flycheck-eagerness))
  ;;
  :config
  (defun my|adjust-flycheck-eagerness ()
    "Adjust how often we check for errors based on if there are any.

This lets us fix any errors as quickly as possible, but in a clean
buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
	  (if flycheck-current-errors 0.5 3.0)))
  ;; Each buffer gets its own idle-change-delay because of the
  ;; buffer-sensitive adjustment above.
  (make-variable-buffer-local 'flycheck-idle-change-delay))

(provide 'syntax-check)
;;; syntax-check.el ends here
