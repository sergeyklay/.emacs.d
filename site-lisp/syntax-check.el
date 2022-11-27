;;; syntax-check.el --- Syntax checkers. -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020, 2021, 2022 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d

;; This file is NOT part of Emacs.

;;;; License

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

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
  :config
  (defun my|adjust-flycheck-eagerness ()
    "Adjust how often we check for errors based on if there are any.

This lets us fix any errors as quickly as possible, but in a clean
buffer we're an order of magnitude laxer about checking."
    (setq flycheck-idle-change-delay
	  (if flycheck-current-errors 0.5 3.0)))
  ;; 2022/11/27: commented because I'm not sure if it's still needed
  ;;
  ;; ;; Each buffer gets its own idle-change-delay because of the
  ;; ;; buffer-sensitive adjustment above.
  ;; (make-variable-buffer-local 'flycheck-idle-change-delay)
  )

(provide 'syntax-check)
;;; syntax-check.el ends here
