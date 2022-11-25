;;; complit.el --- Code completion support. -*- lexical-binding: t; -*-

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

;; Setting up intelligent code completion.

;;; Code:

(eval-when-compile
  (require 'directories))

;;;; Company

(use-package company
  :defer t
  :diminish company-mode
  :custom
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 10)
  (company-selection-wrap-around t)
  (company-show-numbers t)
  (company-minimum-prefix-length 2)
  (company-global-modes
   '(not comint-mode
	 erc-mode
	 minibuffer-inactive-mode
	 shell-mode))
  ;; Setting up default backends for company
  (company-backends
   '((company-files      ; files & direcories
      company-keywords   ; keywords
      company-capf)      ; completion-at-point-functions
     (company-abbrev     ; abbreviations
      company-dabbrev))) ; dynamic abbreviations
  :bind
  ("<C-tab>" . company-complete)
  (:map company-active-map
        ("M-n" . nil)
	("M-p" . nil)
	("C-d" . company-show-doc-buffer)
	("C-n" . company-select-next)
	("C-p" . company-select-previous)
	("SPC" . company-abort)
	("<backtab>" . company-select-previous)))

;; Some packages are too noisy.
;; See URL `http://superuser.com/a/1025827'.
(defun my/suppress-messages (func &rest args)
  "Suppress message output when call FUNC with remaining ARGS."
  (cl-flet ((silence (&rest args1) (ignore)))
    (advice-add 'message :around #'silence)
    (unwind-protect (apply func args)
      (advice-remove 'message #'silence))))

;; Sort company candidates by statistics.
(use-package company-statistics
  :after company
  :hook
  (company-mode . company-statistics-mode)
  :custom
  (company-statistics-file
   (concat user-cache-dir "company-statistics-cache.el"))
  :config
  (advice-add 'company-statistics--load :around #'my/suppress-messages))

(provide 'complit)
;;; complit.el ends here
