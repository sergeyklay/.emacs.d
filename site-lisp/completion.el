;;; completion.el --- Setup completion. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Initialize company.

;;; Code:

(eval-when-compile
  (require 'directories))

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

;; Sort company candidates by statistics.
(use-package company-statistics
  :after company
  :hook
  (company-mode . company-statistics-mode)
  :custom
  (company-statistics-file
   (concat user-cache-dir "company-statistics-cache.el"))
  :config
  (advice-add 'company-statistics--load :around #'suppress-messages))

(provide 'completion)
;;; completion.el ends here
