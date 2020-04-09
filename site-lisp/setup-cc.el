;;; setup-cc.el --- Support for the C-family of languages. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Support for the C-family of languages.

;;; Code:

(require 'cc-utils)

(eval-when-compile
  (require 'company))

(defun company-c-headers-setup-hook ()
  "Add `company-c-headers' to `company-mode'."
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-c-headers
  :after company
  :defer t
  :init
  (add-to-list 'company-backends 'company-c-headers)
  :hook
  ((c-mode-common . company-mode)))

;; Displays function signatures in the mode line.
(use-package c-eldoc
  :defer 2
  :config
  (setq c-eldoc-buffer-regenerate-time 60
	c-eldoc-includes (append '("-I./" "-I../")
				 (cc-get-standard-include-dirs))))

(add-hook 'c-mode-common-hook
	  #'(lambda () (c-turn-on-eldoc-mode)))

(provide 'setup-cc)
;;; setup-cc.el ends here
