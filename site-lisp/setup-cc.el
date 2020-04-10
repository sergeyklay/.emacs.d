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

(require 'setup-tags)

(eval-when-compile
  (require 'compile)
  (require 'company)

  (declare-function ggtags-eldoc-function "ggtags.el" ()))

(use-package company-c-headers
  :after company
  :defer t
  :init
  (add-to-list 'company-backends 'company-c-headers)
  :hook
  ((c-mode-common . company-mode)))

(defun my/custom-compile-command ()
  "Custom compile command to use for C buffers."
  (interactive)
  (setq-local compilation-read-command nil)
  (call-interactively 'compile))

(defun c-binds-hook ()
  "Setup keybindings to use for C buffers."
  (local-set-key (kbd "<f5>") #'my/custom-compile-command))

(use-package cc-mode
  :ensure nil
  :hook ((c-mode . c-binds-hook)
	 (c-mode-common . ggtags-common-hook)))

(provide 'setup-cc)
;;; setup-cc.el ends here
