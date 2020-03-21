;;; langs-zephir.el --- Zephir related configuration. -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020 Serghei Iakovlev <egrep@protonmail.ch>

;; Author: Serghei Iakovlev <egrep@protonmail.ch>
;; URL: https://github.com/sergeyklay/.emacs.d
;;
;; This file is NOT part of GNU Emacs.
;;
;; License: GPLv3

;;; Commentary:

;; Zephir programming language support for GNU Emacs.

;;; Code:

(defun zephir-common-hook ()
  "The common hook to configure `zephir-mode'."
  ;; These modes are not ready to use with `zephir-mode'.
  (company-statistics-mode -1)
  (company-mode -1)
  (flycheck-mode -1)
  (eldoc-mode -1)

  ;; These options are common.
  (setq-local fill-column 120)
  (setq-local indent-tabs-mode nil))

(use-package zephir-mode
  :straight
  (zephir-mode :type git
	       :host github
	       :repo "zephir-lang/zephir-mode")
  :hook ((zephir-mode . subword-mode)
	 (zephir-mode . yas-minor-mode)
	 (zephir-mode . auto-fill-mode)
	 (zephir-mode . zephir-common-hook)
	 (zephir-mode . zephir-abbrev-hook)))

(provide 'langs-zephir)
;;; langs-zephir.el ends here
